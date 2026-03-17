(ns thermos.opt.net.best-solver
  "Support for using the best available solver

  The main model runs a loop in which it calls on a solver.

  This file provides `solve*` which can be used like gurobi/solve* or scip/solve*.

  `solve*` should only be used within a call to `with-lock` with a valid lockfile.

  Within the scope of `with-lock`, calls to `solve*` will use gurobi if they
  can get the lock, and will hold the lock between calls once they have acquired it.

  We use the lock instead of depending on the gurobi license, because we can't hold
  the license between runs of gurobi_cl.

  The lock is awaited using fcntl FD locks, which should produce approximately
  fair queueing between processes, but that's up to the kernel.  
  "
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [lp.scip :as scip]
            [lp.gurobi :as gurobi]
            [clojure.tools.logging :as log])
  (:import [java.io RandomAccessFile]
           [java.nio.channels FileLock]
           [java.util.concurrent CancellationException]))

(def scip-settings
  {"heuristics/actconsdiving/freq" "20"
   "heuristics/adaptivediving/freq" "3"
   "heuristics/adaptivediving/maxlpiterquot" "0.15"
   "heuristics/bound/freq" "20"
   "heuristics/clique/freq" "20"
   "heuristics/coefdiving/freq" "20"
   "heuristics/completesol/freq" "20"
   "heuristics/conflictdiving/freq" "5"
   "heuristics/conflictdiving/maxlpiterquot" "0.225"
   "heuristics/conflictdiving/maxlpiterofs" "1500"
   "heuristics/crossover/freq" "10"
   "heuristics/crossover/nwaitingnodes" "20"
   "heuristics/crossover/nodesquot" "0.15"
   "heuristics/crossover/minfixingrate" "0.5"
   "heuristics/crossover/dontwaitatroot" "TRUE"
   "heuristics/dins/freq" "-1"
   "heuristics/distributiondiving/freq" "5"
   "heuristics/distributiondiving/maxlpiterquot" "0.075"
   "heuristics/distributiondiving/maxlpiterofs" "1500"
   "heuristics/farkasdiving/freq" "-1"
   "heuristics/farkasdiving/maxlpiterquot" "0.075"
   "heuristics/farkasdiving/maxlpiterofs" "1500"
   "heuristics/feaspump/freq" "10"
   "heuristics/feaspump/maxlpiterquot" "0.015"
   "heuristics/feaspump/maxlpiterofs" "1500"
   "heuristics/fixandinfer/freq" "20"
   "heuristics/fracdiving/freq" "5"
   "heuristics/fracdiving/maxlpiterquot" "0.075"
   "heuristics/fracdiving/maxlpiterofs" "1500"
   "heuristics/gins/freq" "10"
   "heuristics/guideddiving/freq" "5"
   "heuristics/guideddiving/maxlpiterquot" "0.075"
   "heuristics/guideddiving/maxlpiterofs" "1500"
   "heuristics/zeroobj/freq" "20"
   "heuristics/intdiving/freq" "20"
   "heuristics/intshifting/freq" "5"
   "heuristics/linesearchdiving/freq" "5"
   "heuristics/linesearchdiving/maxlpiterquot" "0.075"
   "heuristics/linesearchdiving/maxlpiterofs" "1500"
   "heuristics/localbranching/freq" "-1"
   "heuristics/locks/freq" "20"
   "heuristics/lpface/freq" "8"
   "heuristics/alns/freq" "10"
   "heuristics/alns/trustregion/active" "TRUE"
   "heuristics/alns/nodesofs" "2000"
   "heuristics/alns/nodesquot" "0.2"
   "heuristics/nlpdiving/freq" "5"
   "heuristics/mutation/freq" "20"
   "heuristics/multistart/freq" "20"
   "heuristics/mpec/freq" "25"
   "heuristics/objpscostdiving/freq" "10"
   "heuristics/objpscostdiving/maxlpiterquot" "0.015"
   "heuristics/objpscostdiving/maxlpiterofs" "1500"
   "heuristics/octane/freq" "20"
   "heuristics/ofins/freq" "20"
   "heuristics/padm/freq" "20"
   "heuristics/proximity/freq" "20"
   "heuristics/pscostdiving/freq" "5"
   "heuristics/pscostdiving/maxlpiterquot" "0.075"
   "heuristics/pscostdiving/maxlpiterofs" "1500"
   "heuristics/randrounding/freq" "10"
   "heuristics/rens/freq" "20"
   "heuristics/rens/minfixingrate" "0.3"
   "heuristics/rens/nodesofs" "2000"
   "heuristics/reoptsols/freq" "20"
   "heuristics/repair/freq" "20"
   "heuristics/rins/freq" "10"
   "heuristics/rootsoldiving/freq" "10"
   "heuristics/rootsoldiving/maxlpiterquot" "0.015"
   "heuristics/rootsoldiving/maxlpiterofs" "1500"
   "heuristics/shiftandpropagate/freq" "20"
   "heuristics/shifting/freq" "5"
   "heuristics/trivial/freq" "20"
   "heuristics/trivialnegation/freq" "20"
   "heuristics/trustregion/freq" "-1"
   "heuristics/twoopt/freq" "20"
   "heuristics/undercover/freq" "20"
   "heuristics/vbounds/freq" "20"
   "heuristics/veclendiving/freq" "5"
   "heuristics/veclendiving/maxlpiterquot" "0.075"
   "heuristics/veclendiving/maxlpiterofs" "1500"
   "separating/flowcover/freq" "8"})

(def installed?
  (let [path (System/getenv "PATH")
        dirs (map io/file
                  (string/split path (re-pattern (java.io.File/pathSeparator))))
        has-exe (fn [dir bin]
                  (let [f (io/file dir bin)]
                    (and (.exists f)
                         (.isFile f)
                         (.canExecute f))))
        ]
    (memoize
     (fn [binary]
       (some #(has-exe % binary) dirs)))))

(defn claim-lock
  "Try and claim a lock - returns a promise or a future you can wait on"
  [lockfile]
  (when lockfile
    (try
      (let [raf (RandomAccessFile. lockfile "rw")
            chan (.getChannel raf)]
        (future
          (let [lock (.lock chan)]
            (try
              [lock chan raf]
              (catch InterruptedException _
                (try (.release lock) (catch Exception _))
                (try (.close chan) (catch Exception _))
                (try (.close raf) (catch Exception _)))))))
      (catch Exception _))))

(defn release-lock [lock-future]
  (when lock-future
    (try (future-cancel lock-future)
         (catch Exception e))
    (try (let [[lock chan raf] @lock-future]
           (try (.release lock) (catch Exception e) )
           
           (try (.close chan) (catch Exception e))
           (try (.close raf) (catch Exception e ))
           :released)
         (catch CancellationException e))))

(def ^:dynamic *gurobi-lock* nil)

(defmacro with-lock [lockfile & body]
  `(binding [*gurobi-lock* (and (installed? "gurobi_cl")
                                (claim-lock ~lockfile))]
     (try
       (Thread/sleep 100)
       ~@body
       (finally (release-lock *gurobi-lock*)))))

(defn- fix-feastol [settings has-gurobi]
  ;; this is a bit ugly
  (update settings :feasibility-tolerance
          (if has-gurobi
            {:initial "1e-6" :retry "1e-9"}
            {:initial "1e-3" :retry "1e-6"})))

(defn solve* [lp settings]
  (cond
    (not *gurobi-lock*)
    (scip/solve* lp (fix-feastol
                     (merge scip-settings settings)
                     false))

    (future-done? *gurobi-lock*)
    ;; we have the lock
    (gurobi/solve* lp (fix-feastol settings true))

    :else
    ;; we await the lock
    (let [scip-solution
          (future
            (scip/solve* lp (fix-feastol
                             (merge scip-settings settings)
                             false)))

          lock-waiter (future
                        (let [lock @*gurobi-lock*]
                          (future-cancel scip-solution)))
          ]
      (try
        @scip-solution
        (catch CancellationException e
          ;; we got gurobi now so use that instead
          (log/info "Restart with now-available gurobi")
          (gurobi/solve* lp (fix-feastol settings true)))
        (finally (future-cancel lock-waiter))))))


