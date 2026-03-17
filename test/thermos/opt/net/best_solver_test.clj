(ns thermos.opt.net.best-solver-test
  "Test the 'best solver' machinery works as intended"
  (:require [thermos.opt.net.best-solver :as sut]
            [clojure.test :as t]
            [clojure.java.io :as io]
            [lp.proc :as proc]
            [lp.gurobi]
            [lp.scip]))

(defn can-lock? [lockfile]
  (zero? (:exit (proc/run ["flock" "--fcntl" "-w" "0" "-x" (.getCanonicalPath (io/as-file lockfile)) "true"]))))

(defn external-lock
  "We can't lock the file ourselves, as file locks are per-JVM. So we
  shell out to flock for it.

  This will only work on unix systems."
  [lockfile]
  (let [lockfile (.getCanonicalPath (io/as-file lockfile))
        future (future
                 (println "Locking" lockfile)
                 (try
                   (println "flock: "
                            (proc/run
                              ["flock" "--fcntl" "-x" lockfile "yes"]))
                   (catch InterruptedException _)
                   (catch Exception e
                     (println "ex" e))
                   (finally
                     (println "Releasing" lockfile)
                     )))]
    (Thread/sleep 100)
    future
    ))

(t/deftest solver-locking-test
  (let [lockfile (java.io.File/createTempFile "test" ".lock")
        wait (atom 100)]
    (with-redefs [sut/installed? (constantly true)
                  lp.gurobi/solve* (constantly :gurobi)
                  
                  lp.scip/solve* (fn [x y]
                                   (Thread/sleep @wait)
                                   :scip)
                  ]
      (binding [*out* *err*]
        (try
          (t/testing "plain run uses gurobi"
            (t/is (= :gurobi
                     (sut/with-lock lockfile
                       (sut/solve* nil nil)))))
          (t/testing "gurobi released after run"
            (t/is (can-lock? lockfile)))
          
          (t/testing "run uses scip if gurobi not there"
            ;; this is like another process claiming it.
            (let [claim (external-lock lockfile)]
              (t/is (= :scip (sut/with-lock lockfile (sut/solve* nil nil))))
              (future-cancel claim)))
          
          (t/testing "run retries if gurobi becomes available"
            (reset! wait 50000) ;; make "scip" take 50 seconds
            ;; pretend another process has the lock
            (let [claim (external-lock lockfile)]
              (sut/with-lock lockfile
                (let [solution (future (sut/solve* nil nil))]
                  ;; pretend other process has released the lock
                  (future-cancel claim)
                  ;; we should have finished with gurobi
                  (t/is (= :gurobi @solution))))))
          (finally (io/delete-file lockfile true)))))))


