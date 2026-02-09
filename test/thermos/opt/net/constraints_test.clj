(ns thermos.opt.net.constraints-test
  "Tests the correctness of the constraints in the :constraints map in
  the problem.

  These are upper/lower bounds on things like cost, kWh, linear heat density etc

  This could be improved a little by reflecting on what constraint
  keys are known in the schema for a problem.

  Works on a single problem, which should be enough, but could be
  generalised.
  "
  (:require [thermos.opt.net.core :as net]
            [thermos.opt.net.bounds :as bounds]
            [clojure.test :as t]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thermos.opt.test :refer [≈]]))

(def problem
  "A randomly generated 25-building problem. The in-place edits below
  are to transform the problem so that it is about in the middle, so
  that upper and lower bounds can push the solution around a little
  rather than just making it infeasible.

  The magic factor 12 was chosen by hand to this end.

  Wrapped in a delay so that loading tests doesn't trigger SCIP to
  run/bounds calculations"
  (delay
    (->
     (edn/read-string
      (slurp (io/resource "thermos/opt/net/medium-problem.edn")))
     
     (as-> x
         (assoc x :vertices
                (map (fn [v]
                       (cond-> v
                         (:demand v)
                         (update :demand
                                 merge
                                 {:off-network false
                                  :required false
                                  :value (* 12 (:value (:demand v)))})))
                     (:vertices x)))
       (assoc x :bounds (bounds/compute-bounds x))))))

(def base-solution
  "The solution to `problem` above, delayed to prevent work at load time."
  (delay (net/run-model @problem)))

(defn- constraint-value
  "Get the value of the given constraint out, for a problem/solution pair."
  [key problem sol]
  (let [connected-vertices (set (map :id (filter :connected (:vertices sol))))
        connected-edges    (set (map (juxt :i :j) (:edges sol)))]
    (case key
      :kwh (reduce + (keep
                      (comp :kwh :demand)
                      (filter (comp connected-vertices :id)
                              (:vertices problem))))
      ;; TODO assumes we are in NPV mode
      :npv (:objective sol)
      :length (reduce + (keep
                         :length
                         (filter (comp connected-edges (juxt :i :j))
                                 (:edges problem))))
      :linear-density (let [kwh (constraint-value :kwh problem sol)
                            l (constraint-value :length problem sol)]
                        (if (and (zero? l) (zero? kwh))
                          (do
                            (println "probably not a great test case for" key)
                            0.0)
                          (/ kwh l)))
      
      :building-count (count connected-vertices)
      :connection-count (reduce + (keep
                                   (comp :count :demand)
                                   (filter (comp connected-vertices :id)
                                           (:vertices problem))))
      (throw (ex-info "Un-tested constraint" {:key key})))))

(defn- test-constraint
  "The main test logic

  - `constraint-key` determines which constraint we're evaluating
  - `up` and `down` are functions that return either a higher or lower value
    these are here because some constraints are integer valued in the spec
    so up is +1 and down is -1, and some are real-valued so we do +/- 10%
  - `lower` and `upper` bools turn the test for lower-bound and upper bound on
    respectively. There are a couple of situations where `lower` cannot be
    tested because there is no feasible solution where the value is any higher
    (e.g. pv)
  "
  [constraint-key up down lower upper]
  (let [problem @problem
        base-solution @base-solution
        base-value (constraint-value constraint-key problem base-solution)
        ]
    (let [inactive-lower-bound (down base-value)
          active-lower-bound (up base-value)
          inactive-upper-bound active-lower-bound
          active-upper-bound   inactive-lower-bound

          inactive-lower-sol   (-> problem
                                   (assoc-in [:constraints constraint-key :min]
                                             inactive-lower-bound)
                                   (net/run-model))

          inactive-upper-sol   (-> problem
                                   (assoc-in [:constraints constraint-key :max]
                                             inactive-upper-bound)
                                   (net/run-model))]
      

      (when-not (= :valid (:state inactive-lower-sol))
        (println constraint-key ">=" inactive-lower-bound "i not solved"))
      (when-not (= :valid (:state inactive-upper-sol))
        (println constraint-key "<=" inactive-upper-bound "i not solved"))
      
      (t/testing "inactive lower bound doesn't affect objective"
        (t/is (≈ (:objective base-solution)
                 (:objective inactive-lower-sol) 1)))
      (t/testing "inactive upper bound doesn't affect objective"
        (t/is (≈ (:objective base-solution)
                 (:objective inactive-upper-sol) 1)))
      
      (when lower
        (let [active-lower-sol   (-> problem
                                     (assoc-in [:constraints constraint-key :min]
                                               active-lower-bound)
                                     (net/run-model))]
          (when-not (= :valid (:state active-lower-sol))
            (println constraint-key ">=" active-lower-bound "a not solved"))
          (t/testing "active lower bound respected"
           (t/is (<= (int active-lower-bound)
                     (int (constraint-value constraint-key problem active-lower-sol)))))))
      
      (when upper
        (let [active-upper-sol (-> problem
                                   (assoc-in [:constraints constraint-key :max]
                                             active-upper-bound)
                                   (net/run-model))]
          (when-not (= :valid (:state active-upper-sol))
            (println constraint-key "<=" active-upper-bound "a not solved"))
          (t/testing "active upper bound respected"
            (t/is (>= (int active-upper-bound)
                      (int (constraint-value constraint-key problem active-upper-sol))))))))))

(t/deftest constraints-test
  (let [up-double #(* % 1.1)
        down-double #(* % 0.9)]
    (t/testing "kWh constraints"
      (test-constraint :kwh up-double down-double true true))

    ;; we don't check the upper bound on this one because it's impossible to improve
    (t/testing "pv constraints"
      (test-constraint :npv up-double down-double false true))
    
    (t/testing "length constraints"
      (test-constraint :length up-double down-double true true))

    ;; we don't check the lower bound on this one because it's impossible to improve
    (t/testing "linear density constraints"
      (test-constraint :linear-density up-double down-double false true)))

  (let [up-int #(+ % 1)
        down-int #(- % 1)]
    (t/testing "building count constraints"
      (test-constraint :building-count up-int down-int true true))
    (t/testing "connection count constraints"
      (test-constraint :connection-count up-int down-int true true))))
