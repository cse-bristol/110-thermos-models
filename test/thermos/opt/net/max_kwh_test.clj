(ns thermos.opt.net.max-kwh-test
  (:require [clojure.test :as t]
            [thermos.opt.test :refer [≈]]
            [thermos.opt.net.core :as net]))

(defn- packing-problem [limit items-to-pack]
  {:objective :max-kwh
   :mip-gap 0
   :pipe-losses {:kwp [30], :w%m [0]}
   :vertices
   (into [{:id "supply"
           :supply {:capacity-kw 10000.0
                    :capacity-kwh limit
                    :cost%kwh 1.0}}]

         (map-indexed
          (fn [i item]
            {:id (str "d" i)
             :demand {:value 0 :kwh item :kwp 1}})
          items-to-pack))
   :edges
   (vec
    (for [i (range (count items-to-pack))]
      {:id (str "e" i)
       :i "supply"
       :j (str "d" i)
       :length 1
       :cost%m 0
       :cost%kwm 0}))})

(defn- packing-solution [limit items-to-pack]
  (let [sol (net/run-model (packing-problem limit items-to-pack))
        vs (net/assoc-by :id (:vertices sol))]
    (def -vs vs)
    (:output-kwh (vs "supply") 0.0)))

(t/deftest max-kwh-test
  (t/testing "When using the max-kwh objective, kwh are what matter"
    ;; this is a series of small packing problems for which we can
    ;; know the best answer - this form is generally solvable anyway
    ;; because unlike in KNAPSACK the item weight = item value.

    ;; the test is not super extensive because the change to the
    ;; objective is a trivial one and should be independent of the
    ;; rest of the formulation.
    
    (t/is (≈ 1000.0
             (packing-solution 1200.0 [500 500 710])
             0.5))

    (t/is (≈ 1200.0
             (packing-solution 1200.0 [500 500 700])
             0.5))

    (t/is (≈ 0.0
             (packing-solution 400.0 [500 500 700])
             0.5))))



