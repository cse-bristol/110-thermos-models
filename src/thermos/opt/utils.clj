;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.utils
  "Utility functions")

(def HOURS-PER-YEAR 8766.0)

(defn assoc-by
  "Like clojure.core/group-by, except produces a map from (f x) => x
  over coll, where if two x have same f the last one wins"
  [f coll]
  (persistent!
   (reduce
    (fn [a x] (assoc! a (f x) x))
    (transient {})
    coll)))

(defn annual-kwh->kw
  ([kwh-pa]
   (/ kwh-pa HOURS-PER-YEAR))
  ([kwh-pa load-factor]
   (if (and (zero? kwh-pa) (zero? load-factor))
     0.0
     (/ kwh-pa (* load-factor HOURS-PER-YEAR)))))

(defn annual-kw->kwh
  ([kw]
   (* kw HOURS-PER-YEAR))
  ([kw load-factor]
   (* kw HOURS-PER-YEAR load-factor)))


(defn linear-evaluate
  "Evaluate `curve`, given as x,y tuples, at a given `x` value.
  The x-values must be in non-decreasing order.
  Out-of-bounds values throw an exception
  At discontinuities the rightmost point in the input is taken.
  "
  [curve x]
  (let [position (java.util.Collections/binarySearch
                  curve [x]
                  #(<= (first %1) (first %2)))
        position (if (neg? position)
                   (- (- position) 1)
                   position)]
    (if (= position (count curve))
      (let [[x' y] (last curve)]
        (if (> x x')
          (throw (ex-info "Curve evaluation out of bounds (above max)"
                          {:curve curve :x x}))
          y))

      (let [[px py] (nth curve position)]
        (cond
          (and (zero? position)
               (< x px))
          (throw (ex-info "Curve evaluation out of bounds (below min)"
                          {:curve curve :x x}))

          (or (zero? position)
              (== x px)) py

          :else
          (let [[px2 py2] (nth curve (dec position))
                m (/ (- py py2) (- px px2))
                fr (- x px2)]
            (+ py2 (* fr m))))))))
