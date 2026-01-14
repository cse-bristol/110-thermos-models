;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.money
  "Functions for discounting"
  (:require [clojure.walk :as walk]))

(defprotocol PresentValue
  "Protocol for things which can have a present value; the two functions
  it contains are for getting values at specific years, from year zero
  which is right now."
  
  (nominal-value [this year] "What does this value (or cost , -ve) in `year`")
  (tagged-values [this year] "What are the nominal values by tag in
  `year`; returns a map from tag to value, eg {:capex 10}"))

(deftype Series [type values]
  PresentValue
  (nominal-value [_ year] (get values year 0))
  (tagged-values [t year] {type (nominal-value t year)}))

(deftype Equipment [type capex opex repex lifetime]
  PresentValue
  (nominal-value [e year]
    (cond (zero? year) (- capex)
          (zero? (mod year lifetime)) (- (+ opex repex))
          :else (- opex)))
  (tagged-values [e year]
    (cond (zero? year) {[:capex type] capex}
          (zero? (mod year lifetime)) {[:opex type] (- opex)
                                       [:repex type] (- repex)}
          :else {[:opex type] (- opex)})))

(deftype Sum [values]
  PresentValue
  (nominal-value [s year]
    (reduce + 0 (map #(nominal-value % year) values)))
  
  (tagged-values [s year]
    (reduce (partial merge-with +) {}
            (map #(tagged-values % year) values))))

(deftype Revenue [tag inital-value annual-value]
  PresentValue
  (nominal-value [r year] (if (zero? year) inital-value annual-value))
  (tagged-values [r year] {tag (nominal-value r year)}))

(defn- collect
  "Get all the PresentValues out of obj"
  [obj]
  (let [items (atom nil)]
    (walk/prewalk
     (fn [item]
       (when (satisfies? PresentValue item)
         (swap! items conj item))
       item)
     obj)
    @items))

(defn present-value
  "Take the PV of `x` which implements `PresentValue`
  nil -> nil"
  [discount-rate period x]
  (if (satisfies? PresentValue x)
    (let [discount-rate (double (+ discount-rate 1))]
      (loop [acc 0.0
             yr 0
             rt 1.0]
        (if (= period yr) acc
            (recur
             (+ acc
                (/ (nominal-value x yr) rt))
             (inc yr)
             (* rt discount-rate)))))
    (let [values (collect x)]
      (when (seq values)
        ;; TODO should this be zero when there is nothing in there?
        ;; or nil as distinct from zero.
        (reduce + 0.0 (map (partial present-value discount-rate period)
                           values))))))

(defn future-values [period x]
  (if (satisfies? PresentValue x)
    (for [year (range period)
          [tag value] (tagged-values x year)]
      {:year year
       :tag tag
       :value value})
    
    (let [values (collect x)]
      (when (seq values)
        ;; TODO should this be zero when there is nothing in there?
        ;; or nil as distinct from zero.
        (reduce into [] (map (partial future-values period)
                             values))))))

;; These are older functions, still used in the supply model.

(def ^:dynamic *discount-rate* 0.0)
(def ^:dynamic *accounting-period* 1)

(defn pv-sequence
  ([xs] (pv-sequence xs *discount-rate* *accounting-period*))
  ([xs r p]
   (let [xs (take p xs)]
     (present-value r p (->Series :pv (vec xs))))))

(defn pv-recurring
  ([x] (pv-recurring x *discount-rate* *accounting-period*))
  ([x r p]
   (cond
     (zero? x) 0
     (zero? r) (* p x)
     true      (pv-sequence (repeat x) r p))))

(defn periodic-sequence
  "Generate a periodic sequence of £x every i years (and so on for other xis)"
  [x i & xis]
  (apply map +
         (cycle (conj (repeat (dec i) 0) x))
         (for [[x i] (partition-all 2 xis)]
           (cycle (conj (repeat (dec i) 0) x)))))
