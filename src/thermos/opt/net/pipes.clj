;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.net.pipes
  "All the logic for pipe sizing, that is outside the network model

  The network model deals in delivered power and present cost as a linear function thereof.

  The code here bridges this to flow temperatures etc. and nominal diameters.

  There are a few stages in typical use, which are:

  1. Call `curves` to precompute a table which says for all our different diameters how much power & heat loss we expect.
  2. Call `linear-cost` to get a linear approximation for £/kW for the network model
  3. Given a network model solution in kW, call `solved-principal` and `solved-diameter`
     to get back to costs and diameters.

  At the moment there is a parallel copy of this in thermos-ui which also supports steam as a medium.
  This should be deprecated or moved here later.
  "

  (:require [thermos.opt.utils :refer [assoc-by
                                       linear-evaluate
                                       annual-kwh->kw
                                       annual-kw->kwh]]))

(def heat-capacity "The specific heat of water" 4.18)

(def ^:private density-curve
  "The density of water at different temperatures, as [[temperature, density]];
https://en.wikipedia.org/wiki/Water_%28data_page%29"
  [[0.0   0.9998395]
   [3.984 0.999972]
   [4.0   0.9999720]
   [5.0   0.99996]
   [10.0  0.9997026]
   [15.0  0.9991026]
   [20.0  0.9982071]
   [22.0  0.9977735]
   [25.0  0.9970479]
   [30.0  0.9956502]
   [35.0  0.99403]
   [40.0  0.99221]
   [45.0  0.99022]
   [50.0  0.98804]
   [55.0  0.98570]
   [60.0  0.98321]
   [65.0  0.98056]
   [70.0  0.97778]
   [75.0  0.97486]
   [80.0  0.97180]
   [85.0  0.96862]
   [90.0  0.96531]
   [95.0  0.96189]
   [100.0 0.95835]])

(defn water-density
  "What is the density of water at the given temperature"
  ^double [^double t]
  (* 1000 (linear-evaluate density-curve t)))

(defn heat-loss-w-per-m
  "For a normal hot water pipe, how much heat loss do we expect"
  ^double [^double delta-t ^double diameter]
  (* delta-t
     (if (zero? diameter) 0 (+ (* 0.16807 (Math/log diameter)) 0.85684)))) ;;create constant?

(def ^:doc-const
  ^{:source "THERMOS"}
  pipe-velocity-offset
  "Offset term in the equation to calculate velocity for a given pipe diameter.  
   Shifts the entire velocity curve up or down so that the fitted
   equation fits the table of recommended design velocities for different diameters of pipe CIBSE CP1 (2020) Table 4."
  -0.4834)

(def ^:doc-const
  ^{:source "THERMOS"}
  pipe-velocity-coefficient
  "Scaling coefficient for the diameter term in the equation to calculate velocity for a 
   given pipe diameter. Controls how strongly changes to pipe diameter affect the predicted velocity
   to fit the table of recommended design velocities for different diameters of pipe CIBSE CP1 (2020) Table 4."
  4.7617)

(def ^:doc-const
  ^{:source "THERMOS"}
  pipe-diameter-exponent
  "Exponent applied to internal pipe diameter in the equation to calculate velocity for a 
   given pipe diameter. Defines the curvature of the relationship to fit 
   the table of recommended design velocities for different diameters of pipe CIBSE CP1 (2020) Table 4."
  0.3701)

(def ^:doc-const
  ^{:source "CIBSE CP1 https://www.cibse.org/knowledge-research/knowledge-portal/cp1-heat-networks-code-of-practice-for-the-uk-2020-pdf"}
  pipe-maximum-velocity-m-per-s
  "The maximum velocity for a pipe in line with the CIBSE CP1 Guidance for Heat Networks, 
   which states the maximum velocity for larger bore pipes is 3m/s"
  3.0)

(defn velocity-m-per-s
  "Calculates the water velocity for a pipe of a given diameter in metres using an approximation derived in the THERMOS project.
   This uses a curve to fit the table of recommended design velocities for different diameters of pipe CIBSE CP1 (2020) Table 4."
  ^double
  [^double diameter-m]
  (min
   pipe-maximum-velocity-m-per-s
   (+ (*
       pipe-velocity-coefficient
       (Math/pow diameter-m pipe-diameter-exponent))
      pipe-velocity-offset)))

(defn kw-per-m
  "For a hot water pipe, how many kw can we carry with a given diameter
  in metres?"
  ^double
  [^double diameter-m
   ^double delta-t
   ^double t-avg]
  (let [density   (water-density t-avg)
        area      (* Math/PI (Math/pow (* diameter-m 0.5) 2.0))
        velocity  (velocity-m-per-s diameter-m)
        flow-rate (* area velocity)
        mass-rate (* flow-rate density)]
    (* mass-rate heat-capacity delta-t)))

(defn curves
  "Precompute information for evaluating pipe sizes; takes `rows`, fills in the diameter,
  capacity-kw and losses-w, and puts them into a structure that is efficient for the
  lookups we have to do elsewhere."
  [^double flow ^double return ^double ground pipe-costs]

  (let [rows (:rows pipe-costs)

        curve-data
        (let [delta-t (- flow return)
              t-bar (/ (+ flow return) 2.0)
              delta-g (- t-bar ground)
              pipe-rows (into (sorted-map) rows)]
          (vec
           (for [[dia row] pipe-rows
                 :let [dia (/ dia 1000.0)
                       capacity-kw (or (:capacity-kw row)
                                       (kw-per-m dia delta-t t-bar))
                       losses-w   (or (some-> (:losses-kwh row)
                                              (annual-kwh->kw)
                                              (* 1000.0))
                                      (heat-loss-w-per-m delta-g dia))]
                 :when (and capacity-kw losses-w)]
             (merge
              row
              {:diameter dia
               :capacity-kw capacity-kw
               :losses-w losses-w}))))]

    {:default-civils (:default-civils pipe-costs)

     :dia->kw
     (->> curve-data
          (map (juxt :diameter :capacity-kw))
          (sort)
          (vec))

     :min-kw
     (reduce min
             Double/MAX_VALUE
             (map :capacity-kw curve-data))

     :max-kw
     (reduce max 0 (map :capacity-kw curve-data))

     :heat-loss-curve
     (->> curve-data
          (map (juxt :capacity-kw :losses-w))
          (sort)
          (vec))

     :data
     (->> curve-data
          (assoc-by :capacity-kw)
          (into (sorted-map)))}))

(defn dia->kw-range
  "Given the output of `curves`, return [min kW, max kW], the range in kW for which we would use a pipe of this diameter. That is the size of the next pipe down + 1 kW upto this one."
  [curves dia-mm]

  (let [dia (/ dia-mm 1000.0)
        dia->kw (into (sorted-map) (:dia->kw curves))
        next-up (rsubseq dia->kw <= dia)]
    (cond
      (empty? next-up)
      ;; diameter is below minimum, so we want 0 .. min
      [0 (second (first dia->kw))]

      (== (first (first next-up)) dia) ;; diameter is in the table
      (let [[[_ a] [_ b]] next-up] [(inc (or b -1)) a])

      :else ;; diameter is not in the table
      (let [diameter-up (first (first (subseq dia->kw > dia)))]
        (if (nil? diameter-up)
          (throw (ex-info "Diameter is larger than maximum known in table" {:dia dia :dia->kw dia->kw}))
          (dia->kw-range curves (* 1000.0 diameter-up)))))))

(defn dia->kw
  "Given some curves from above, interpolate out the kw carried by a pipe of given dia.
  This is happy with diameters that are not in the table."
  [curves dia-mm]

  (linear-evaluate (:dia->kw curves) (/ dia-mm 1000.0)))

(defn max-kw
  "What's the max power on this set of curves?"
  [curves]

  (:max-kw curves 0))

(defn min-kw
  "What's the min power on this set of curves?"
  [curves]

  (:min-kw curves 0))

(defn heat-loss-curve
  "Return a vector like [[kwp, w-losses/m], ...]"
  [curves]

  (:heat-loss-curve curves))

(defn curve-rows
  "Return the rows information in curves. This is like the :rows the input to `curves`
  except that
  - the losses and capacity are filled in for rows where that data is missing.
  - it's a seq of maps, and the maps have :diameter, rather than being a map from diameter.
  "
  [curves]
  (sort-by :diameter (vals (:data curves))))

(defn- la-solution [k1 k2 k3 k4 k5]
  (let [top (- (* 2 k2 k3)
               (* k4 k5))
        den (- (* k5 k5) (* 4 k1 k3))
        m (if (zero? top) 0.0 (/ top den))
        top (+ k2 (* 2 m k1))
        c (if (zero? top) 0 (/ (- top) k5))]
    [c m]))

(defn- linear-approximate
  "Curve is a series of x,y points making a piecewise linear function.
  Return [c m] such that the curve is 'best' approximated by y=mx + c
  over the interval xmin - xmax

  The formula used comes from minimising analytically the square
  difference between the linear form and the curve. I did this by
  taking the derivative and solving where it's zero."
  [curve x-min x-max]
  (cond
    (< (Math/abs (- x-min x-max)) 0.1)
    [(linear-evaluate curve (/ (+ x-min x-max) 2)) 0]

    (<= x-max (first (first curve)))
    [(second (first curve)) 0]

    (>= x-min (first (last curve)))
    [(second (last curve)) 0]

    :else
    ;; compute an optimal m and c to minimize error

    (let [imax (dec (count curve))]
      (loop [i  (int 0)
             k1 (double 0)
             k2 (double 0)
             k3 (double 0)
             k4 (double 0)
             k5 (double 0)]
        (if (= i imax) (la-solution k1 k2 k3 k4 k5)
            (let [[xi yi] (nth curve i)
                  [xj yj] (nth curve (inc i))]
              (cond
                (or (= i imax)
                    (> xi x-max))
                (la-solution k1 k2 k3 k4 k5)

                (< xj x-min)
                (recur (inc i) k1 k2 k3 k4 k5)

                :else
                (let [mi (/ (- yj yi) (- xj xi))
                      ci (- yi (* mi xi))
                      ;; clamp left
                      xi (if (< xi x-min) x-min xi)
                      yi (if (<= xi x-min) (+ ci (* mi x-min)) yi)
                      ;; clamp right
                      xj (if (> xj x-max) x-max xj)
                      yj (if (>= xj x-max) (+ ci (* mi x-max)) yj)

                      X3 (- (Math/pow xj 3) (Math/pow xi 3))
                      X2 (- (Math/pow xj 2) (Math/pow xi 2))
                      X1 (- xj xi)]

                  (recur (inc i)
                         (+ k1 (/ X3 3.0))
                         (+ k2 (* -2.0 mi (/ X3 3.0)) (- (* ci X2)))
                         (+ k3 X1)
                         (+ k4 (* (- mi) X2) (- (* 2 ci X1)))
                         (+ k5 X2))))))))))

(defn linear-cost
  "Give the linearised approximate cost function for something with
  given civil cost and power range.

  - `curves` is the result of the function above
  - `length-by-civils` is a map from civil cost ID to length of the path that has that cost ID
  - `total-length` is used to normalize the cost that comes out
  - `kw-min` and `kw-max` decide which bit of the resulting cost curve we are approximating.

  Returns a tuple of [fixed cost, cost/kwp]
  "
  [curves length-by-civils total-length kw-min kw-max]

  (let [data (:data curves)
        default-cost (:default-civils curves)

        cost-curve
        (for [[capacity costs] data]
          [capacity
           (reduce-kv
            (fn [acc cost-id length]
              (+ acc
                 (* (/ length total-length)
                    (+ (get costs :pipe 0)
                       (get costs (or cost-id default-cost) 0)))))
            0
            length-by-civils)])]

    (let [curve-min (min-kw curves)
          curve-max (max-kw curves)

          kw-min (min curve-max (max kw-min curve-min))
          kw-max (max curve-min (min kw-max curve-max))

          terms (linear-approximate cost-curve kw-min kw-max)
          ;; [c m] terms
          ]
      terms)))

(defn solved-principal
  "Given a kwp, how much would a certain path cost per meter"
  [curves civil-id kwp]

  (let [default-cost (:default-civils curves)
        data    (:data curves)
        next-up (or (second (first (subseq data >= kwp)))
                    (second (last data)))]
    (+ (get next-up :pipe 0)
       (get next-up (or civil-id default-cost) 0))))

(defn solved-diameter [curves kwp]
  (let [data    (:data curves)
        next-up (or (second (first (subseq data >= kwp)))
                    (second (last data)))]
    (* 1000.0 (get next-up :diameter))))

(defn solved-losses-kwh%m [curves kwp]
  (let [data    (:data curves)
        next-up (or (second (first (subseq data >= kwp)))
                    (second (last data)))]
    (annual-kw->kwh
     (/ (get next-up :losses-w) 1000.0))))
