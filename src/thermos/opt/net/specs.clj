;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.net.specs
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]))

(def vertex
  [:map
   [:id :any]

   [:demand {:optional true}
    [:map
     [:kwh :double]
     [:kwp :double]
     [:required {:optional true} :boolean]
     [:off-network {:optional true} :boolean]

     [:value {:optional true} :double]
     [:value%kwp {:optional true} :double]
     [:value%kwh {:optional true} :double]
     [:count {:optional true} :int]
     [:group {:optional true} :any]

     [:insulation {:optional true}
      [:* [:map
           [:id :any]
           [:cost {:optional true} :double]
           [:cost%kwh {:optional true} :double]
           [:maximum {:optional true} :double]]]]

     [:alternatives {:optional true}
      [:* [:map
           [:id :any]
           [:cost {:optional true} :double]
           [:cost%kwh {:optional true} :double]
           [:cost%kwp {:optional true} :double]
           [:emissions {:optional true}
            [:map-of :any :double]]]]]]

    [:supply {:optional true}
     [:map
      [:capacity-kw :double]
      [:capacity-kwh {:optional true} :double]
      [:cost {:optional true} :double]
      [:cost%kwh {:optional true} :double]
      [:cost%kwp {:optional true} :double]
      [:emissions {:optional true} [:map-of :any :double]]
      [:exclusive-groups {:optional true} [:set :any]]]]]])

(def edge
  [:map
   [:i :any]
   [:j :any]
   [:length :double]

   [:cost%m {:optional true} :double]
   [:cost%kwm {:optional true} :double]

   [:required {:optional true} :boolean]

   [:max-capacity%kwp {:optional true} :double]

   ;; These are only relevant if there is an existing pipe.
   ;; When there is an existing pipe, the upgrade costs get paid if
   ;; the required capacity is over the existing capacity.
   ;; The basic costs get paid in any case, so the upgrade cost
   ;; has to be the delta.
   [:existing-capacity%kwp {:optional true} :double]
   [:upgrade-cost%m {:optional true}   :double]
   [:upgrade-cost%kwm {:optional true} :double]])

(def network-problem
  (m/schema
   [:map
    [:emissions {:optional true}
     [:map-of
      :any [:map
            [:cost {:optional true} :double]
            [:maximum {:optional true} :double]]]]
    
    [:diversity-limit {:optional true} [:double
                                        {:default 0.62}]]
    [:diversity-rate {:optional true} [:double
                                       {:default 1.0}]]
    [:pipe-losses {:optional true}
     [:map
      [:kwp [:vector :double]]
      [:w%m [:vector :double]]]]
    
    [:force-insulation {:optional true} :boolean]
    [:supply-limit {:optional true} :int]
    [:objective-scale [:double {:default 1.0}]]
    [:objective-precision  [:double {:default 0.0}]]
    [:edge-cost-precision  [:double {:default 0.0}]]
    [:no-loops  {:optional true}
     [:boolean {:default false}]]
    [:objective
     [:enum {:default :max-npv} :max-npv :max-kwh]]

    [:iteration-limit [:int {:default 1000}]]
    [:time-limit [:double {:default 1.0}]]
    [:mip-gap  [:double {:default 0.05}]]
    [:param-gap [:double {:default 0.0}]]
    [:should-be-feasible [:boolean {:default false}]]
    
    [:vertices [:+ vertex]]
    [:edges [:* edge]]

    [:constraints
     ;; TODO a nice constraint to have here would be something like
     ;; max NPV s.t. max capex = C
     ;; but at the moment the model cannot see capex vs future costs.
     ;; and it would be a big change to support this
     (->> (for [var [:kwh :npv :length :linear-density]]
            [var {:optional true}
             [:map
              [:min {:optional true} [:maybe :double]]
              [:max {:optional true} [:maybe :double]]]])
          (concat
           (for [var [:building-count :connection-count]]
             [var {:optional true}
              [:map
               [:min {:optional true} [:maybe :int]]
               [:max {:optional true} [:maybe :int]]]]))
          (into [:map {:default {}}]))]]))

(let [coerce (m/coercer
              network-problem
              (mt/transformer
               mt/json-transformer ;; cleans up doubles etc
               (mt/default-value-transformer {::mt/add-optional-keys true})))]
  (defn ensure-valid-problem
    "Check problem is valid, or throw a diagnostic exception"
    [problem]
    (try
      (coerce problem)
      (catch clojure.lang.ExceptionInfo e
        (throw (ex-info "Invalid network problem"
                        (-> (ex-data e) :data :explain me/humanize)))))))

