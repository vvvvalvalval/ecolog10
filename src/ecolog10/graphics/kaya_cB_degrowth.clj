(ns ecolog10.graphics.kaya-cB-degrowth
  (:require [ecolog10.centibels-math :as cB]
            [ecolog10.graphics.reduction-plans.commons :as red-plans]
            [ecolog10.input-data :as input-data]
            [ecolog10.utils.data :as udata]
            [oz.core :as oz]))


(defn predicted-population
  [year-map]
  (or
    (:population_prediction_UN_medium_variant year-map)
    (:population_UN_estimate_1750_1950 year-map)
    (:population_HYDE_estimate_1750_1950 year-map)))


(def pop-2021
  (->> (deref input-data/d_historical-and-projected-population)
    (filter #(-> % :year (= 2021)))
    first
    predicted-population))


(def kaya-time-chart
  {:$schema "https://vega.github.io/schema/vega/v5.json",
   :title "Influence of economic and demographic growth on economic decarbonization requirements for a -2.89 cB/year pathway"
   :description "",
   :width 400, :height 200,
   :scales [{:name "x",
             :type :time
             :range "width",
             :zero false :nice true
             :domain {:data "table", :field "x"}}
            {:name "y",
             :type "linear",
             :range "height",
             :nice true,
             :zero true,
             :domain {:data "table", :field "ghgvar_plus_pop"}}
            {:name "color",
             :type "ordinal",
             :range "category",
             :domain [0 1 2]}],
   :axes [{:title "Time"
           :orient "top",
           :scale "x",
           :grid true
           :zindex 0}
          {:title "GHG/GDP variation (cB)"
           :orient "left",
           :scale "y",
           :grid true
           :zindex 0}],
   :padding 5,
   :data [{:name "table",
           :values
           (vec
             (for [ym (deref input-data/d_historical-and-projected-population)
                   :let [year (:year ym)]
                   :when (<= 2020 year 2050)]
               (let [no-growth
                     (if (< year 2021)
                       0.
                       (/
                         (*
                           red-plans/exp-decay-limit-year-target-cB
                           (- year 2021))
                         (- 2036 2021)))
                     plus-gdp-growth
                     (+ no-growth
                       (if (< year 2021)
                         0.
                         (*
                           (-> 2 cB/rpct-to-scalar cB/scalar-to-cB (-))
                           (- year 2021))))
                     plus-pop-growth
                     (+ plus-gdp-growth
                       (if (< year 2021)
                         0.
                         (-> (/ (predicted-population ym) pop-2021)
                           (cB/scalar-to-cB)
                           (-))))]
                 {:x (udata/year->temporal year)
                  :year year
                  :ghgvar_no_growth no-growth
                  :ghgvar_plus_gdp plus-gdp-growth
                  :ghgvar_plus_pop plus-pop-growth})))}
          {:name "rightmost_point"
           :source "table"
           :transform
           [{:type :aggregate
             :fields [:year]
             :ops [:argmax]
             :as [:k]}
            {:type :project
             :fields ["k.x"
                      "k.year"
                      "k.ghgvar_no_growth"
                      "k.ghgvar_plus_gdp"
                      "k.ghgvar_plus_pop"]
             :as [:x
                  :year
                  :ghgvar_no_growth
                  :ghgvar_plus_gdp
                  :ghgvar_plus_pop]}]}]
   :marks [
           {:type "area",
            :from {:data "table"},
            :encode {:enter {:x {:scale "x", :field :x},
                             :y {:scale "y", :field :ghgvar_no_growth}
                             :y2 {:scale "y", :field :ghgvar_plus_gdp}
                             :fill {:scale "color", :value 1}}
                     :update {:fillOpacity {:value 0.7}},
                     :hover {:fillOpacity {:value 0.5}}}}
           {:type :text
            :from {:data "rightmost_point"}
            :encode {:update
                     {:text {:value "No-growth baseline (-2.89 cB/year)"}
                      :align {:value "left"},
                      :baseline {:value "middle"}
                      :x {:scale "x", :field :x, :offset 3},
                      :y {:scale "y", :field :ghgvar_no_growth},
                      :fill {:scale "color" :value 0}}}}
           {:type "area",
            :from {:data "table"},
            :encode {:enter {:x {:scale "x", :field :x},
                             :y {:scale "y", :field :ghgvar_plus_gdp}
                             :y2 {:scale "y", :field :ghgvar_plus_pop}
                             :fill {:scale "color", :value 2}}
                     :update {:fillOpacity {:value 0.7}},
                     :hover {:fillOpacity {:value 0.5}}}}
           {:type :text
            :from {:data "rightmost_point"}
            :encode {:enter {:align {:value "left"},
                             :baseline {:value "middle"}},
                     :update
                     {:x {:scale "x", :field :x, :offset 3},
                      :y {:scale "y", :signal "0.5 * (datum.ghgvar_no_growth + datum.ghgvar_plus_gdp)"},
                      :text {:value "+2% GDP-per-capita annual growth"},
                      :fill {:scale "color" :value 1}}}}
           {:type "line",
            :from {:data "table"},
            :encode {:enter {:x {:scale "x", :field :x},
                             :y {:scale "y", :field :ghgvar_no_growth}
                             :stroke {:scale "color", :value 0}}
                     :update {:fillOpacity {:value 0.7}},
                     :hover {:fillOpacity {:value 0.5}}}}
           {:type :text
            :from {:data "rightmost_point"}
            :encode {:enter {:align {:value "left"},
                             :baseline {:value "middle"}},
                     :update
                     {:x {:scale "x", :field :x, :offset 3},
                      :y {:scale "y", :signal "0.5 * (datum.ghgvar_plus_gdp + datum.ghgvar_plus_pop)"},
                      :text {:value "+ UN projected population growth"},
                      :fill {:scale "color" :value 2}}}}

           #_{:type "group",
              :from {:facet {:name "series", :data "table", :groupby "c"}},
              :marks [{:type "area",
                       :from {:data "series"},
                       :encode {:enter {:interpolate {:value "monotone"},
                                        :x {:scale "x", :field "x"},
                                        :y {:scale "y", :field "y0"},
                                        :y2 {:scale "y", :field "y1"},
                                        :fill {:scale "color", :field "c"}},
                                :update {:fillOpacity {:value 1}},
                                :hover {:fillOpacity {:value 0.5}}}}]}],})

(defn get-vega-chart
  []
  [:vega kaya-time-chart])

;(oz/view! (get-vega-chart))
