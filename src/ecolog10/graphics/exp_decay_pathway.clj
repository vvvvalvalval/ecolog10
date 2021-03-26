(ns ecolog10.graphics.exp-decay-pathway
  (:require [ecolog10.input-data :as input-data]
            [ecolog10.centibels-math :as cB]
            [ecolog10.graphics.reduction-plans.commons :as red-plans]
            [ecolog10.utils.data :as udata]
            [oz.core :as oz]))


(def ref-year 2021)


(def exp-decay-single-chart
  (let [pivot-year (+ ref-year (/ input-data/remaining-carbon-budget input-data/yearly-emissions-2019))]
    {:$schema "https://vega.github.io/schema/vega-lite/v4.json",
     :description "",
     :title {:text "Exponential-decay emissions reduction pathway"
             :align "center"
             :anchor "middle"
             :orient "bottom"
             :baseline "top"}
     :padding 20
     :vconcat
     [{:height 120 :width 360
       :layer
       [{:data {:values [{:t_0 (udata/year->temporal ref-year)
                          :t_l (udata/year->temporal pivot-year)
                          :zero_emissions 0.
                          :init_ghg_emissions input-data/yearly-emissions-2019
                          :rem_carbon_budget input-data/remaining-carbon-budget}]}
         :layer [{:mark {:type :rect}
                  :encoding {:x {:field :t_0 :type "temporal"}
                             :x2 {:field :t_l :type "temporal"}
                             :y {:field :zero_emissions :type :quantitative}
                             :y2 {:field :init_ghg_emissions :type :quantitative}
                             :color {:value "red"}
                             :opacity {:value 0.15}}}
                 {:mark {:type "text",
                         :align "left",
                         :baseline "bottom",
                         :dx 5
                         :dy -20}
                  :transform
                  [{:calculate "['Remaining emissions budget:', format(datum.rem_carbon_budget, '.0f') + ' GtCO₂e as of ' + timeFormat(datum.t_0, '%Y/%m')]"
                    :as :remaining_emissions_subtext}]
                  :encoding {:x {:field :t_0 :type "temporal"}
                             :y {:field :zero_emissions :type :quantitative}
                             :text {:field :remaining_emissions_subtext}
                             :color {:value "red"}
                             :opacity {:value 0.6}}}]}
        {:data {:values
                (for [t (concat (range 2019 2045 0.1) [2045])
                      :let [cb-red (if (< t ref-year)
                                     0.
                                     (*
                                       red-plans/exp-decay-limit-year-target-cB
                                       (double
                                         (/
                                           (- t ref-year)
                                           (- pivot-year ref-year)))))]]
                  {:t (udata/year->temporal t)
                   :yearly_ghg_emissions
                   (-> cb-red
                     (cB/cB-to-scalar)
                     (* input-data/yearly-emissions-2019))

                   :red_start_year ref-year})}
         :mark {:type :line
                :interpolate :monotone}
         :encoding {:x {:field :t, :type "temporal",
                        :title "Time"}
                    :y {:field :yearly_ghg_emissions, :type "quantitative"
                        :title "CO₂ emissions (GtCO₂e/year)"}
                    :strokeWidth {:value 1}}}]}
      {:height 180 :width 360
       :layer
       [{:data {:values
                (for [t [2019
                         ref-year
                         pivot-year
                         2045]
                      :let [cb-red (if (< t ref-year)
                                     0
                                     (* red-plans/exp-decay-limit-year-target-cB
                                       (double
                                         (/
                                           (- t ref-year)
                                           (- pivot-year ref-year)))))]]
                  {:t (udata/year->temporal t)
                   :cB_red cb-red})}
         :mark {:type :line
                :interpolate :linear}
         :encoding {:x {:field :t, :type "temporal",
                        :title nil
                        :axis {:orient "top"}},
                    :y {:field :cB_red, :type "quantitative"
                        :title "Reduction (cB)"}
                    :strokeWidth {:value 1}}}
        {:data {:values
                [(let [t 2045]
                   {:t (udata/year->temporal t)
                    :red_start_year ref-year
                    :cB_red
                    (* red-plans/exp-decay-limit-year-target-cB
                      (double
                        (/
                          (- t ref-year)
                          (- pivot-year ref-year))))
                    :cB_reduction_speed (/ red-plans/exp-decay-limit-year-target-cB
                                          (- pivot-year ref-year))})]}
         :transform
         [{:calculate "format(datum.cB_reduction_speed, '.2f') + ' cB/year'"
           :as :cB_reduction_speed_description}]
         :encoding {:x {:field :t, :type "temporal"},
                    :y {:field :cB_red, :type "quantitative"
                        :title "Reduction (cB)"}},
         :layer [{:mark {:type "text", :align "left", :dx 4}
                  :encoding {:text
                             {:field :cB_reduction_speed_description}}}]}]}]}))


(defn get-vegalite-chart
  []
  [:vega-lite exp-decay-single-chart])


;(oz/view! (get-vegalite-chart))