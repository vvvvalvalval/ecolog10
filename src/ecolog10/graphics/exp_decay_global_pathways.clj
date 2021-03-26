(ns ecolog10.graphics.exp-decay-global-pathways
  (:require [ecolog10.centibels-math :as cB]
            [ecolog10.graphics.reduction-plans.commons :as red-plans]
            [ecolog10.input-data :as input-data]
            [ecolog10.utils.data :as udata]
            [oz.core :as oz]))

(def ref-year 2020.7)


(def redstart-years
  [2021
   2026
   2031])


(def exp-decay-multi-chart
  (let [limit-year (+ ref-year (/ input-data/remaining-carbon-budget input-data/yearly-emissions-2019))]
    {:$schema "https://vega.github.io/schema/vega-lite/v4.json",
     :padding 20
     :description "Various scenarios for constant centibel-speed (a.k.a 'exponential decay') emissions pathways",
     :title {:text "Reducing emissions at constant centibel-speed (exponential decay)"
             :subtitle "Deducing the centibel-slope from the remaining emissions budget and the '-43.4 cB guideline'"
             :align "center"
             :anchor "middle"
             :orient "bottom"
             :baseline "top"}
     :vconcat
     [{:height 150 :width 300
       :layer
       [{:data {:values [{:t_0 (udata/year->temporal ref-year)
                          :t_l (udata/year->temporal limit-year)
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
                (for [rstart-y (reverse redstart-years)      ;; HACK to have earliest on top.
                      t (concat (range 2019 2045 0.1) [2045])
                      :let [cb-red (if (< t rstart-y)
                                     0.
                                     (*
                                       red-plans/exp-decay-limit-year-target-cB
                                       (double
                                         (/
                                           (- t rstart-y)
                                           (- limit-year rstart-y)))))]]
                  {:t (udata/year->temporal t)
                   ;; FIXME I don't get why this displays so poorly
                   :yearly_ghg_emissions
                   (-> cb-red
                     (cB/cB-to-scalar)
                     (* input-data/yearly-emissions-2019))

                   :red_start_year rstart-y})}
         :mark {:type :line
                :interpolate :monotone}
         :encoding {:x {:field :t, :type "temporal",
                        :title "Time"}
                    :y {:field :yearly_ghg_emissions, :type "quantitative"
                        :title "CO₂ emissions (GtCO₂e/year)"}
                    :color {:field :red_start_year, :type "nominal"
                            :legend {:title "Reductions start year"}}
                    :strokeWidth {:value 1}}}
        {:data {:values [{:t (udata/year->temporal limit-year)
                          :init_ghg_emissions input-data/yearly-emissions-2019
                          :zero_emissions 0.}]}
         :transform
         [{:calculate "'Pivot Year: ' + timeFormat(datum.t, '%Y')"
           :as :my_rule_subtext}]
         :encoding {:opacity {:value 0.5}}
         :layer
         [{:mark "rule"
           :encoding {:x {:field :t, :type "temporal"}
                      :y {:field :init_ghg_emissions, :type "quantitative"}
                      :y2 {:field :zero_emissions, :type "quantitative"}
                      :strokeDash {:value [3 2]}
                      :strokeWidth {:value 1}}}
          {:mark {:type "text",
                  :align "left",
                  :baseline "bottom",
                  :dx 2,
                  :dy -2,
                  :y "height"}
           :encoding {:x {:field :t, :type "temporal"}
                      :text {:field :my_rule_subtext}}}]}]}
      {:height 200 :width 300
       :layer
       [{:data {:values
                (for [rstart-y (reverse redstart-years)      ;; HACK to have earliest on top.
                      t [2019
                         rstart-y
                         limit-year
                         2045]
                      :let [cb-red (if (< t rstart-y)
                                     0
                                     (* red-plans/exp-decay-limit-year-target-cB
                                       (double
                                         (/
                                           (- t rstart-y)
                                           (- limit-year rstart-y)))))]]
                  {:t (udata/year->temporal t)
                   :cB_red cb-red
                   :red_start_year rstart-y})}
         :mark {:type :line
                :interpolate :linear}
         :encoding {:x {:field :t, :type "temporal",
                        :title nil
                        :axis {:orient "top"}},
                    :y {:field :cB_red, :type "quantitative"
                        :title "Reduction (cB)"}
                    :color {:field :red_start_year, :type "nominal"
                            :legend {:title "Reductions start year"}}
                    :strokeWidth {:value 1}}}
        {:data {:values
                (for [rstart-y (reverse redstart-years)]
                  (let [t 2045]
                    {:t (udata/year->temporal t)
                     :red_start_year rstart-y
                     :cB_red
                     (* red-plans/exp-decay-limit-year-target-cB
                       (double
                         (/
                           (- t rstart-y)
                           (- limit-year rstart-y))))
                     :cB_reduction_speed (/ red-plans/exp-decay-limit-year-target-cB
                                           (- limit-year rstart-y))}))}
         :transform
         [{:calculate "format(datum.cB_reduction_speed, '.2f') + ' cB/year'"
           :as :cB_reduction_speed_description}]
         :encoding {:x {:field :t, :type "temporal",
                        #_#_:title "Time"},
                    :y {:field :cB_red, :type "quantitative"
                        :title "Reduction (cB)"}
                    :color {:field :red_start_year, :type "nominal"
                            :legend {:title "Reductions start year"}}},
         :layer [#_{:mark {:type "circle"}}
                 {:mark {:type "text", :align "left", :dx 4}
                  :encoding {:text
                             {:field :cB_reduction_speed_description}}}]}
        {:data {:values [{:cB_red red-plans/exp-decay-limit-year-target-cB}]}
         :transform
         [{:calculate "format(datum.cB_red, '.1f') + ' cB'"
           :as :my_rule_subtext}]
         :encoding {:color {:value "darkgreen"}
                    :strokeDash {:value [5, 3]}
                    :opacity {:value 1.}}
         :layer
         [{:mark "rule"
           :encoding {:y {:field :cB_red, :type "quantitative"}
                      :strokeWidth {:value 1}}}
          {:mark {:type "text",
                  :align "left",
                  :baseline "middle",
                  :dx 2,
                  ;:dy -2,
                  :x "width"}
           :encoding {:y {:field :cB_red, :type "quantitative"}
                      :text {:field :my_rule_subtext}}}]}
        {:data {:values [{:t (udata/year->temporal limit-year)}]}
         :transform
         [{:calculate "'Pivot Year: ' + timeFormat(datum.t, '%Y')"
           :as :my_rule_subtext}]
         :encoding {:opacity {:value 0.5}}
         :layer
         [{:mark "rule"
           :encoding {:x {:field :t, :type "temporal"}
                      :strokeDash {:value [3 2]}
                      :strokeWidth {:value 1}}}
          {:mark {:type "text",
                  :align "left",
                  :baseline "top",
                  :dx 2,
                  :dy 2,
                  :y 0}
           :encoding {:x {:field :t, :type "temporal"}
                      :text {:field :my_rule_subtext}}}]}]}]}))


(defn get-vegalite-chart
  []
  [:vega-lite exp-decay-multi-chart])

;(oz/view! (get-vegalite-chart))
