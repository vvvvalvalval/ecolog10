(ns ecolog10.graphics.sketch-ghg-intensity-extrapolations
  (:require [oz.core :as oz]))


(def proxy-cost-sketch
  {:$schema "https://vega.github.io/schema/vega-lite/v4.json",
   :description "Various scenarios for constant centibel-speed (a.k.a 'exponential decay') emissions pathways",
   :title {:text "Sketch: approximations of ROI in GHG-intensity reduction by local extrapolations"}
   :height 150 :width 300
   :data {:values
          (let [A -1
                B (/ 1. 5.)]
            (for [x (range 0. 10. 1e-1)
                  :let [;; NOTE this example function was chosen by assuming a implicit experience curve effect,
                        ;; for which both the GHG-intensity and the invested R&D cost are proportional to the cost.
                        Gx (Math/pow (+ 1 (* B x)) A)
                        G*x (+ 1 (* A B x))
                        G**x (Math/exp (* A B x))]
                  m [{:ghg_intensity_curve "0 True G(x)"
                      :reduction_cost x
                      :ghg_intensity Gx
                      :is_exact true}
                     {:ghg_intensity_curve "1 cB-based approximation"
                      :reduction_cost x
                      :ghg_intensity G**x
                      :is_exact false}
                     {:ghg_intensity_curve "2 %-based approximation"
                      :reduction_cost x
                      :ghg_intensity G*x
                      :is_exact false}]
                  :when (<= 0 (:ghg_intensity m))]
              m))}
   :config {:axis {:ticks false
                   :labels false
                   :grid false}}
   :layer
   [{:mark {:type :line
            :interpolate :monotone}
     :transform [{:filter "datum.is_exact"}]
     :encoding {:x {:field :reduction_cost, :type "quantitative",
                    :title "Investment in reduction"}
                :y {:field :ghg_intensity, :type "quantitative"
                    :title "GHG intensity"}
                :color {:field :ghg_intensity_curve,
                        :type "nominal"
                        :legend {:title nil
                                 :labelExpr "slice(datum.value, 2)"}}
                :strokeWidth {:value 2}}}
    {:mark {:type :line
            :interpolate :monotone}
     :transform [{:filter "!datum.is_exact"}]
     :encoding {:x {:field :reduction_cost, :type "quantitative",
                    :title "Investment in reduction"}
                :y {:field :ghg_intensity, :type "quantitative"
                    :title "GHG intensity"}
                :color {:field :ghg_intensity_curve,
                        :type "nominal"
                        :legend {:title nil
                                 :labelExpr "slice(datum.value, 2)"}}
                :strokeDash {:value [4 1]}
                :strokeWidth {:value 1}}}]})


(defn get-vegalite-chart
  []
  [:vega-lite proxy-cost-sketch])


;(oz/view! (get-vegalite-chart))

