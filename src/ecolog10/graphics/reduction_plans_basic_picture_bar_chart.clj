(ns ecolog10.graphics.reduction-plans-basic-picture-bar-chart
  (:require [data-carving.transform :as dc]
            [ecolog10.centibels-math :as cB]
            [ecolog10.graphics.reduction-plans.commons :as red-plans]
            [ecolog10.graphics.utils.vega :as uvega]
            [oz.core :as oz]))


(def basic-reduction-plans
  [{:plan_actions
    [{:rdact_description "-50% GHG intensity"
      :rdact_cb (-> -50. (cB/rpct-to-scalar) (cB/scalar-to-cB))
      :rdact_factor "intensity"}
     {:rdact_description "-40% Consumption level"
      :rdact_cb (-> -40. (cB/rpct-to-scalar) (cB/scalar-to-cB))
      :rdact_factor "consumption"}]}])


(def basic-reductions_transforms
  (into (red-plans/reduction-plans-bar-chart-transforms basic-reduction-plans nil)
    [(dc/tfn add-total-rpct
       [vega-spec]
       (-> vega-spec
         (update :data conj
           {:name "reduction_plans",
            :values (->> basic-reduction-plans
                      (into []
                        (map-indexed
                          (fn [plan-id rplan]
                            (let [plan_total_cb (->> rplan :plan_actions
                                                  (map :rdact_cb)
                                                  (apply + 0.))]
                              {:plan_id plan-id
                               :plan_total_cb plan_total_cb
                               :plan_total_rpct
                               (-> plan_total_cb
                                 (cB/cB-to-scalar)
                                 (cB/scalar-to-rpct))})))))})
         (update :marks conj
           {:name "plan_total_reduction_txt"
            :type "text",
            :from {:data "reduction_plans"},
            :encode {:enter
                     (-> {:text {:signal "format(datum.plan_total_rpct, '.1f') + '%'"}
                          :x {:scale "xscale", :field :plan_total_cb, :offset 2}
                          :y {:scale "yscale", :field :plan_id}
                          :width {:scale "xscale", :field :rdact_cb}
                          :height {:scale "yscale"}
                          :align {:value "left"},
                          :fontStyle {:value :italic}
                          :fontWeight {:value :bold}
                          :fillOpacity {:value 0.6}}
                       (uvega/text_center-in-hband))}})))]))


(defn get-vega-chart
  []
  [:vega (dc/end-result red-plans/initial-vertical-bar-chart basic-reductions_transforms)])


;(oz/view! (get-vega-chart))