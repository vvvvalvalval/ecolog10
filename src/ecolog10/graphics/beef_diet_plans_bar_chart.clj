(ns ecolog10.graphics.beef-diet-plans-bar-chart
  (:require [data-carving.transform :as dc]
            [data-carving.ui.incremental-oz :as incr-oz]
            [ecolog10.centibels-math :as cB]
            [ecolog10.graphics.reduction-plans.commons :as red-plans]
            [lambdaisland.deep-diff2 :as ddiff]
            [oz.core :as oz]))


(def beef-reduction-plans
  [{:plan_actions
    [{:rdact_description "10X fewer meat meals"
      :rdact_cb -100.
      :rdact_factor "consumption"}]}
   {:plan_actions
    (let [cb0 (-> -60. (cB/rpct-to-scalar) (cB/scalar-to-cB))]
      [{:rdact_description "Switch to low-carbon beef producers"
        :rdact_cb cb0
        :rdact_factor "intensity"}
       {:rdact_description "4X fewer meat meals"
        :rdact_cb (- -100. cb0)
        :rdact_factor "consumption"}])}
   {:plan_actions
    (let [cb0 (-> -45. (cB/rpct-to-scalar) (cB/scalar-to-cB))]
      [{:rdact_description "Replace half of beef by poultry/pork"
        :rdact_cb cb0
        :rdact_factor "intensity"}
       {:rdact_description "5X fewer meat meals"
        :rdact_cb (- -100. cb0)
        :rdact_factor "consumption"}])}
   {:plan_actions
    [{:rdact_description "Replace all beef by poultry"
      :rdact_cb -100.
      :rdact_factor "intensity"}]}])


(def transforms-for-reduction-plans-bar-chart
  (red-plans/reduction-plans-bar-chart-transforms beef-reduction-plans "4 diet plans to reduce GHG emissions from beef consumption."))


(defn get-vega-chart
  []
  [:vega (dc/end-result red-plans/initial-vertical-bar-chart transforms-for-reduction-plans-bar-chart)])


;(oz/view! (get-vega-chart))
;(oz/view! (incr-oz/sculpture-oz-page {::incr-oz/viz-type :vega, ::incr-oz/targets-reagent? true} red-plans/initial-vertical-bar-chart transforms-for-reduction-plans-bar-chart))

(comment

  (oz/export!
    (incr-oz/sculpture-oz-page {::incr-oz/viz-type :vega, ::incr-oz/targets-reagent? false} red-plans/initial-vertical-bar-chart transforms-for-reduction-plans-bar-chart)
    "generated/sculpture-test-0.html")

  (ddiff/pretty-print
    (ddiff/diff {:a 1 :b 2} {:a 1 :b 3})
    (ddiff/printer {:color-markup :html-inline}))

  *e)



