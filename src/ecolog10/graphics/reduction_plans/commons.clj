(ns ecolog10.graphics.reduction-plans.commons
  (:require [com.rpl.specter :as sp]
            [data-carving.transform :as dc]
            [ecolog10.centibels-math :as cB]
            [ecolog10.graphics.utils.vega :as uvega]
            [ecolog10.utils.data :as udata]
            [vvvvalvalval.supdate.api :as supd]))




(def exp-decay-limit-year-target-cB
  "cB Reduction to achieve by the Pivot Year"
  (* 100
    (Math/log10
      (/ 1. Math/E))))


(def squared-exp-decay-limit-year-target-cB
  "cB Reduction to achieve by the Pivot Year"
  (* 100
    (Math/log10
      (Math/exp
        (/ Math/PI -4)))))


(comment

  ;; time until Pivot Year
  (/ 646.5 42.1)
  => 15.356294536817101

  ;; Since it's September 2020, that would mean early 2036.

  exp-decay-limit-year-target-cB
  => -43.42944819032518

  (-> exp-decay-limit-year-target-cB cB-to-scalar scalar-to-rpct)


  (/ exp-decay-limit-year-target-cB 15)
  => -2.8952965460216786

  (-> *1 cB-to-scalar scalar-to-rpct)
  ;; -6.45%

  (/ exp-decay-limit-year-target-cB 10)
  => -4.342944819032518

  (-> *1 cB-to-scalar scalar-to-rpct)
  ;; -9.52%


  *e)




(def initial-vertical-bar-chart
  {;; taken from: https://vega.github.io/vega/examples/bar-chart/
   :$schema "https://vega.github.io/schema/vega/v5.json",
   :description "A basic bar chart example, with value labels shown upon mouse hover.",

   :data [{:name "table",
           :values [{:category "A", :amount 28}
                    {:category "B", :amount 55}
                    {:category "C", :amount 43}
                    {:category "D", :amount 91}
                    {:category "E", :amount 81}
                    {:category "F", :amount 53}
                    {:category "G", :amount 19}
                    {:category "H", :amount 87}]}],

   :width 400, :height 200
   :padding 5,
   :scales [{:name "xscale",
             :type "band",
             :domain {:data "table", :field "category"},
             :range "width",
             :padding 0.05,
             :round true}
            {:name "yscale",
             :domain {:data "table", :field "amount"},
             :nice true,
             :range "height"}]
   :axes [{:orient "bottom",
           :scale "xscale"}
          {:orient "left",
           :scale "yscale"}],

   :signals [{:name "tooltip",
              :value {},
              :on [{:events "rect:mouseover", :update "datum"}
                   {:events "rect:mouseout", :update "{}"}]}]
   :marks [{:type "rect",
            :from {:data "table"},
            :encode {:enter {:x {:scale "xscale", :field "category"},
                             :width {:scale "xscale", :band 1},
                             :y {:scale "yscale", :field "amount"},
                             :y2 {:scale "yscale", :value 0}},
                     :update {:fill {:value "steelblue"}},
                     :hover {:fill {:value "red"}}}}
           {:type "text",
            :encode {:enter {:align {:value "center"}, :baseline {:value "bottom"}, :fill {:value "#333"}},
                     :update {:x {:scale "xscale", :signal "tooltip.category", :band 0.5},
                              :y {:scale "yscale", :signal "tooltip.amount", :offset -2},
                              :text {:signal "tooltip.amount"},
                              :fillOpacity [{:test "datum === tooltip", :value 0} {:value 1}]}}}],})


(defn reduction-plans-bar-chart-transforms
  [reduction-plans title-text]
  [(dc/tfn make-more-minimalist
     "Simplifies the initial Vega chart, removing the hovering behaviour."
     [vega-spec]
     (-> vega-spec
       (dissoc :signals)
       (update :marks #(-> % (butlast) (vec)))
       (update-in [:marks 0 :encode] dissoc :hover)))
   (dc/tfn make-horizontal-bar-chart [vega-spec]
     (-> vega-spec
       (update :axes uvega/replace-deep
         {"left" "bottom"
          "bottom" "left"})
       (update :scales uvega/replace-deep
         {"height" "width"
          "width" "height"})
       (uvega/replace-deep {"xscale" "yscale"
                            "yscale" "xscale"})
       (->>
         (sp/transform [:marks sp/ALL :encode sp/MAP-VALS]
           (fn [enc]
             (uvega/replace-deep enc
               {:x :y
                :width :height
                :y :x
                :y2 :x2}))))))
   (dc/tfn add-real-data
     "Gets rid of the fake example data, replacing it with the data we actually want to display."
     [vega-spec]
     (-> vega-spec
       (assoc
         :data
         [{:name "factor_reductions",
           :values (->> reduction-plans
                     (into []
                       (comp
                         (map-indexed
                           (fn [i plan]
                             (->> (:plan_actions plan)
                               (udata/add-cumulated-field
                                 :rdact_previous_cb
                                 :rdact_cb)
                               (mapv
                                 (fn [rdact]
                                   (assoc rdact
                                     :plan_id i
                                     :rdact_next_cb (+ (:rdact_previous_cb rdact) (:rdact_cb rdact))
                                     :rdact_rpct (-> rdact :rdact_cb cB/cB-to-scalar cB/scalar-to-rpct)))))))
                         cat)))}])
       (assoc
         :legends
         [{:title "Reduced factor"
           :fill "colorscale"}])
       (update :scales conj
         (-> {:name "colorscale"
              :type :ordinal
              :domain ["consumption" "intensity"]}
           (uvega/scale_set-discrete-color-scheme ["#55a868" "#4c72b0"])))
       (uvega/replace-deep {"table" "factor_reductions"
                            "category" :plan_id
                            "amount" :rdact_cb})
       (uvega/update-props :marks (uvega/having? {:type "rect"})
         (fn [mark]
           (-> mark
             (assoc :name "reduction_action_bar")
             (supd/supdate
               {:encode
                {:update false
                 :enter (fn [enc]
                          (-> enc
                            (dissoc :x2)
                            (assoc
                              :y {:scale "yscale", :field :plan_id}
                              :x {:scale "xscale", :field :rdact_previous_cb}
                              :x2 {:scale "xscale", :field :rdact_next_cb}
                              :fill {:scale "colorscale" :field :rdact_factor})))}}))))
       (uvega/update-props :scales (uvega/having? {:name "xscale"})
         (fn [scale]
           (merge scale
             {:reverse true
              :domain {:fields [{:data "factor_reductions", :field :rdact_next_cb}
                                {:data "factor_reductions", :field :rdact_previous_cb}]}})))))
   (dc/tfn add-subtext-above-bars
     "Thins out the bars and adds subtext on top of them, describing reduction actions."
     [vega-spec]
     (let [bars-thickness 0.3
           bars-v-padding (/ (- 1 bars-thickness) 2)]
       (-> vega-spec
         (uvega/update-props :marks (uvega/having? {:name "reduction_action_bar"})
           (fn [mark]
             (supd/supdate mark
               {:encode
                {:enter (fn [enc]
                          (-> enc
                            (uvega/hbar_set-padding bars-v-padding)))}})))
         (update :marks conj
           {:name "reduction_action_bar_subtext"
            :type "text",
            :from {:data "factor_reductions"},
            :encode {:update
                     (->
                       {:text {:signal "datum.rdact_description  + ' (' + format(datum.rdact_rpct, '.0f') + '%)'"}
                        :fill {:scale "colorscale" :field :rdact_factor}
                        :fillOpacity {:value 1}
                        :fontStyle {:value :italic}
                        :x {:scale "xscale",
                            :signal "datum.rdact_previous_cb"
                            :offset 2}
                        :align {:value "left"},
                        :y {:scale "yscale",
                            :field :plan_id}}
                       (uvega/text_put-above-hbar bars-v-padding -2))}}))))
   (dc/tfn adjust-dimensions
     "Widens the chart so that subtext can span without collision."
     [vega-spec]
     (assoc vega-spec
       :height (* 60 (count reduction-plans))
       :width 850))
   (dc/tfn clear-y-axis [vega-spec]
     (uvega/update-props vega-spec :axes (uvega/having? {:scale "yscale"})
       (fn [ax]
         (-> ax
           (uvega/ax_hide-labels)
           (uvega/ax_hide-ticks)))))
   (dc/tfn add-titles [vega-spec]
     (-> vega-spec
       (assoc :title title-text)
       (uvega/update-props :axes (uvega/having? {:scale "xscale"})
         assoc :title "Reduction (cB)")))
   (dc/tfn adjust-title [vega-spec]
     (update vega-spec :title
       (fn [title-text]
         (when (some? title-text)
           {:text title-text
            :orient :bottom
            :offset 15}))))
   (dc/tfn add-bars-content
     "Adds text *inside* the bars, this time quantifying the reduction actions in centibels."
     [vega-spec]
     (-> vega-spec
       (update :marks conj
         {:name "reduction_action_bar_content"
          :type "text",
          :from {:data "factor_reductions"},
          :encode {:enter
                   (-> {:text {:signal "format(datum.rdact_cb, '.1f') + ' cB'"}
                        :x {:scale "xscale",
                            :signal "datum.rdact_previous_cb + (datum.rdact_cb / 2)"}
                        :y {:scale "yscale", :field :plan_id},
                        :width {:scale "xscale", :field :rdact_cb}
                        :height {:scale "yscale"}
                        :align {:value "center"},
                        :fill {:value "white"} :fontWeight {:value :normal}
                        :fillOpacity {:value 1}}
                     (uvega/text_center-in-hband))}})))])