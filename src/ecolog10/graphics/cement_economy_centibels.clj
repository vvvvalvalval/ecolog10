(ns ecolog10.graphics.cement-economy-centibels
  (:require [ecolog10.centibels-math :as cB]
            [ecolog10.graphics.utils.vega :as uvega]
            [ecolog10.utils.data :as udata]
            [oz.core :as oz]))


(def cement-target-cb -100)


(def factors
  [{:rdact_factor_name "CO₂ Intensity"}
   {:rdact_factor_name "Cement density"}
   {:rdact_factor_name "Usage"}])


(def cement-reductions
  (->
    [{:rdact_description "Carbon capture technology"
      :rdact_cb (-> -70 cB/rpct-to-scalar cB/scalar-to-cB)
      :rdact_factor "CO₂ Intensity"}
     {:rdact_description "Leaner architectural designs"
      :rdact_cb (-> -30 cB/rpct-to-scalar cB/scalar-to-cB)
      :rdact_factor "Cement density"}]
    (as-> reds
      (conj reds
        {:rdact_description "Frugality: building less or smaller"
         :rdact_cb (- cement-target-cb
                     (apply +
                       (map :rdact_cb
                         reds)))
         :rdact_colour "#55a868"
         :rdact_factor "Usage"}))))


(def cement-reductions-chart
  (let [bars-thickness 0.3
        bars-v-padding (/ (- 1 bars-thickness) 2)

        factor_reductions
        (->> cement-reductions
          (map
            (fn add-rcpt [r]
              (assoc r
                :rdact_rpct
                (-> r :rdact_cb cB/cB-to-scalar cB/scalar-to-rpct))))
          (udata/add-cumulated-field :rdact_previous_cb :rdact_cb))]
    ;; colours from: http://seaborn.pydata.org/tutorial/color_palettes.html (Default Seaborn palette)
    {:$schema "https://vega.github.io/schema/vega/v5.json",
     :title {:text "An allocation of reduction actions for cement production"
             :orient :bottom
             :offset 15}
     :width 500, :height 250,
     :data [{:name "factors"
             :values (concat
                       [{:rdact_factor_name "Target"}]
                       factors)}
            {:name "target_reduction"
             :values [{:rdact_description "Total reduction in CO₂ emissions",
                       :rdact_cb cement-target-cb,
                       :rdact_rpct (-> cement-target-cb cB/cB-to-scalar cB/scalar-to-rpct)
                       :rdact_factor "Target"}]}
            {:name "factor_reductions",
             :values factor_reductions}
            {:name "joining_lines"
             :values (concat
                       (->> factor_reductions
                         (partition 2 1)
                         (mapv
                           (fn [[r1 r2]]
                             {:y1 (:rdact_factor r1)
                              :y2 (:rdact_factor r2)
                              :x (:rdact_previous_cb r2)})))
                       (when (= (double cement-target-cb)
                               (apply + 0.
                                 (map :rdact_cb
                                   cement-reductions)))
                         [{:y1 "Target"
                           :y2 (:rdact_factor (last cement-reductions))
                           :x cement-target-cb}]))}],
     ;:signals [{:name "tooltip",
     ;           :value {},
     ;           :on [{:events "rect:mouseover", :update "datum"} {:events "rect:mouseout", :update "{}"}]}]
     :axes [{:orient "left",
             :scale "yscale"
             :title "Factors"
             :encode {:labels                               ;; https://github.com/vega/vega/issues/791#issuecomment-297766635
                      {:update
                       {:text {:signal "datum.label === 'Target' ? '' : datum.label"}
                        :fontWeight {:signal "datum.label === 'Target' ? 'bold' : 'normal'"}}}
                      :ticks
                      {:update
                       {:opacity {:signal "datum.label === 'Target' ? 0 : 1"}}}}}
            {:orient "bottom",
             :scale "xscale"
             :title "Contributions (cB)"}],
     :scales [{:name "yscale",
               :type "band",
               :domain {:data "factors", :field :rdact_factor_name},
               :range "height",
               :nice true}
              {:name "xscale",
               :domain [cement-target-cb 0]
               :range "width",
               ;:padding 0.05,
               :reverse true
               :round true}],
     :padding 5,
     :marks [{:name "target_bar"
              :type "rect",
              :from {:data "target_reduction"},
              :encode {:enter
                       (->
                         {:x {:scale "xscale", :value 0}
                          :width {:scale "xscale", :field :rdact_cb}
                          :y {:scale "yscale", :field :rdact_factor}}
                         (uvega/hbar_set-padding bars-v-padding))
                       :update {:fill {:value "#dd8462"}},
                       :hover {:fill {:value "#c44e52"}}}}
             {:name "target_bar_content"
              :type "text",
              :from {:data "target_reduction"},
              :encode {:enter (-> {:text {:signal "'Target: ' + format(datum.rdact_cb, '.1f') + ' cB'"}
                                   :fill {:value "white"} :fontWeight {:value :bold} :fontStyle {:value :italic}
                                   :fillOpacity {:value 1}
                                   :x {:scale "xscale", :signal "datum.rdact_cb / 2"}
                                   :align {:value "center"}
                                   :y {:scale "yscale", :field :rdact_factor}}
                                (uvega/text_center-in-hband))}}
             {:name "target_bar_subtext"
              :type "text",
              :from {:data "target_reduction"},
              :encode {:update
                       (->
                         {:align {:value "right"},
                          :fontStyle {:value :italic}
                          :fill {:value "#dd8462"}
                          :y {:scale "yscale", :field :rdact_factor},
                          :x {:scale "xscale",
                              :field :rdact_cb
                              :offset -2},
                          :text {:signal "datum.rdact_description  + ' (' + format(datum.rdact_rpct, '.0f') + '%)'"},
                          :fillOpacity {:value 1}}
                         (uvega/text_put-below-hbar bars-v-padding 2))}}

             {:name "reduction_action_bar"
              :type "rect",
              :from {:data "factor_reductions"},
              :encode {:enter (->
                                {:y {:scale "yscale", :field :rdact_factor},
                                 :width {:scale "xscale", :field :rdact_cb}
                                 :x {:scale "xscale", :field :rdact_previous_cb}}
                                (uvega/hbar_set-padding bars-v-padding))
                       :update {:fill {:signal "datum.rdact_colour || '#4c72b0'"}},
                       :hover {:fill {:value "#c44e52"}}}}
             {:name "reduction_action_bar_content"
              :type "text",
              :from {:data "factor_reductions"},
              :encode {:enter (-> {:text {:signal "format(datum.rdact_cb, '.1f') + ' cB'"}
                                   :x {:scale "xscale",
                                       :signal "datum.rdact_previous_cb + (datum.rdact_cb / 2)"}
                                   :align {:value :center}
                                   :y {:scale "yscale", :field :rdact_factor},
                                   :fill {:value "white"} :fontWeight {:value :normal #_:bold}
                                   :fillOpacity {:value 1}}
                                (uvega/text_center-in-hband))}},
             {:name "reduction_action_bar_subtext"
              :type "text",
              :from {:data "factor_reductions"},
              :encode {:update
                       (->
                         {:align {:value "right"},
                          :y {:scale "yscale", :field :rdact_factor},
                          :x {:scale "xscale",
                              :signal "datum.rdact_previous_cb + datum.rdact_cb"
                              :offset -2},
                          :text {:signal "datum.rdact_description  + ' (' + format(datum.rdact_rpct, '.0f') + '%)'"},
                          :fontStyle {:value :italic}
                          :fill {:signal "datum.rdact_colour || '#4c72b0'"}
                          :fillOpacity {:value 1}}
                         (uvega/text_put-below-hbar bars-v-padding 2))}}

             {:name "line_joining_bars"
              :type "rule"
              :from {:data "joining_lines"}
              :encode {:update
                       {:zindex {:value -10}
                        :x {:scale "xscale" :field :x}
                        :x2 {:scale "xscale" :field :x}
                        :y {:scale "yscale" :field :y1, :band bars-v-padding}
                        :y2 {:scale "yscale" :field :y2, :band (- 1 bars-v-padding)}
                        :strokeDash {:value [4 2]}
                        :strokeWidth {:value 0.5}
                        :opacity {:value 0.5}}}}]}))

(defn get-vega-chart
  []
  [:vega cement-reductions-chart])

;(oz/view! cement-reductions-chart)