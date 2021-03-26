(ns ecolog10.graphics.pct-vs-centibels-reductions
  (:require [ecolog10.centibels-math :as cB]
            [ecolog10.graphics.utils.vega :as uvega]
            [ecolog10.utils.data :as udata]
            [oz.core :as oz]))


(def reduction-actions
  [{:rdact_factor 0.25
    :rdact_factor_name "(Carbon Intensity)"
    :rdact_description "Efficiency: -75% emission factor"
    :rdact_colour "#4c72b0"}
   {:rdact_factor 0.4
    :rdact_factor_name "(Usage)"
    :rdact_description "Sobriety: -60% usage"
    :rdact_colour "#55a868"}])


(def pct-vs-cB-chart
  (let [target-pct (apply * 100.
                     (map :rdact_factor reduction-actions))]
    {:$schema "https://vega.github.io/schema/vega/v5.json",
     :title
     {:text "Classical and centibel-based visualizations of a carbon strategy compounding efficiency and sobriety gains"
      :subtitle "Note that all 3 charts represent exactly the same reduction actions. By using centibels, the 3rd chart is not biased towards a particular factor."
      :orient :bottom
      :offset 15}
     :width 600, :height 400,

     :layout {"padding" 20, "columns" 1, "bounds" "full", "align" "each"}
     :marks
     [{:type :group
       :encode {:update {:x {:value 0}, :x2 {:value 600},
                         :y {:value 0}, :y2 {:value 200}}},
       :layout {"padding" 20, "bounds" "full", "align" "each"}
       :signals
       [{:name "my_width" :init "[0, 300]"}
        {:name "my_height" :init "[0, 200]"}]
       :padding 5
       :marks
       (letfn [(classical-chart [reduction-actions]
                 {:data [{:name "column_names"
                          :values (concat
                                    [{:rdact_factor_name "Initial"}]
                                    (mapv
                                      #(select-keys % [:rdact_factor_name])
                                      reduction-actions)
                                    [{:rdact_factor_name "Target"}])}
                         {:name "reduction_actions"
                          :values
                          (map
                            (fn [ra [start-pct end-pct]]
                              (merge ra
                                {:rdact_start_pct start-pct
                                 :rdact_end_pct end-pct}))
                            reduction-actions
                            (->> reduction-actions
                              (map :rdact_factor)
                              (reductions * 100.)
                              (partition-all 2 1)))}]
                  :axes [{:orient "left",
                          :scale "yscale"
                          :title "Relative GHG emissions (%)"}
                         {:orient "bottom",
                          :scale "xscale"
                          :title nil
                          :encode
                          {
                           :labels                          ;; https://github.com/vega/vega/issues/791#issuecomment-297766635
                           {:update
                            {:text {:signal "datum.label[0] === '(' ? '' : datum.label"}}}
                           :ticks
                           {:update
                            {:opacity {:signal "datum.label[0] === '(' ? 0 : 1"}}}}}],
                  :scales [{:name "xscale",
                            :type "band",
                            :domain {:data "column_names", :field :rdact_factor_name},
                            :range {:signal "my_width"}
                            :padding 0.05,
                            :nice true}
                           {:name "yscale",
                            :domain [0. 100.]
                            :range {:signal "my_height",}
                            :padding 0,
                            :reverse true
                            :round true}],
                  :padding 5,
                  :marks
                  [{:name "bar_initial"
                    :type "rect",
                    :encode {:enter {:x {:scale "xscale", :value "Initial", #_#_:band bars-v-padding},
                                     :width {:scale "xscale", :band 1.}
                                     :y {:scale "yscale", :value 0}
                                     :y2 {:scale "yscale", :value 100.}},
                             :update {:fill {:value "#bbbbbb"}}}}

                   {:name "bar_target"
                    :type "rect",
                    :encode {:enter {:x {:scale "xscale", :value "Target", #_#_:band bars-v-padding},
                                     :width {:scale "xscale", :band 1.}
                                     :y {:scale "yscale", :value 0.}
                                     :y2 {:scale "yscale", :value target-pct}},
                             :update {:fill {:value "#dd8462"}}}}
                   {:name "bars_reduction_actions"
                    :type "rect"
                    :from {:data "reduction_actions"}
                    :encode {:enter {:x {:scale "xscale", :field :rdact_factor_name, #_#_:band bars-v-padding},
                                     :width {:scale "xscale", :band 1.}
                                     :y {:scale "yscale", :field :rdact_start_pct}
                                     :y2 {:scale "yscale", :field :rdact_end_pct}},
                             :update {:fill {:field :rdact_colour}}}}
                   {:name "txt_reduction_actions"
                    :type "text"
                    :from {:data "reduction_actions"}
                    :encode {:enter {:align {:value "left"},
                                     :baseline {:value "middle"},
                                     :fontStyle {:value :italic}
                                     :fill {:field :rdact_colour}},
                             :update
                             {:y {:scale "yscale",
                                  :signal "0.5 * (datum.rdact_start_pct + datum.rdact_end_pct)",
                                  :offset 2},
                              :x {:scale "xscale",
                                  :field :rdact_factor_name
                                  :band 1.
                                  :offset 2},
                              :text {:field :rdact_description},
                              :fillOpacity {:value 1}}}}]})]
         [(merge
            {:type :group
             :encode {:update {:x {:value 0}, :x2 {:value 300},
                               :y {:value 0}, :y2 {:value 200}}},
             :padding 5}
            (classical-chart reduction-actions))
          (merge
            {:type :group
             :encode {:update {:x {:value 300}, :x2 {:value 600},
                               :y {:value 0}, :y2 {:value 200}}},
             :padding 5}
            (classical-chart (reverse reduction-actions)))])}
      (let [target-cB (-> (- target-pct 100.) cB/rpct-to-scalar cB/scalar-to-cB)
            reduction-actions1
            (->> reduction-actions
              (mapv
                (fn [ra]
                  (assoc ra
                    :rdact_cb (-> ra :rdact_factor cB/scalar-to-cB))))
              (udata/add-cumulated-field :rdact_previous_cb :rdact_cb))]
        {:type :group
         :encode {:update {:x {:value 0}, :x2 {:value 600},
                           :y {:value 200}, :y2 {:value 400}}},
         :layout {"padding" 20, "bounds" "full", "align" "each"}
         :signals
         [{:name "my_width" :init "[0, 600]"}
          {:name "my_height" :init "[0, 200]"}]

         :data [{:name "target_reduction"
                 :values [{:rdact_cb target-cB
                           :rdact_factor "Target"}]}
                {:name "factors"
                 :values (concat
                           [{:rdact_factor_name "Target"}]
                           (mapv
                             #(select-keys % [:rdact_factor_name])
                             reduction-actions))}
                {:name "reduction_actions"
                 :values reduction-actions1}
                {:name "joining_lines"
                 :values (concat
                           (->> reduction-actions1
                             (partition 2 1)
                             (mapv
                               (fn [[r1 r2]]
                                 {:y1 (:rdact_factor_name r1)
                                  :y2 (:rdact_factor_name r2)
                                  :x (:rdact_previous_cb r2)})))
                           (when (= (double target-cB)
                                   (apply + 0.
                                     (map :rdact_cb
                                       reduction-actions1)))
                             [{:y1 "Target"
                               :y2 (:rdact_factor_name (last reduction-actions1))
                               :x target-cB}]))}]
         :axes [(-> {:orient "left", :scale "yscale"}
                  (uvega/ax_hide-ticks)
                  (uvega/ax_hide-labels))
                {:orient "bottom",
                 :scale "xscale"
                 :title "GHG emissions reduction (cB)"}],
         :scales [{:name "yscale",
                   :type "band",
                   :domain {:data "factors", :field :rdact_factor_name},
                   :range {:signal "my_height"}
                   :nice true,}
                  {:name "xscale",
                   :domain [target-cB 0]
                   :range {:signal "my_width"},
                   :reverse true
                   :round true}],
         :padding 5,
         :marks
         (let [bars-thickness 0.3
               bars-v-padding (/ (- 1 bars-thickness) 2)]
           [{:name "target_bar"
             :type "rect",
             :from {:data "target_reduction"}
             :encode {:enter (->
                               {:y {:scale "yscale", :value "Target"}
                                :x2 {:scale "xscale", :field :rdact_cb}
                                :x {:scale "xscale", :value 0}}
                               (uvega/hbar_set-padding bars-v-padding))
                      :update {:fill {:value "#dd8462"}}}}
            {:name "target_bar_content"
             :type "text",
             :from {:data "target_reduction"},
             :encode {:enter (-> {:text {:signal "'Target: ' + format(datum.rdact_cb, '.1f') + ' cB'"}
                                  :x {:scale "xscale", :signal "datum.rdact_cb / 2"}
                                  :y {:scale "yscale", :field :rdact_factor},
                                  :align {:value "center"},
                                  :fill {:value "white"} :fontWeight {:value :bold} :fontStyle {:value :italic}
                                  :fillOpacity {:value 1}}
                               (uvega/text_center-in-hband))}}
            {:name "reduction_action_bar"
             :type "rect",
             :from {:data "reduction_actions"},
             :encode {:enter (-> {:y {:scale "yscale", :field :rdact_factor_name}
                                  :x2 {:scale "xscale", :signal "datum.rdact_previous_cb + datum.rdact_cb"}
                                  :x {:scale "xscale", :field :rdact_previous_cb}}
                               (uvega/hbar_set-padding bars-v-padding))
                      :update {:fill {:field :rdact_colour}},}}
            {:name "reduction_action_bar_content"
             :type "text",
             :from {:data "reduction_actions"},
             :encode {:enter (-> {:text {:signal "format(datum.rdact_cb, '.1f') + ' cB'"}
                                  :x {:scale "xscale",
                                      :signal "datum.rdact_previous_cb + (datum.rdact_cb / 2)"}
                                  :y {:scale "yscale", :field :rdact_factor_name},
                                  :align {:value "center"},
                                  :fill {:value "white"}
                                  :fillOpacity {:value 1}}
                               (uvega/text_center-in-hband))}}
            {:name "reduction_action_bar_subtext"
             :type "text",
             :from {:data "reduction_actions"},
             :encode {:update
                      (-> {:text {:field :rdact_description},
                           :fontStyle {:value :italic}
                           :fill {:field :rdact_colour},
                           :fillOpacity {:value 1}
                           :x {:scale "xscale",
                               :signal "datum.rdact_previous_cb + datum.rdact_cb"
                               :offset -2}
                           :align {:value "right"}
                           :y {:scale "yscale", :field :rdact_factor_name},}
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
                       :opacity {:value 0.5}}}}])})]}))


(defn get-vega-chart
  []
  [:vega pct-vs-cB-chart])


;(oz/view! (get-vega-chart))





(comment ;;;; Concatenating in Vega

  ;; First attempt, does not work as both charts overlap badly,
  ;; probably because the scales both use the overall width and height
  (def some-chart
    (let []
      (merge
        {:$schema "https://vega.github.io/schema/vega/v5.json",
         :description "A basic bar chart example, with value labels shown upon mouse hover.",
         :width 400, :height 400
         :marks
         (for [i [0 1]]
           (let [xscale "xscale"
                 yscale "yscale"
                 table "table"]
             (merge
               {:type :group
                :encode
                {:update
                 {:x {:value 0}
                  :x2 {:value 400}
                  :y {:value (* i 200)}
                  :y2 {:value (* (inc i)
                                200)}}}}
               {:padding 5,
                :data [{:name table,
                        :values [{:category "A", :amount 28}
                                 {:category "B", :amount 55}
                                 {:category "C", :amount 43}
                                 {:category "D", :amount 91}
                                 {:category "E", :amount 81}
                                 {:category "F", :amount 53}
                                 {:category "G", :amount 19}
                                 {:category "H", :amount 87}]}]
                :scales [{:name xscale
                          :type "band",
                          :domain {:data table, :field "category"},
                          :range "width"
                          :padding 0.05,
                          :round true}
                         {:name yscale,
                          :domain {:data table, :field "amount"},
                          :nice true,
                          :range "height"}]                 ;[(* i 200), (* (inc i) 200)]}]
                :axes [{:orient "bottom", :scale xscale}
                       {:orient "left", :scale yscale}],
                :marks [{:type "rect",
                         :from {:data table},
                         :encode {:enter {:x {:scale xscale, :field "category"},
                                          :width {:scale xscale, :band 1},
                                          :y2 {:scale yscale, :field "amount"},
                                          :y {:scale yscale, :value 0}},
                                  :update {:fill {:value "steelblue"}},}}]})))})))


  ;;;; Achieves horizontal concatenation
  (def some-chart
    (let []
      (merge
        {:$schema "https://vega.github.io/schema/vega/v5.json",
         :description "A basic bar chart example, with value labels shown upon mouse hover.",
         :width 400, :height 400
         ;:autosize :none
         :layout
         {"padding" 20,
          ;"columns" 1,
          "bounds" "full",
          "align" "each"},
         :marks
         (for [i [0 1]]
           (let [xscale "xscale"
                 yscale "yscale"
                 table "table"]
             (merge
               {:type :group
                :encode
                {:update
                 {:x {:value 0}
                  :x2 {:value 400}
                  :y {:value (* i 200)}
                  :y2 {:value (* (inc i)
                                200)}}}}
               {:padding 5,
                :data [{:name table,
                        :values [{:category "A", :amount 28}
                                 {:category "B", :amount 55}
                                 {:category "C", :amount 43}
                                 {:category "D", :amount 91}
                                 {:category "E", :amount 81}
                                 {:category "F", :amount 53}
                                 {:category "G", :amount 19}
                                 {:category "H", :amount 87}]}]
                :scales [{:name xscale
                          :type "band",
                          :domain {:data table, :field "category"},
                          :range "width"
                          :padding 0.05,
                          :round true}
                         {:name yscale,
                          :domain {:data table, :field "amount"},
                          :nice true,
                          :range [200 0]}]
                :axes [{:orient "bottom", :scale xscale}
                       {:orient "left", :scale yscale}],
                :marks [{:type "rect",
                         :from {:data table},
                         :encode {:enter {:x {:scale xscale, :field "category"},
                                          :width {:scale xscale, :band 1},
                                          :y2 {:scale yscale, :field "amount"},
                                          :y {:scale yscale, :value 0}},
                                  :update {:fill {:value "steelblue"}}}}]})))})))


  ;;;; Achieves vertical concatenation
  (def some-chart
    (let []
      (merge
        {:$schema "https://vega.github.io/schema/vega/v5.json",
         :description "A basic bar chart example, with value labels shown upon mouse hover.",
         :width 400, :height 400
         :layout
         {"padding" 20,
          "columns" 1, ;; <-- here's what's changed compared to the above
          "bounds" "full",
          "align" "each"},
         :marks
         (for [i [0 1]]
           (let [xscale "xscale"
                 yscale "yscale"
                 table "table"]
             (merge
               {:type :group
                :encode
                {:update
                 {:x {:value 0}
                  :x2 {:value 400}
                  :y {:value (* i 200)}
                  :y2 {:value (* (inc i)
                                200)}}}}
               {:padding 5,
                :data [{:name table,
                        :values [{:category "A", :amount 28}
                                 {:category "B", :amount 55}
                                 {:category "C", :amount 43}
                                 {:category "D", :amount 91}
                                 {:category "E", :amount 81}
                                 {:category "F", :amount 53}
                                 {:category "G", :amount 19}
                                 {:category "H", :amount 87}]}]
                :scales [{:name xscale
                          :type "band",
                          :domain {:data table, :field "category"},
                          :range "width"
                          :padding 0.05,
                          :round true}
                         {:name yscale,
                          :domain {:data table, :field "amount"},
                          :nice true,
                          :range [200 0]}]                  ;[(* i 200), (* (inc i) 200)]}]
                :axes [{:orient "bottom", :scale xscale}
                       {:orient "left", :scale yscale}],
                :marks [{:type "rect",
                         :from {:data table},
                         :encode {:enter {:x {:scale xscale, :field "category"},
                                          :width {:scale xscale, :band 1},
                                          :y2 {:scale yscale, :field "amount"},
                                          :y {:scale yscale, :value 0}},
                                  :update {:fill {:value "steelblue"}},}}]})))})))

  ;;;; Mix of vertical and horizontal concatenation
  (def some-chart
    (letfn [(base-bar-chart [xscale-range yscale-range]
              {:padding 5,
               :data [{:name "table",
                       :values [{:category "A", :amount 28}
                                {:category "B", :amount 55}
                                {:category "C", :amount 43}
                                {:category "D", :amount 91}
                                {:category "E", :amount 81}
                                {:category "F", :amount 53}
                                {:category "G", :amount 19}
                                {:category "H", :amount 87}]}],
               :scales [{:name "xscale",
                         :type "band",
                         :domain {:data "table", :field "category"},
                         :range xscale-range
                         :padding 0.05,
                         :round true}
                        {:name "yscale",
                         :domain {:data "table", :field "amount"},
                         :nice true,
                         :range yscale-range}],
               :axes [{:orient "bottom", :scale "xscale"} {:orient "left", :scale "yscale"}],
               :marks [{:type "rect",
                        :from {:data "table"},
                        :encode {:enter {:x {:scale "xscale", :field "category"},
                                         :width {:scale "xscale", :band 1},
                                         :y2 {:scale "yscale", :field "amount"},
                                         :y {:scale "yscale", :value 0}},
                                 :update {:fill {:value "steelblue"}}}}]})]
      {:$schema "https://vega.github.io/schema/vega/v5.json",
       :description "A basic bar chart example, with value labels shown upon mouse hover.",
       :width 600,
       :height 400,
       :layout {"padding" 20, "columns" 1, "bounds" "full", "align" "each"},
       :marks [{:type :group,
                :encode {:update {:x {:value 0}, :x2 {:value 600},
                                  :y {:value 0}, :y2 {:value 200}}},
                :padding 5,
                :layout {"padding" 20, "bounds" "full", "align" "each"},
                :signals {:name "my_width" :value [0 300], :init "[0, 300]"}
                :marks [(merge
                          {:type :group,
                           :encode {:update {:x {:value 0}, :x2 {:value 300},
                                             :y {:value 0}, :y2 {:value 200}}},}
                          (base-bar-chart {:signal "my_width"} [200 0]))
                        (merge
                          {:type :group,
                           :encode {:update {:x {:value 300}, :x2 {:value 600},
                                             :y {:value 0}, :y2 {:value 200}}},}
                          (base-bar-chart [0 300] [200 0]))]}

               (merge
                 {:type :group,
                  :encode {:update {:x {:value 0}, :x2 {:value 600},
                                    :y {:value 200}, :y2 {:value 400}}}}
                 (base-bar-chart "width" [200 0]))]}))


  (oz/view! [:vega some-chart])

  *e)
