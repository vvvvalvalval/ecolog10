(ns centibel-sobriety.core
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [oz.core :as oz])
  (:import (java.util GregorianCalendar)))


(defn rpct-to-scalar
  [rel-pct]
  (+ 1
    (/ rel-pct 100)))


(defn scalar-to-cB
  [scalar]
  (* 100.
    (Math/log10 scalar)))


(defn cB-to-scalar
  [cb]
  (Math/pow 10. (/ cb 100.)))


(defn scalar-to-rpct
  [scalar]
  (* 100
    (- scalar 1)))


(comment

  (-> -50. rpct-to-scalar scalar-to-cB
    (/ 30)
    cB-to-scalar
    scalar-to-rpct)

  (-> -5.7 rpct-to-scalar scalar-to-cB)

  (ratio?)
  (println
    (->> ["900" 400 300 200 "100"
          "50" 33 25
          20 15 10 5
          4 3 2 "1"
          "0"
          "-1" -2 -3 -4
          -5 -10 -15 -20 -25 -30
          -40 "-50"
          -60 -65 -70 "-75" -80 -85 "-90"
          "-95" -97 -98 "-99"]
      (map
        (fn [n]
          (let [[rel-pct emphasis?]
                (if (string? n)
                  [(read-string n) true]
                  [n false])
                sf (rpct-to-scalar rel-pct)
                cb (scalar-to-cB sf)]
            [emphasis?
             (if (integer? rel-pct)
               (format "%+d%%" rel-pct)
               (format "%+.1f%%" (double rel-pct)))
             (if (integer? sf)
               (format "×%d" sf)
               (format "×%.2f" (double sf)))
             (format "%+.2f cB" cb)])))
      (map
        (fn [[emphasis? s1 s2 s3]]
          (->> [s1 s2 s3]
            (mapv
              (fn [s]
                (str "|"
                  (if emphasis?
                    (str "*" s "*")
                    s)))))))
      (interpose [""])
      (apply concat)
      (str/join "\n")))

  *e)


(comment

  ;; Drawing percentages on a logarithmic scale


  (def +cB->+x (* -14.99 1e-2))

  (->> (range -0.9 2 0.1)
    (map
      (fn [+pct]
        (+ 1 +pct)))
    (map
      (fn [r]
        (* 1e2
          (Math/log10 r))))
    (map
      (fn [+cB]
        (* +cB->+x
          +cB)))
    (map
      (fn [+x]
        (+ -8.04 +x))))


  (def variations
    [2/3 3/4 (Math/pow 8/10 2)])

  (->> variations
    (mapv
      scalar-to-cB))

  (->> *1
    (mapv
      #(* % +cB->+x)))

  (- -100 (apply + *1))

  (-> *1 cB-to-scalar scalar-to-rpct)

  *e)

;; ------------------------------------------------------------------------------
;; Oz dummy example

(defn play-data [& names]
  (for [n names
        i (range 20)]
    {:time i :item n :quantity (+ (Math/pow (* i (count n)) 0.8) (rand-int (count n)))}))

(def line-plot
  {:data {:values (play-data "monkey" "slipper" "broom")}
   :encoding {:x {:field "time" :type "quantitative"}
              :y {:field "quantity" :type "quantitative"}
              :color {:field "item" :type "nominal"}}
   :mark "line"})


(comment
  (oz/view! line-plot)
  *e)


;; ------------------------------------------------------------------------------
;; Vega Bar Chart example
;; https://vega.github.io/vega/tutorials/bar-chart/

(def bar-chart-example
  {:axes [{:orient "bottom", :scale "xscale"} {:orient "left", :scale "yscale"}],
   :width 400, :height 200,
   :scales [{:name "xscale",
             :type "band",
             :domain {:data "table", :field "category"},
             :range "width",
             :padding 0.05,
             :round true}
            {:name "yscale",
             :domain {:data "table", :field "amount"},
             :nice true,
             :range "height"}],
   :padding 5,
   :marks [{:type "rect",
            :from {:data "table"},
            :encode {:enter {:x {:scale "xscale", :field "category", :band 0.25},
                             :width {:scale "xscale", :band 0.5},
                             :y {:scale "yscale", :field "amount"},
                             :y2 {:scale "yscale", :value 0}},
                     :update {:fill {:value "steelblue"}},
                     :hover {:fill {:value "red"}}}}
           {:type "text",
            :encode {:enter {:align {:value "center"},
                             :baseline {:value "bottom"},
                             :fill {:value "#333"}},
                     :update
                     {:x {:scale "xscale", :signal "tooltip.category", :band 0.5},
                      :y {:scale "yscale", :signal "tooltip.amount", :offset -2},
                      :text {:signal "tooltip.amount"},
                      :fillOpacity [{:test "isNaN(tooltip.amount)", :value 0} {:value 1}]}}}],
   :$schema "https://vega.github.io/schema/vega/v5.json",
   :signals [{:name "tooltip",
              :value {},
              :on [{:events "rect:mouseover", :update "datum"} {:events "rect:mouseout", :update "{}"}]}],
   :data [{:name "table",
           :values [{:category "A", :amount 28}
                    {:category "B", :amount 55}
                    {:category "C", :amount 43}
                    {:category "D", :amount 91}
                    {:category "E", :amount 81}
                    {:category "F", :amount 53}
                    {:category "G", :amount 19}
                    {:category "H", :amount 87}]}]})


(comment

  (oz/view! [:vega bar-chart-example])

  *e)


;; ------------------------------------------------------------------------------
;; Cement CO2 emissions


(def cement-target-cb -100)


(def factors
  [{:rdact_factor_name "CO₂ Intensity"}
   {:rdact_factor_name "Cement density"}
   {:rdact_factor_name "Usage"}])


(def cement-reductions
  (->
    [{:rdact_description "Carbon capture technology"
      :rdact_cb (-> -70 rpct-to-scalar scalar-to-cB)
      :rdact_factor "CO₂ Intensity"}
     {:rdact_description "Leaner architectural designs"
      :rdact_cb (-> -30 rpct-to-scalar scalar-to-cB)
      :rdact_factor "Cement density"}]
    (as-> reds
      (conj reds
        {:rdact_description "Sobriety: building less or smaller"
         :rdact_cb (- cement-target-cb
                     (apply +
                       (map :rdact_cb
                         reds)))
         :rdact_colour "#55a868"
         :rdact_factor "Usage"}))))


(defn add-cumulated-field
  [cum-field-name field ms]
  (vec
    (reductions
      (fn [acc m]
        (assoc m
          cum-field-name
          (+
            (get acc cum-field-name)
            (field acc))))
      (assoc (first ms) cum-field-name 0)
      (rest ms))))


(def cement-reductions-chart
  (let [bars-thickness 0.3
        bars-v-padding (/ (- 1 bars-thickness) 2)

        factor_reductions
        (->> cement-reductions
          (map
            (fn add-rcpt [r]
              (assoc r
                :rdact_rpct
                (-> r :rdact_cb cB-to-scalar scalar-to-rpct))))
          (add-cumulated-field :rdact_previous_cb :rdact_cb))]
    ;; colours from: http://seaborn.pydata.org/tutorial/color_palettes.html (Default Seaborn palette)
    {:title
     {:text "An allocation of reduction actions for cement production"
      :orient :bottom
      :offset 15}
     :$schema "https://vega.github.io/schema/vega/v5.json",
     :width 500, :height 250,
     :data [{:name "factors"
             :values (concat
                       [{:rdact_factor_name "Target"}]
                       factors)}
            {:name "target_reduction"
             :values [{:rdact_description "Total reduction in CO₂ emissions",
                       :rdact_cb cement-target-cb,
                       :rdact_rpct (-> cement-target-cb cB-to-scalar scalar-to-rpct)
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
               :nice true,}
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
              :encode {:enter {:y {:scale "yscale", :field :rdact_factor, :band bars-v-padding},
                               :height {:scale "yscale", :band bars-thickness}
                               :width {:scale "xscale", :field :rdact_cb}
                               :x {:scale "xscale", :value 0}},
                       :update {:fill {:value "#dd8462"}},
                       :hover {:fill {:value "#c44e52"}}}}
             {:name "target_bar_content"
              :type "text",
              :from {:data "target_reduction"},
              :encode {:enter {:text {:signal "'Target: ' + format(datum.rdact_cb, '.1f') + ' cB'"}
                               :x {:scale "xscale", :signal "datum.rdact_cb / 2"}
                               :y {:scale "yscale", :field :rdact_factor, :band (+ bars-v-padding (/ bars-thickness 2))},
                               :width {:scale "xscale", :field :rdact_cb}
                               :height {:scale "yscale", :band bars-thickness}
                               :align {:value "center"},
                               :baseline {:value "middle"},
                               :fill {:value "white"} :fontWeight {:value :bold} :fontStyle {:value :italic}
                               :fillOpacity {:value 1}}}}
             {:name "target_bar_subtext"
              :type "text",
              :from {:data "target_reduction"},
              :encode {:enter {:align {:value "right"},
                               :baseline {:value "top"},
                               :fontStyle {:value :italic}
                               :fill {:value "#dd8462"}},
                       :update
                       {:y {:scale "yscale",
                            :field :rdact_factor,
                            :band (+ bars-v-padding bars-thickness)              ;; for centering
                            :offset 2},
                        :x {:scale "xscale",
                            :field :rdact_cb
                            :offset -2},
                        :text {:signal "datum.rdact_description  + ' (' + format(datum.rdact_rpct, '.0f') + '%)'"},
                        :fillOpacity {:value 1}}}}

             {:name "reduction_action_bar"
              :type "rect",
              :from {:data "factor_reductions"},
              :encode {:enter {:y {:scale "yscale", :field :rdact_factor, :band bars-v-padding},
                               :height {:scale "yscale", :band bars-thickness}
                               :width {:scale "xscale", :field :rdact_cb}
                               :x {:scale "xscale", :field :rdact_previous_cb}},
                       :update {:fill {:signal "datum.rdact_colour || '#4c72b0'"}},
                       :hover {:fill {:value "#c44e52"}}}}
             {:name "reduction_action_bar_content"
              :type "text",
              :from {:data "factor_reductions"},
              :encode {:enter {:text {:signal "format(datum.rdact_cb, '.1f') + ' cB'"}
                               :x {:scale "xscale",
                                   :signal "datum.rdact_previous_cb + (datum.rdact_cb / 2)"}
                               :y {:scale "yscale", :field :rdact_factor, :band (+ bars-v-padding (/ bars-thickness 2))},
                               :width {:scale "xscale", :field :rdact_cb}
                               :height {:scale "yscale", :band bars-thickness}
                               :align {:value "center"},
                               :baseline {:value "middle"},
                               :fill {:value "white"} :fontWeight {:value :normal #_:bold}
                               :fillOpacity {:value 1}},}}
             {:name "reduction_action_bar_subtext"
              :type "text",
              :from {:data "factor_reductions"},
              :encode {:enter {:align {:value "right"},
                               :baseline {:value "top"},
                               :fontStyle {:value :italic}
                               :fill {:signal "datum.rdact_colour || '#4c72b0'"}},
                       :update
                       {:y {:scale "yscale",
                            :field :rdact_factor,
                            :band (+ bars-v-padding bars-thickness)              ;; for centering
                            :offset 2},
                        :x {:scale "xscale",
                            :signal "datum.rdact_previous_cb + datum.rdact_cb"
                            :offset -2},
                        :text {:signal "datum.rdact_description  + ' (' + format(datum.rdact_rpct, '.0f') + '%)'"},
                        :fillOpacity {:value 1}}}}

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


(comment

  (oz/view! [:vega cement-reductions-chart])

  *e)


;; ------------------------------------------------------------------------------
;; Exponential decay pathways

(def remaining-carbon-budget
  646.5)


(def yearly-emissions-2019
  42.1)


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


(defn year->temporal
  [y]
  (letfn [(year-epoch [y]
            (-> (GregorianCalendar.
                  (int y) 0 1)
              .getTime
              .getTime))]
    (let [flr (year-epoch (Math/floor y))
          ceil (year-epoch (Math/ceil y))
          frac-part (- y (Math/floor y))]
      (-> (+
            flr
            (*
              frac-part
              (- ceil flr)))
        (double)
        (Math/round)
        (long)))))

(comment
  (year->temporal 2019)
  (year->temporal 2019.5)
  (year->temporal 2020)

  (->> (range 2019 2045.01 0.25)
    (map year->temporal)
    (mapv (fn [t]
            (new java.util.Date t))))

  *e)


(def exp-decay-chart
  (let [ref-year 2020.7
        limit-year (+ ref-year (/ remaining-carbon-budget yearly-emissions-2019))
        redstart-years [2021
                        2026
                        2031]]
    ;; FIXME vconcat emissions curve
    ;; FIXME
    {:$schema "https://vega.github.io/schema/vega-lite/v4.json",
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
       [{:data {:values [{:t_0 (year->temporal ref-year)
                          :t_l (year->temporal limit-year)
                          :zero_emissions 0.
                          :init_ghg_emissions yearly-emissions-2019
                          :rem_carbon_budget remaining-carbon-budget}]}
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
                                       exp-decay-limit-year-target-cB
                                       (double
                                         (/
                                           (- t rstart-y)
                                           (- limit-year rstart-y)))))]]
                  {:t (year->temporal t)
                   ;; FIXME I don't get why this displays so poorly
                   :yearly_ghg_emissions
                   (-> cb-red
                     (cB-to-scalar)
                     (* yearly-emissions-2019))

                   :red_start_year rstart-y})}
         :mark {:type :line
                :interpolate :monotone}
         :encoding {:x {:field :t, :type "temporal",
                        :title "Time"}
                    :y {:field :yearly_ghg_emissions, :type "quantitative"
                        :title "CO₂ emissions (GtCO₂e/year)"}
                    :color {:field :red_start_year, :type "nominal"
                            :legend {:title "Reductions start year"}}}}
        {:data {:values [{:t (year->temporal limit-year)
                          :init_ghg_emissions yearly-emissions-2019
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
                      :strokeDash {:value [3 2]}}}
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
                                     (* exp-decay-limit-year-target-cB
                                       (double
                                         (/
                                           (- t rstart-y)
                                           (- limit-year rstart-y)))))]]
                  {:t (year->temporal t)
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
                            :legend {:title "Reductions start year"}}}}
        {:data {:values
                (for [rstart-y (reverse redstart-years)]
                  (let [t 2045]
                    {:t (year->temporal t)
                     :red_start_year rstart-y
                     :cB_red
                     (* exp-decay-limit-year-target-cB
                       (double
                         (/
                           (- t rstart-y)
                           (- limit-year rstart-y))))
                     :cB_reduction_speed (/ exp-decay-limit-year-target-cB
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
        {:data {:values [{:cB_red exp-decay-limit-year-target-cB}]}
         :transform
         [{:calculate "format(datum.cB_red, '.1f') + ' cB'"
           :as :my_rule_subtext}]
         :encoding {:color {:value "darkgreen"}
                    :strokeDash {:value [5, 3]}
                    :opacity {:value 1.}}
         :layer
         [{:mark "rule"
           :encoding {:y {:field :cB_red, :type "quantitative"}}}
          {:mark {:type "text",
                  :align "left",
                  :baseline "middle",
                  :dx 2,
                  ;:dy -2,
                  :x "width"}
           :encoding {:y {:field :cB_red, :type "quantitative"}
                      :text {:field :my_rule_subtext}}}]}
        {:data {:values [{:t (year->temporal limit-year)}]}
         :transform
         [{:calculate "'Pivot Year: ' + timeFormat(datum.t, '%Y')"
           :as :my_rule_subtext}]
         :encoding {:opacity {:value 0.5}}
         :layer
         [{:mark "rule"
           :encoding {:x {:field :t, :type "temporal"}
                      :strokeDash {:value [3 2]}}}
          {:mark {:type "text",
                  :align "left",
                  :baseline "top",
                  :dx 2,
                  :dy 2,
                  :y 0}
           :encoding {:x {:field :t, :type "temporal"}
                      :text {:field :my_rule_subtext}}}]}]}]}))

(comment
  (oz/view! exp-decay-chart)
  *e)