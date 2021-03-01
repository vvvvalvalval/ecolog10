(ns centibel-sobriety.core
  (:require [centibel-sobriety.math :as cB :refer :all]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [com.rpl.specter :as sp]
            [lambdaisland.deep-diff2 :as ddiff]
            [hickory.core]
            [oz.core :as oz]
            [vvvvalvalval.supdate.api :as supd]
            [clojure.java.shell :as shell])
  (:import (java.util GregorianCalendar)
           (clojure.lang LineNumberingPushbackReader)
           (java.nio.file Paths)))

;; ------------------------------------------------------------------------------
;; Data utils

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


;; ------------------------------------------------------------------------------
;; Vega Utils



(defn ax_hide-labels
  [ax]
  (update ax :encode assoc
    :labels                                                 ;; https://github.com/vega/vega/issues/791#issuecomment-297766635
    {:update
     {:text {:value ""}}}))


(defn ax_hide-ticks
  [ax]
  (update ax :encode assoc
    :ticks
    {:update
     {:opacity {:value 0}}}))




(defn hbar_set-padding
  "Narrows a horizontal bar mark around its vertical position, adding padding in the band above and below.

  Given:
  - rect-enc: a map that Vega-encodes the bar's vertical position (:y)
  - padding: a number in [0, 0.5)
  Returns the transformed rect-enc."
  [{:as rect-enc, y :y} padding]
  (let [bar-thickness (- 1. (* 2 padding))]
    (-> rect-enc
      (update :y assoc :band padding)
      (update :height
        (fn [h]
          (merge
            (select-keys y [:scale])
            h
            {:band bar-thickness}))))))


(comment
  (hbar_set-padding
    {:y {:scale "yscale", :field :rdact_factor}
     :width {:scale "xscale", :field :rdact_cb}
     :x {:scale "xscale", :value 0}}
    0.2)
  => {:y {:scale "yscale", :field :rdact_factor, :band 0.2},
      :width {:scale "xscale", :field :rdact_cb},
      :x {:scale "xscale", :value 0},
      :height {:scale "yscale", :band 0.6}}

  *e)


(defn scale_set-discrete-color-scheme
  [scale-obj colors]
  (assoc scale-obj :range {:scheme colors}))


(defn text_center-in-hband
  [{:as text-enc, y :y}]
  (assoc text-enc
    :y (assoc y :band 0.5)
    :baseline {:value "middle"}))


(defn text_put-below-hbar
  [{:as text-enc, y :y} padding offset-px]
  (merge text-enc
    {:y (merge y
          {:band (- 1. padding)
           :offset offset-px})
     :baseline {:value "top"}}))


(defn text_put-above-hbar
  [{:as text-enc, y :y} padding offset-px]
  (merge text-enc
    {:y (merge y
          {:band padding
           :offset offset-px})
     :baseline {:value "bottom"}}))

;; ------------------------------------------------------------------------------
;; Misc

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
;; Horizontal comparison scale


(def comparison-scale-chart
  (let [cB-from -100.5
        cB-to 0.]
    {:$schema "https://vega.github.io/schema/vega/v5.json",
     ;:title {:text "centibels-% mapping"}
     :axes []
     :width 1000, :height 100,
     :data [{:name "centibels",
             :values (for [cB (range
                                (long (Math/floor cB-from))
                                (long (inc (Math/floor cB-to))))
                           :when (<= cB-from cB cB-to)]
                       {:x cB
                        :h (- (case (mod cB 10)
                                0 3
                                5 2
                                1))
                        :txt
                        (when (= 0 (mod cB 10))
                          (str
                            (if (pos? cB)
                              "+")
                            cB
                            " cB"))})}
            {:name "percentages"
             :values (for [rpct (range -90 1000)
                           :let [cB (-> rpct rpct-to-scalar scalar-to-cB)]
                           :when (<= cB-from cB cB-to)]
                       {:x cB
                        :txt (if (= 0 (mod rpct 10))
                               (str
                                 (when (not (neg? rpct)) "+")
                                 rpct
                                 "%"))
                        :h (case (mod rpct 10)
                             0 3
                             5 2
                             1)})}]

     :scales [{:name "xscale",
               :reverse true
               :domain {:data "centibels", :field :x},
               :range "width",
               :padding 0.05}
              {:name "yscale",
               :domain [-10. 10.]
               :range "height"}],
     :padding 5,
     :marks [{:name "haxis"
              :type "rule"
              :encode {:update
                       {:x {:scale "xscale" :value cB-from}
                        :x2 {:scale "xscale" :value cB-to}
                        :y {:scale "yscale" :value 0.}
                        :strokeWidth {:value 0.7}
                        :opacity {:value 0.8}}}}

             {:name "cB_ticks"
              :type "rule"
              :from {:data "centibels"}
              :encode {:update
                       {;:zindex {:value -10}
                        :x {:scale "xscale" :field :x}
                        :y {:scale "yscale" :value 0.}
                        :y2 {:scale "yscale" :field :h}
                        :stroke {:value "#001c7f"}
                        :strokeWidth {:value 1.}
                        :opacity {:value 1.}}}}

             {:name "rcpt_ticks"
              :type "rule"
              :from {:data "percentages"}
              :encode {:update
                       {;:zindex {:value -10}
                        :x {:scale "xscale" :field :x}
                        :y {:scale "yscale" :value 0.}
                        :y2 {:scale "yscale" :field :h}
                        :stroke {:value "#b1400d"}
                        :strokeWidth {:value 1.}
                        :opacity {:value 1.}}}}

             {:name "cB_subtexts"
              :type "text"
              :from {:data "centibels"}
              :encode
              {:enter {:align {:value "center"}
                       :baseline {:value "top"}
                       :fill {:value "#001c7f"}}
               :update {:x {:scale "xscale" :field :x}
                        :y {:scale "yscale" :field :h :offset 2}
                        :text {:field :txt}
                        :fillOpacity {:value 1}}}}

             {:name "rpct_subtexts"
              :type "text"
              :from {:data "percentages"}
              :encode
              {:enter {:align {:value "center"}
                       :baseline {:value "bottom"}
                       :fill {:value "#b1400d"}}
               :update {:x {:scale "xscale" :field :x}
                        :y {:scale "yscale" :field :h :offset -2}
                        :text {:field :txt}
                        :fillOpacity {:value 1}}}}]}))

;(oz/view! [:vega comparison-scale-chart])


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
                         (hbar_set-padding bars-v-padding))
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
                                (text_center-in-hband))}}
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
                         (text_put-below-hbar bars-v-padding 2))}}

             {:name "reduction_action_bar"
              :type "rect",
              :from {:data "factor_reductions"},
              :encode {:enter (->
                                {:y {:scale "yscale", :field :rdact_factor},
                                 :width {:scale "xscale", :field :rdact_cb}
                                 :x {:scale "xscale", :field :rdact_previous_cb}}
                                (hbar_set-padding bars-v-padding))
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
                                (text_center-in-hband))}},
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
                         (text_put-below-hbar bars-v-padding 2))}}

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


;(oz/view! [:vega cement-reductions-chart])


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


(def exp-decay-single-chart
  (let [ref-year 2021
        limit-year (+ ref-year (/ remaining-carbon-budget yearly-emissions-2019))]
    {:$schema "https://vega.github.io/schema/vega-lite/v4.json",
     :description "",
     :title {:text "Exponential-decay emissions reduction pathway"
             :align "center"
             :anchor "middle"
             :orient "bottom"
             :baseline "top"}
     :vconcat
     [{:height 120 :width 480
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
                (for [t (concat (range 2019 2045 0.1) [2045])
                      :let [cb-red (if (< t ref-year)
                                     0.
                                     (*
                                       exp-decay-limit-year-target-cB
                                       (double
                                         (/
                                           (- t ref-year)
                                           (- limit-year ref-year)))))]]
                  {:t (year->temporal t)
                   :yearly_ghg_emissions
                   (-> cb-red
                     (cB-to-scalar)
                     (* yearly-emissions-2019))

                   :red_start_year ref-year})}
         :mark {:type :line
                :interpolate :monotone}
         :encoding {:x {:field :t, :type "temporal",
                        :title "Time"}
                    :y {:field :yearly_ghg_emissions, :type "quantitative"
                        :title "CO₂ emissions (GtCO₂e/year)"}}}]}
      {:height 180 :width 480
       :layer
       [{:data {:values
                (for [t [2019
                         ref-year
                         limit-year
                         2045]
                      :let [cb-red (if (< t ref-year)
                                     0
                                     (* exp-decay-limit-year-target-cB
                                       (double
                                         (/
                                           (- t ref-year)
                                           (- limit-year ref-year)))))]]
                  {:t (year->temporal t)
                   :cB_red cb-red})}
         :mark {:type :line
                :interpolate :linear}
         :encoding {:x {:field :t, :type "temporal",
                        :title nil
                        :axis {:orient "top"}},
                    :y {:field :cB_red, :type "quantitative"
                        :title "Reduction (cB)"}}}
        {:data {:values
                [(let [t 2045]
                   {:t (year->temporal t)
                    :red_start_year ref-year
                    :cB_red
                    (* exp-decay-limit-year-target-cB
                      (double
                        (/
                          (- t ref-year)
                          (- limit-year ref-year))))
                    :cB_reduction_speed (/ exp-decay-limit-year-target-cB
                                          (- limit-year ref-year))})]}
         :transform
         [{:calculate "format(datum.cB_reduction_speed, '.2f') + ' cB/year'"
           :as :cB_reduction_speed_description}]
         :encoding {:x {:field :t, :type "temporal"},
                    :y {:field :cB_red, :type "quantitative"
                        :title "Reduction (cB)"}},
         :layer [{:mark {:type "text", :align "left", :dx 4}
                  :encoding {:text
                             {:field :cB_reduction_speed_description}}}]}]}]}))

;(oz/view! exp-decay-single-chart)


(def exp-decay-multi-chart
  (let [ref-year 2020.7
        limit-year (+ ref-year (/ remaining-carbon-budget yearly-emissions-2019))
        redstart-years [2021
                        2026
                        2031]]
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

;(oz/view! exp-decay-multi-chart)


;; ------------------------------------------------------------------------------
;; % vs cB chart


(def pct-vs-cB-chart
  (let [reduction-actions
        [{:rdact_factor 0.25
          :rdact_factor_name "(Carbon Intensity)"
          :rdact_description "Efficiency: -75% emission factor"
          :rdact_colour "#4c72b0"}
         {:rdact_factor 0.4
          :rdact_factor_name "(Usage)"
          :rdact_description "Sobriety: -60% usage"
          :rdact_colour "#55a868"}]
        target-pct (apply * 100.
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
      (let [target-cB (-> (- target-pct 100.) rpct-to-scalar scalar-to-cB)
            reduction-actions1
            (->> reduction-actions
              (mapv
                (fn [ra]
                  (assoc ra
                    :rdact_cb (-> ra :rdact_factor scalar-to-cB))))
              (add-cumulated-field :rdact_previous_cb :rdact_cb))]
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
                  (ax_hide-ticks)
                  (ax_hide-labels))
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
                               (hbar_set-padding bars-v-padding))
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
                               (text_center-in-hband))}}
            {:name "reduction_action_bar"
             :type "rect",
             :from {:data "reduction_actions"},
             :encode {:enter (-> {:y {:scale "yscale", :field :rdact_factor_name}
                                  :x2 {:scale "xscale", :signal "datum.rdact_previous_cb + datum.rdact_cb"}
                                  :x {:scale "xscale", :field :rdact_previous_cb}}
                               (hbar_set-padding bars-v-padding))
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
                               (text_center-in-hband))}}
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
                        (text_put-below-hbar bars-v-padding 2))}}
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


;(oz/view! [:vega pct-vs-cB-chart])





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



;; ------------------------------------------------------------------------------
;; Kaya

(def historical-and-projected-population
  ;; Source: https://ourworldindata.org/future-population-growth (Val, 05 Oct 2020)
  ;; https://ourworldindata.org/grapher/UN-population-projection-medium-variant?tab=chart&time=2100&region=World
  (with-open [rdr (io/reader
                    (io/resource "comparison-of-world-population-projections.csv"))]
    (into []
      (comp
        (drop 1)
        (map
          (let [cols [:year
                      :population_HYDE_estimate_1750_1950
                      :population_UN_estimate_1750_1950
                      :population_prediction_UN_medium_variant
                      :population_prediction_UN_high_variant
                      :population_prediction_UN_low_variant
                      :population_prediction_UN_constant_variant]]
            (fn [row]
              (into {}
                (map
                  (fn [k s]
                    [k
                     (when-not (= "" s)
                       (if (= :year k)
                         (Long/parseLong s 10)
                         (Double/parseDouble s)))])
                  cols row))))))
      (csv/read-csv rdr))))


(defn predicted-population
  [year-map]
  (or
    (:population_prediction_UN_medium_variant year-map)
    (:population_UN_estimate_1750_1950 year-map)
    (:population_HYDE_estimate_1750_1950 year-map)))


(def pop-2021
  (->> historical-and-projected-population
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
             (for [ym historical-and-projected-population
                   :let [year (:year ym)]
                   :when (<= 2020 year 2050)]
               (let [no-growth
                     (if (< year 2021)
                       0.
                       (/
                         (*
                           exp-decay-limit-year-target-cB
                           (- year 2021))
                         (- 2036 2021)))
                     plus-gdp-growth
                     (+ no-growth
                       (if (< year 2021)
                         0.
                         (*
                           (-> 2 rpct-to-scalar scalar-to-cB -)
                           (- year 2021))))
                     plus-pop-growth
                     (+ plus-gdp-growth
                       (if (< year 2021)
                         0.
                         (-> (/ (predicted-population ym) pop-2021)
                           scalar-to-cB
                           -)))]
                 {:x (year->temporal year)
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


;(oz/view! kaya-time-chart)




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

;(oz/view! proxy-cost-sketch)



;; ------------------------------------------------------------------------------
;; Various plans for reducing beed consumption GHG emissions


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


(def beef-ghg-reductions-plans-chart
  (let [bars-thickness 0.3
        bars-v-padding (/ (- 1 bars-thickness) 2)]
    {:title {:text "4 diet plans to reduce GHG emissions from beef consumption."
             :orient :bottom
             :offset 15}
     :$schema "https://vega.github.io/schema/vega/v5.json",
     :width 850, :height 250,
     :data [{:name "factor_reductions",
             :values (->> beef-reduction-plans
                       (into []
                         (comp
                           (map-indexed
                             (fn [i plan]
                               (->> (:plan_actions plan)
                                 (add-cumulated-field
                                   :rdact_previous_cb
                                   :rdact_cb)
                                 (mapv
                                   (fn [rdact]
                                     (assoc rdact
                                       :plan_id i
                                       :rdact_rpct (-> rdact :rdact_cb cB/cB-to-scalar cB/scalar-to-rpct)))))))
                           cat)))}],
     :axes [(-> {:orient "left",
                 :scale "yscale"}
              (ax_hide-labels)
              (ax_hide-ticks))
            {:title "Reduction (cB)"
             :orient "bottom",
             :scale "xscale"}],
     :scales [{:name "yscale",
               :type "band",
               :domain {:data "factor_reductions", :field :plan_id},
               :range "height"}
              {:name "xscale",
               :domain [-100. 0]
               :range "width",
               :reverse true
               :round true}
              (-> {:name "color"
                   :type :ordinal
                   :domain {:data "factor_reductions", :field :rdact_factor}}
                (scale_set-discrete-color-scheme ["#55a868" "#4c72b0"]))],
     :legends [{:title "Reduced factor"
                :fill "color"}]
     :padding 5,
     :marks [{:name "reduction_action_bar"
              :type "rect",
              :from {:data "factor_reductions"},
              :encode {:enter
                       (-> {:y {:scale "yscale", :field :plan_id},
                            :x {:scale "xscale", :field :rdact_previous_cb}
                            :width {:scale "xscale", :field :rdact_cb}
                            :fill {:scale "color" :field :rdact_factor}}
                         (hbar_set-padding bars-v-padding))}}
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
                         (text_center-in-hband))}}
             {:name "reduction_action_bar_subtext"
              :type "text",
              :from {:data "factor_reductions"},
              :encode {:update
                       (->
                         {:text {:signal "datum.rdact_description  + ' (' + format(datum.rdact_rpct, '.0f') + '%)'"}
                          :fill {:scale "color" :field :rdact_factor}
                          :fillOpacity {:value 1}
                          :fontStyle {:value :italic}
                          :x {:scale "xscale",
                              :signal "datum.rdact_previous_cb"
                              :offset 2}
                          :align {:value "left"},
                          :y {:scale "yscale",
                              :field :plan_id}}
                         (text_put-above-hbar bars-v-padding -2))}}]}))


;(oz/view! [:vega beef-ghg-reductions-plans-chart])


;; ------------------------------------------------------------------------------
;; Sculpting vega specs

(defmacro show-metas
  [& args]
  (into [*file*]
    (mapv meta args)))

(def x
  (show-metas

    (+ 2
      (* 3 12))
    (fn [x]
      (+ 3 4))))

(comment




  (meta #'sculpting-oz)


  *e)

(defn expr-string-at
  [file line column]
  (with-open [rdr (LineNumberingPushbackReader.
                    (io/reader file))]
    (dotimes [_ (dec line)]
      (.readLine rdr))
    (dotimes [_ (dec column)]
      (.read rdr))
    (str
      (str/join (repeat (dec column) " "))
      (second (read+string rdr)))
    #_
    (let [lf (.getLineNumber rdr)
          cf (.getColumnNumber rdr)]
      (with-open [rdr2 (io/reader file)]
        (let [n-lines (+ 1 (- lf line))]
          (->> (line-seq rdr2)
            (drop (dec line))
            (take n-lines)
            (str/join "\n")))))))


(comment
  (println
    (expr-string-at "/Users/val/projects/centibel-sobriety/centibel-sobriety/src/centibel_sobriety/core.clj"
      1799 5))

  *e)


(defn resolve-git-sha []
  (when-some [m (re-matches #"(.+)\n"
                  (-> (shell/sh "git" "rev-parse" "HEAD")
                    :out))]
    (let [[_ git-commit-sha] m]
      git-commit-sha)))


(defn github-url
  [[fpath lnum]]
  (str
    "https://github.com/vvvvalvalval/ecolog10/blob/"
    (resolve-git-sha)
    "/"
    (let [[_ relpath] (re-matches #".*\/centibel\-sobriety\/(src\/.*)"
                        fpath)]
      relpath)
    "#L" lnum))


(comment
  (.relativize
    (str
      (.toUri
        (Paths/get "/Users/val/projects/centibel-sobriety/centibel-sobriety/src/centibel_sobriety/core.clj"
          (into-array String []))))
    (str
      (.toUri
        (.toAbsolutePath (Paths/get "." (into-array String []))))))
  *e)


(defn html-inline-style
  [style-map]
  (str/join " "
    (map
      (fn [[k v]]
        (str (name k) ": " v ";"))
      style-map)))



(defn sculpting-oz
  [{:as _opts
    viz-type-kw ::viz-type
    targets-reagent? ::targets-reagent?
    :or {viz-type-kw :vega
         targets-reagent? false}}
   initial-chart transforms]
  (into [:div]
    (loop [ret [[:h2 "The big picture"]
                [:div
                 [:div {:style (cond-> {:display "inline-block"}
                                 (not targets-reagent?)
                                 (html-inline-style))}
                  [:div [:strong [:em "Starting point:"]]]
                  [:div {:style (cond-> {:border "1px dotted lightgray"}
                                  (not targets-reagent?)
                                  (html-inline-style))}
                   [viz-type-kw initial-chart]]]
                 [:div {:style (cond-> {:display "inline-block"}
                                 (not targets-reagent?)
                                 (html-inline-style))}
                  [:div [:strong [:em "End result:"]]]
                  [:div {:style (cond-> {:border "1px dotted lightgray"}
                                  (not targets-reagent?)
                                  (html-inline-style))}
                   [viz-type-kw
                    (reduce
                      (fn [chrt {:as _tf, transform-fn :data-carving.transorm/transform-fn}]
                        (transform-fn chrt))
                      initial-chart
                      transforms)]]]]
                [:hr]
                [:h2 "Detailed steps"]
                [:div
                 [:div {:style (cond-> {:display "inline-block"}
                                 (not targets-reagent?)
                                 (html-inline-style))}
                  [:div [:strong [:em "Starting point:"]]]
                  [:div {:style (cond-> {:border "1px dotted lightgray"}
                                  (not targets-reagent?)
                                  (html-inline-style))}
                   [viz-type-kw initial-chart]]]]]
           chart initial-chart
           tfs transforms]
      (if (empty? tfs)
        ret
        (let [{:as tf, transform-fn :data-carving.transorm/transform-fn} (first tfs)
              next-chart (transform-fn chart)]
          (recur
            (conj ret
              [:div {:style (cond-> {:margin-top "5em"}
                              (not targets-reagent?)
                              (html-inline-style))}
               (when-some [fn-name (:data-carving.transform/name tf)]
                 [:span [:strong [:code fn-name " : "]]
                  (when-some [[f l _c ns-sym] (:data-carving.transform/source-location tf)]
                    [:code [:a {:href (github-url [f l])
                                :target "_blank"}
                            (str "(" ns-sym ":" l ")")]])])
               (when-some [docstring (:data-carving.transform/docstring tf)]
                 [:span [:blockquote docstring]])
               (when-some [[f l c _ns-sym] (:data-carving.transform/source-location tf)]
                 [:div
                  [:pre [:code (expr-string-at f l c)]]])
               [:pre
                (let [data-diff-html
                      (with-out-str
                        (ddiff/pretty-print
                          (let [[x1 x2] (clojure.data/diff chart next-chart)]
                            (ddiff/diff x1 x2))
                          (ddiff/printer {:color-markup :html-inline
                                          :color-scheme
                                          {:lambdaisland.deep-diff2.printer-impl/deletion [:bold :red]
                                           :lambdaisland.deep-diff2.printer-impl/insertion [:bold :green]
                                           ;; lambdaisland.deep-diff2.puget uses green and red for
                                           ;; boolean/tag, but we want to reserve
                                           ;; those for diffed values.

                                           :delimiter nil
                                           :tag nil

                                           ;; primitive values
                                           :nil nil
                                           :boolean nil
                                           :number nil
                                           :string nil
                                           :character nil
                                           :keyword nil
                                           :symbol nil
                                           :lambdaisland.deep-diff2.printer-impl/other [:bold :black]}})))]
                  (if targets-reagent?
                    [:div {:dangerouslySetInnerHTML {:__html data-diff-html}}]
                    (into [:div]
                      (map hickory.core/as-hiccup)
                      (hickory.core/parse-fragment data-diff-html))))]]
              [:div
               [:div {:style (cond-> {:display "inline-block"}
                               (not targets-reagent?)
                               (html-inline-style))}
                [:div [:strong [:em "Before:"]]]
                [:div {:style (cond-> {:border "1px dotted lightgray"}
                                (not targets-reagent?)
                                (html-inline-style))}
                 [viz-type-kw chart]]]
               [:div {:style (cond-> {:display "inline-block"}
                               (not targets-reagent?)
                               (html-inline-style))}
                [:div [:strong [:em "After:"]]]
                [:div {:style (cond-> {:border "1px dotted lightgray"}
                                (not targets-reagent?)
                                (html-inline-style))}
                 [viz-type-kw next-chart]]]])
            next-chart
            (next tfs)))))))


(defn replace-deep
  [m old->new]
  (sp/transform
    (sp/walker #(contains? old->new %))
    (fn [x]
      (get old->new x))
    m))

(defn having?
  [k v]
  (fn [x]
    (and
      (map? x)
      (contains? x k)
      (= v (get x k)))))


(defn tfn-emit
  [fn-name docstring [arg-sym] expr]
  {:data-carving.transform/name `(quote ~fn-name)
   :data-carving.transform/docstring docstring
   :data-carving.transform/source-location
   (let [{l :line c :column} (meta expr)]
     (when (and *file* l c)
       [*file* l c `(quote ~(ns-name *ns*))]))
   :data-carving.transorm/transform-fn
   `(fn [~arg-sym]
      ~expr)})


(defmacro tfn
  "'Transform-FN' Expresses a chart transform in a syntax similar to a function. Returns a map."
  ([arg-vec expr]
   (tfn-emit nil nil arg-vec expr))
  ([fn-name arg-vec expr]
   (tfn-emit fn-name nil arg-vec expr))
  ([fn-name docstring arg-vec expr]
   (tfn-emit fn-name docstring arg-vec expr)))



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


(def transforms-to-beef-plans
  [(tfn make-more-minimalist
     "Simplifies the initial Vega chart, removing the hovering behaviour."
     [vega-spec]
     (-> vega-spec
       (dissoc :signals)
       (update :marks #(-> % (butlast) (vec)))
       (update-in [:marks 0 :encode] dissoc :hover)))
   (tfn remove-padding
     "Zeroes the :padding property, just to see what this does."
     [vega-spec]
     (-> vega-spec
       (assoc :padding 0)
       #_(update-in [:scales 0] dissoc :padding)))
   (tfn make-horizontal-bar-chart [vega-spec]
     (-> vega-spec
       (update-in [:axes 0] assoc :orient "left")
       (update-in [:axes 1] assoc :orient "bottom")
       (update-in [:scales 0] assoc :range "height")
       (update-in [:scales 1] assoc :range "width")
       (replace-deep {"xscale" "yscale"
                      "yscale" "xscale"})
       (->>
         (sp/transform [:marks sp/ALL :encode sp/MAP-VALS]
           (fn [enc]
             (replace-deep enc
               {:x :y
                :width :height
                :y :x
                :y2 :x2}))))))
   (tfn add-real-data
     "Gets rid of the fake example data, replacing it with the data we actually want to display."
     [vega-spec]
     (-> vega-spec
       (assoc
         :data
         [{:name "factor_reductions",
           :values (->> beef-reduction-plans
                     (into []
                       (comp
                         (map-indexed
                           (fn [i plan]
                             (->> (:plan_actions plan)
                               (add-cumulated-field
                                 :rdact_previous_cb
                                 :rdact_cb)
                               (mapv
                                 (fn [rdact]
                                   (assoc rdact
                                     :plan_id i
                                     :rdact_rpct (-> rdact :rdact_cb cB/cB-to-scalar cB/scalar-to-rpct)))))))
                         cat)))}])
       (assoc
         :legends
         [{:title "Reduced factor"
           :fill "color"}])
       (update :scales conj
         (-> {:name "color"
              :type :ordinal
              :domain {:data "factor_reductions", :field :rdact_factor}}
           (scale_set-discrete-color-scheme ["#55a868" "#4c72b0"])))
       (replace-deep {"table" "factor_reductions"
                      "category" :plan_id
                      "amount" :rdact_cb})
       (->>
         (sp/transform [:marks sp/ALL (having? :type "rect") :encode]
           (fn [encode]
             (-> encode
               (supd/supdate
                 {:update false
                  :enter
                  (fn [enc]
                    (-> enc
                      (dissoc :x2)
                      (assoc
                        :y {:scale "yscale", :field :plan_id}
                        :x {:scale "xscale", :field :rdact_previous_cb}
                        :width {:scale "xscale", :field :rdact_cb}
                        :fill {:scale "color" :field :rdact_factor})))}))))
         (sp/transform [:scales sp/ALL (having? :name "xscale")]
           (fn [scale]
             (merge scale
               {:reverse true
                :domain [-100. 0]}))))))
   (tfn add-subtext-above-bars
     "Thins out the bars and adds subtext on top of them, describing reduction actions."
     [vega-spec]
     (let [bars-thickness 0.3
           bars-v-padding (/ (- 1 bars-thickness) 2)]
       (-> vega-spec
         (update-in [:marks 0 :encode :enter]
           (fn [enc]
             (-> enc
               (hbar_set-padding bars-v-padding))))
         (update :marks conj
           {:name "reduction_action_bar_subtext"
            :type "text",
            :from {:data "factor_reductions"},
            :encode {:update
                     (->
                       {:text {:signal "datum.rdact_description  + ' (' + format(datum.rdact_rpct, '.0f') + '%)'"}
                        :fill {:scale "color" :field :rdact_factor}
                        :fillOpacity {:value 1}
                        :fontStyle {:value :italic}
                        :x {:scale "xscale",
                            :signal "datum.rdact_previous_cb"
                            :offset 2}
                        :align {:value "left"},
                        :y {:scale "yscale",
                            :field :plan_id}}
                       (text_put-above-hbar bars-v-padding -2))}}))))
   (tfn adjust-dimensions
     "Widens the chart so that subtext can span without collision."
     [vega-spec]
     (assoc vega-spec :width 850, :height 250))
   (tfn clear-y-axis [vega-spec]
     (sp/transform [:axes sp/ALL (having? :scale "yscale")]
       (fn [ax]
         (-> ax
           (ax_hide-labels)
           (ax_hide-ticks)))
       vega-spec))
   (tfn add-titles [vega-spec]
     (-> vega-spec
       (assoc :title "4 diet plans to reduce GHG emissions from beef consumption.")
       (->>
         (sp/transform [:axes sp/ALL (having? :scale "xscale")]
           (fn [ax]
             (assoc ax :title "Reduction (cB)"))))))
   (tfn adjust-title [vega-spec]
     (update vega-spec :title
       (fn [title-text]
         {:text title-text
          :orient :bottom
          :offset 15})))
   (tfn add-bars-content
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
                     (text_center-in-hband))}})))])

(def <introduction>
  [:div
   [:p
    "This code-generated Oz document demonstrates a kind of 'literate dataviz programming', "
    "in which a sophisticated Vega chart is progressively made via various edits to a "
    [:a {:href "https://vega.github.io/vega/examples/bar-chart/" :target "_blank"} "basic example chart"]
    "."]
   [:p
    [:em
     "(Realistically, this may well be the main approach developers use for programming Vega graphics. "
     "Who makes a correct Vega chart from scratch?)"]]
   [:p
    "Clojure's data diffing tools and code-awareness (via macros) are used to  "
    [:strong "automatically display what changed between the various steps of building the final chart. "]
    "This might make the chart's code more accessible, as it's digested in small increments by the reader of the code. "
    "It may also be useful for teaching Vega."]
   [:p
    "Another strength of Clojure in this case is its data-transformation capabilities, "
    "which make the incremental edits to the Vega steps manageable."]
   [:p
    [:strong
     "The rest of this document was generated automatically, "
     "from the code that programs the incremental edits leading to the final chart."]]])

(def sculpture
  [:div
   <introduction>
   (sculpting-oz {::viz-type :vega, ::targets-reagent? true}
     initial-vertical-bar-chart
     transforms-to-beef-plans)])


(oz/view! sculpture)

(comment

  (oz/export!
    [:div
     <introduction>
     (sculpting-oz {::viz-type :vega, ::targets-reagent? false}
       initial-vertical-bar-chart
       transforms-to-beef-plans)]
    "generated/sculpture-test-0.html")

  (ddiff/pretty-print
    (ddiff/diff {:a 1 :b 2} {:a 1 :b 3})
    (ddiff/printer {:color-markup :html-inline}))

  *e)


