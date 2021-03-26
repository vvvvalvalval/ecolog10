(ns ecolog10.graphics.rpct-centibels-conversion-ruler
  (:require [ecolog10.centibels-math :as cB]
            [oz.core :as oz]))


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
                           :let [cB (-> rpct cB/rpct-to-scalar cB/scalar-to-cB)]
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

(defn get-vega-chart
  []
  [:vega comparison-scale-chart])


;(oz/view! (get-vega-chart))