(ns centibel-sobriety.core
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [oz.core :as oz]))


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
   :width 400,
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
            :encode {:enter {:x {:scale "xscale", :field "category"},
                             :width {:scale "xscale", :band 1},
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
   :height 200,
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
