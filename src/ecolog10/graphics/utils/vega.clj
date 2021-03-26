(ns ecolog10.graphics.utils.vega
  (:require [com.rpl.specter :as sp]))

;; ------------------------------------------------------------------------------
;; General


(defn update-props
  [vega-spec k pred f & args]
  (sp/transform [k sp/ALL pred]
    (fn [x]
      (apply f x args))
    vega-spec))


(defn replace-deep
  [m old->new]
  (sp/transform
    (sp/walker #(contains? old->new %))
    (fn [x]
      (get old->new x))
    m))


(defn having?
  [k->v]
  (apply every-pred
    map?
    (mapv
      (fn [[k v]]
        (fn [m]
          (and
            (contains? m k)
            (= v (get m k)))))
      k->v)))



;; ------------------------------------------------------------------------------
;; Viz transforms


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