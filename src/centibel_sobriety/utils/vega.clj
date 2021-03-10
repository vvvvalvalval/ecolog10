(ns centibel-sobriety.utils.vega
  (:require [com.rpl.specter :as sp]
            [vvvvalvalval.supdate.api :as supd]))


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
