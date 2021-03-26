(ns ecolog10.utils.data
  (:import (java.util GregorianCalendar)))



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