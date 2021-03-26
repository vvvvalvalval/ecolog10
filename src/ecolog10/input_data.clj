(ns ecolog10.input-data
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]))


(def remaining-carbon-budget
  646.5)


(def yearly-emissions-2019
  42.1)


(def d_historical-and-projected-population
  ;; Source: https://ourworldindata.org/future-population-growth (Val, 05 Oct 2020)
  ;; https://ourworldindata.org/grapher/UN-population-projection-medium-variant?tab=chart&time=2100&region=World
  (delay
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
        (csv/read-csv rdr)))))