(ns centibel-sobriety.img-exports
  (:require [oz.core :as oz]
            [centibel-sobriety.core :as cb-charts]
            [cheshire.core :as json]))


(defn save-charts-to-files!
  []
  [(oz/export!
     cb-charts/comparison-scale-chart
     "./img/cars-economy-centibels-2.svg"
     {:from-format :hiccup
      :to-format :svg})])


(comment

  (oz/export!
    cb-charts/cement-reductions-chart
    "./img/cars-economy-centibels-2.svg")

  *e)