(ns ecolog10.graphics-all
  (:require [ecolog10.graphics.beef-diet-plans-bar-chart]
            [ecolog10.graphics.cement-economy-centibels]
            [ecolog10.graphics.exp-decay-global-pathways]
            [ecolog10.graphics.exp-decay-pathway]
            [ecolog10.graphics.kaya-cB-degrowth]
            [ecolog10.graphics.pct-vs-centibels-reductions]
            [ecolog10.graphics.reduction-plans-basic-picture-bar-chart]
            [ecolog10.graphics.rpct-centibels-conversion-ruler]
            [ecolog10.graphics.sketch-ghg-intensity-extrapolations]
            [oz.core :as oz]))


(defn page-view-all
  []
  (into [:div]
    (->>
      [["beef-diet-plans-bar-chart.svg"
        (ecolog10.graphics.beef-diet-plans-bar-chart/get-vega-chart)]
       ["cement-economy-centibels.svg"
        (ecolog10.graphics.cement-economy-centibels/get-vega-chart)]
       ["exp-decay-global-pathways.svg"
        (ecolog10.graphics.exp-decay-global-pathways/get-vegalite-chart)]
       ["exp-decay-pathway.svg"
        (ecolog10.graphics.exp-decay-pathway/get-vegalite-chart)]
       ["kaya-cB-degrowth.svg"
        (ecolog10.graphics.kaya-cB-degrowth/get-vega-chart)]
       ["pct-vs-centibels-reductions.svg"
        (ecolog10.graphics.pct-vs-centibels-reductions/get-vega-chart)]
       ["reduction-plans-basic-picture-bar-chart.svg"
        (ecolog10.graphics.reduction-plans-basic-picture-bar-chart/get-vega-chart)]
       ["rpct-centibels-conversion-ruler.svg"
        (ecolog10.graphics.rpct-centibels-conversion-ruler/get-vega-chart)]
       ["sketch-ghg-intensity-extrapolations.svg"
        (ecolog10.graphics.sketch-ghg-intensity-extrapolations/get-vegalite-chart)]]
      (map
        (fn [[file-name chart]]
          [:div
           [:h2 file-name]
           [:div {:style {:display "inline-block"
                          :border "1px dotted lightgray"}}
            chart]]))
      (interpose [:hr]))))

(oz/view! (page-view-all))

