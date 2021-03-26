(ns data-carving.ui.incremental-oz
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [lambdaisland.deep-diff2 :as ddiff]
            [data-carving.transform :as dc]
            [hickory.core])
  (:import (clojure.lang LineNumberingPushbackReader)))




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
      (second (read+string rdr)))))


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
                   [viz-type-kw (dc/end-result initial-chart transforms)]]]]
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


(defn sculpture-oz-page
  [opts initial-chart transforms]
  [:div
   <introduction>
   (sculpting-oz opts
     initial-chart
     transforms)])
