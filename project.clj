(defproject vvvvalvalval/centibels-sobriety "0.1.0-SNAPSHOT"
  :description "An accessible introduction to using logarithmic representations for planning the environmental transition."
  :url "https://github.com/asciidoctor/asciidoctor-lein-plugin/tree/master/example"
  :license {:name "The MIT License"
            :url "https://github.com/asciidoctor/asciidoctor-lein-plugin/blob/master/LICENSE.adoc"}

  :dependencies [[lambdaisland/deep-diff2 "2.0.108"]
                 [hickory "0.7.1"]
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/data.csv "1.0.0"]
                 [ring/ring-core "1.8.1"] ;; for Oz deps problem: https://github.com/metasoarous/oz/issues/125
                 [vvvvalvalval/supdate "0.2.3"]
                 [com.rpl/specter "1.1.3"]
                 [metasoarous/oz "1.6.0-alpha26"]]

  ; List of plugins
  :plugins [[lein-asciidoctor "0.1.17"]
            [lein-auto "0.1.3"]
            [lein-shell "0.5.0"]]

  ; List of hooks
  ; It's used for running lein-asciidoctor during compile phase
  :hooks [lein-asciidoctor.plugin]

  :auto {"asciidoctor" {:file-pattern #"\.adoc$"
                        :paths ["adoc"]
                        :wait-time 500}
         "shell" {:file-pattern #"\.svg$"}}

  ; lein-asciidoctor configuration
  :asciidoctor {:sources "adoc/*.adoc"
                :format :html
                :to-dir "generated"
                :extract-css true
                :source-highlight true
                :toc :left})
