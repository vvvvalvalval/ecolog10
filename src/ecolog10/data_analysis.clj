(ns ecolog10.data-analysis
  (:require [ecolog10.centibels-math :as cB :refer :all]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set :as cset]
            [oz.core :as oz]))


(comment

  ;;;; What are the fastest sustained decline in GHG/E observe across all countries?
  ;; Hope to see fast decline with oil shocks

  ;; https://ourworldindata.org/grapher/co2-per-unit-energy?tab=table&time=earliest..latest
  (io/resource "co2-per-unit-energy.csv")

  ;; https://ourworldindata.org/grapher/co2-intensity?tab=table&time=1870..latest
  (io/resource "co2-intensity.csv")



  (with-open [rdr (io/reader
                    (io/resource "co2-per-unit-energy.csv"))]
    (into []
      (comp
        (take 5))
      (csv/read-csv rdr)))
  =>
  [["Entity" "Code" "Year" "CO2 per unit energy (kgCO2 per kilowatt-hour)"]
   ["Africa" "" "1965" "0.4616215601409676"]
   ["Africa" "" "1966" "0.4660580802255964"]
   ["Africa" "" "1967" "0.4973917902782552"]
   ["Africa" "" "1968" "0.5089223785523191"]]


  (with-open [rdr (io/reader
                    (io/resource "co2-intensity.csv"))]
    (into []
      (comp
        (take 5))
      (csv/read-csv rdr)))
  =>
  [["Entity" "Code" "Year" "CO2 per GDP (kg per $PPP)"]
   ["Afghanistan" "AFG" "1950" "0.004322793912560688"]
   ["Afghanistan" "AFG" "1951" "0.004565425234216761"]
   ["Afghanistan" "AFG" "1952" "0.004416085749934761"]
   ["Afghanistan" "AFG" "1953" "0.004826425769632711"]]


  (def rel_co2-per-unit-energy
    (with-open [rdr (io/reader (io/resource "co2-per-unit-energy.csv"))]
      (into #{}
        (map
          (fn [[Entity Code Year y]]
            {:entity Entity
             :year (Long/parseLong Year)
             :co2-per-unit-energy (Double/parseDouble y)}))
        (rest
          (csv/read-csv rdr)))))

  (def rel_co2-intensity
    (with-open [rdr (io/reader (io/resource "co2-intensity.csv"))]
      (into #{}
        (map
          (fn [[Entity Code Year y]]
            {:entity Entity
             :year (Long/parseLong Year)
             :co2-per-GDP (Double/parseDouble y)}))
        (rest
          (csv/read-csv rdr)))))

  (def rel_all
    (into #{}
      (map
        (fn [m]
          (assoc m
            :primary-energy-per-GDP
            (/
              (:co2-per-GDP m)
              (:co2-per-unit-energy m)))))
      (cset/join rel_co2-per-unit-energy rel_co2-intensity)))


  (into (sorted-set)
    (map :entity)
    rel_all)

  (def whitelisted-countries
    #{;"Africa" ;; statistical glitch
      "Asia"
      "Asia (excl. China & India)"
      "China"
      "EU-27"
      "EU-28"
      "Europe"
      "North America"
      "North America (excl. USA)"
      "Oceania"
      "South America"
      "United States"
      "World"})


  (def whitelisted-countries
    #{;"Africa" ;; statistical glitch
      "Albania"
      "Algeria"
      "Angola"
      "Argentina"
      "Armenia"
      "Asia"
      "Asia (excl. China & India)"
      "Australia"
      "Austria"
      ;"Azerbaijan"
      "Bahrain"
      "Bangladesh"
      "Barbados"
      "Belarus"
      "Belgium"
      "Benin"
      "Bolivia"
      ;"Bosnia and Herzegovina"
      "Botswana"
      "Brazil"
      "Bulgaria"
      "Cambodia"
      "Cameroon"
      "Canada"
      "Cape Verde"
      "Chile"
      "China"
      "Colombia"
      "Comoros"
      "Congo"
      "Costa Rica"
      "Croatia"
      "Cuba"
      "Cyprus"
      "Czech Republic"
      "Democratic Republic of Congo"
      "Denmark"
      "Djibouti"
      "Dominica"
      "Dominican Republic"
      "EU-27"
      "EU-28"
      "Ecuador"
      "Egypt"
      "El Salvador"
      "Equatorial Guinea"
      "Estonia"
      "Ethiopia"
      "Europe"
      "Europe (excl. EU-27)"
      "Europe (excl. EU-28)"
      "Finland"
      "France"
      "Gabon"
      "Gambia"
      ;"Georgia"
      "Germany"
      "Ghana"
      "Greece"
      "Guatemala"
      "Guinea-Bissau"
      "Haiti"
      "Honduras"
      "Hong Kong"
      "Hungary"
      "Iceland"
      "India"
      "Indonesia"
      "Iran"
      ;"Iraq"
      "Ireland"
      "Israel"
      "Italy"
      "Jamaica"
      "Japan"
      "Jordan"
      "Kazakhstan"
      "Kenya"
      "Kuwait"
      "Kyrgyzstan"
      "Latvia"
      "Lebanon"
      "Lesotho"
      "Libya"
      "Lithuania"
      "Luxembourg"
      "Macedonia"
      "Malaysia"
      "Malta"
      "Mauritius"
      "Mexico"
      "Moldova"
      "Mongolia"
      "Montenegro"
      "Morocco"
      "Mozambique"
      "Namibia"
      "Nepal"
      "Netherlands"
      "New Zealand"
      "Nicaragua"
      "Niger"
      "Nigeria"
      "North America"
      "North America (excl. USA)"
      "North Korea"
      "Norway"
      "Oceania"
      "Oman"
      "Pakistan"
      "Panama"
      "Paraguay"
      "Peru"
      "Philippines"
      "Poland"
      "Portugal"
      ;"Qatar"
      "Romania"
      "Russia"
      "Saint Lucia"
      "Sao Tome and Principe"
      "Saudi Arabia"
      "Senegal"
      "Serbia"
      "Seychelles"
      "Singapore"
      "Slovakia"
      "Slovenia"
      "South Africa"
      "South America"
      "South Korea"
      "Spain"
      "Sri Lanka"
      "Sudan"
      "Swaziland"
      "Sweden"
      "Switzerland"
      "Taiwan"
      ;"Tajikistan"
      "Tanzania"
      "Thailand"
      "Togo"
      "Trinidad and Tobago"
      "Tunisia"
      "Turkey"
      ;"Turkmenistan"
      "Ukraine"
      "United Arab Emirates"
      "United Kingdom"
      "United States"
      "Uruguay"
      ;"Uzbekistan"
      "Venezuela"
      "Vietnam"
      "World"
      "Yemen"
      ;"Zambia"
      "Zimbabwe"})


  (def whitelisted-countries
    #{"Albania"
      "Australia"
      "Austria"
      "Belarus"
      "Belgium"
      "Bulgaria"
      "Canada"
      "Croatia"
      "Denmark"
      "EU-27"
      "EU-28"
      "Estonia"
      "Europe"
      "Europe (excl. EU-27)"
      "Europe (excl. EU-28)"
      "Finland"
      "France"
      "Germany"
      "Greece"
      "Hungary"
      "Iceland"
      "Ireland"
      "Israel"
      "Italy"
      "Japan"
      "Latvia"
      "Lithuania"
      "Luxembourg"
      "Macedonia"
      "Malta"
      "Mauritius"
      "Moldova"
      "Montenegro"
      "Netherlands"
      "New Zealand"
      "North America"
      "North America (excl. USA)"
      "Norway"
      "Oceania"
      "Poland"
      "Portugal"
      "Romania"
      "Russia"
      "Serbia"
      "Singapore"
      "Slovakia"
      "Slovenia"
      "Spain"
      "Sweden"
      "Switzerland"
      "Taiwan"
      "Ukraine"
      "United Kingdom"
      "United States"
      "World"})


  (def whitelisted-countries
    #{"Australia"
      "Belgium"
      "Canada"
      "Denmark"
      "EU-27"
      "EU-28"
      "Europe"
      "Finland"
      "France"
      "Germany"
      "Greece"
      "Iceland"
      "Ireland"
      "Israel"
      "Italy"
      "Japan"
      "Luxembourg"
      "Malta"
      "Mauritius"
      "Netherlands"
      "New Zealand"
      "North America"
      "North America (excl. USA)"
      "Norway"
      "Portugal"
      "Spain"
      "Sweden"
      "Switzerland"
      "Taiwan"
      "United Kingdom"
      "United States"
      "World"})

  (def whitelisted-countries
    #{"Australia"
      "Belgium"
      "Canada"
      ;"Denmark"
      "EU-27"
      "EU-28"
      "Europe"
      "Finland"
      "France"
      ;"Germany"
      "Greece"
      ;"Iceland"
      ;"Ireland"
      "Israel"
      "Italy"
      "Japan"
      ;"Luxembourg"
      ;"Malta"
      ;"Mauritius"
      "Netherlands"
      "New Zealand"
      ;"North America"
      ;"North America (excl. USA)"
      ;"Norway"
      "Portugal"
      "Spain"
      ;"Sweden"
      ;"Switzerland"
      ;"Taiwan"
      "United Kingdom"
      ;"United States"
      "World"})

  (def whitelisted-countries
    #{"World"})

  (->>
    (for [{:as m1, y1 :year e1 :entity v1 :final-energy-per-GDP} rel_all
          :when (contains? whitelisted-countries e1)
          {:as m2, y2 :year e2 :entity v2 :final-energy-per-GDP} rel_all
          :when (and
                  (= y2 (+ y1 10))
                  (= e2 e1)
                  (< v2 v1))]
      (let [cb-red (/
                     (-> (/ v2 v1) cB/scalar-to-cB)
                     (- y2 y1))]
        [cb-red
         e1
         y1 y2]))
    (sort-by first)
    (take 100))

  ;; https://ourworldindata.org/grapher/final-to-primary-energy-ratio?time=2010
  (def rel_final-primary-ratio
    (with-open [rdr (io/reader (io/resource "final-to-primary-energy-ratio.csv"))]
      (into #{}
        (map
          (fn [[Entity Code Year v]]
            {:entity Entity
             :year (Long/parseLong Year)
             :ratio-final-primary (* 1e-2 (Double/parseDouble v))}))
        (rest
          (csv/read-csv rdr)))))

  (def rel_all
    (into #{}
      (map
        (fn [m]
          (assoc m
            :final-energy-per-GDP
            (*
              (:ratio-final-primary m)
              (:primary-energy-per-GDP m)))))
      (cset/join rel_all rel_final-primary-ratio)))


  (def rel_world-sums)


  (def whitelisted-countries
    #{"Australia"
      "Canada"
      "Denmark"
      "France"
      "Germany"
      "Greece"
      "Iceland"
      "Italy"
      "Japan"
      "Netherlands"
      "New Zealand"
      "North America"
      "Norway"
      "Spain"
      "United Kingdom"
      "United States"})

  (oz/view!
    [:vega-lite
     {:$schema "https://vega.github.io/schema/vega-lite/v4.json",
      :description "",
      :data {:values (into []
                       (filter #(contains? whitelisted-countries
                                  (:entity %)))
                       rel_all)},
      :mark "line",
      :encoding {:x {:field :year, :type "quantitative"},
                 :y {:field :final-energy-per-GDP, :type "quantitative"},
                 :color {:field :entity, :type "nominal"}}}])


  ;; https://ourworldindata.org/grapher/energy-intensity-of-economies
  (-> (/
        1.43 ;; 2015 - World
        1.72) ;; 2005 - World
    cB/scalar-to-cB (/ (- 2015 2005)))
  => -0.80

  (-> (/
        1.43 ;; 2015 - World
        1.99) ;; 1995 - World
    cB/scalar-to-cB (/ (- 2015 1995)))
  => -0.7175851947232242

  *e)

