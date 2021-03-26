(ns ecolog10.sandbox
  (:require [clojure.string :as str]
            [ecolog10.centibels-math :as cB]))


(comment

  (-> -50. cB/rpct-to-scalar cB/scalar-to-cB
    (/ 30)
    cB/cB-to-scalar
    cB/scalar-to-rpct)

  (-> -5.7 cB/rpct-to-scalar cB/scalar-to-cB)

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
                sf (cB/rpct-to-scalar rel-pct)
                cb (cB/scalar-to-cB sf)]
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