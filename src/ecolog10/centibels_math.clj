(ns ecolog10.centibels-math)


(defn rpct-to-scalar
  [rel-pct]
  (+ 1
    (/ rel-pct 100)))


(defn scalar-to-cB
  [scalar]
  (* 100.
    (Math/log10 scalar)))


(defn cB-to-scalar
  [cb]
  (Math/pow 10. (/ cb 100.)))


(defn scalar-to-rpct
  [scalar]
  (* 100
    (- scalar 1)))
