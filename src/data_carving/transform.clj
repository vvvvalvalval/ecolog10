(ns data-carving.transform)


(defn tfn-emit
  [fn-name docstring [arg-sym] expr]
  {:data-carving.transform/name `(quote ~fn-name)
   :data-carving.transform/docstring docstring
   :data-carving.transform/source-location
   (let [{l :line c :column} (meta expr)]
     (when (and *file* l c)
       [*file* l c `(quote ~(ns-name *ns*))]))
   :data-carving.transorm/transform-fn
   `(fn [~arg-sym]
      ~expr)})


(defmacro tfn
  "'Transform-FN' Expresses a chart transform in a syntax similar to a function. Returns a map."
  ([arg-vec expr]
   (tfn-emit nil nil arg-vec expr))
  ([fn-name arg-vec expr]
   (tfn-emit fn-name nil arg-vec expr))
  ([fn-name docstring arg-vec expr]
   (tfn-emit fn-name docstring arg-vec expr)))


(defn end-result
  [initial-chart transforms]
  (reduce
    (fn [chrt {:as _tf, transform-fn :data-carving.transorm/transform-fn}]
      (transform-fn chrt))
    initial-chart
    transforms))