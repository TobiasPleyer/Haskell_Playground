(defn my-juxt
  "Return the juxtaposition of a collection of functions."
  [fs]
  (fn [x] (map (fn [f] (f x)) fs)))
