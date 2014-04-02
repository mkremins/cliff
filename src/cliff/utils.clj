(ns cliff.utils)

(defn seq->map [aseq]
  (let [v (vec aseq)]
    (zipmap (range (count v)) v)))
