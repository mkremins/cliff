(ns cliff.core
  (:require [clojure.set :as set]
            [cliff.utils :refer [seq->map]]))

(declare diff)

(defn- map-diff [m1 m2]
  (let [ks1     (set (keys m1))
        ks2     (set (keys m2))
        idents  (set (filter #(= (get m1 %) (get m2 %)) ks1))
        ks1     (set/difference ks1 idents)
        ks2     (set/difference ks2 idents)
        renames (reduce (fn [renames old-k]
                          (let [v (get m1 old-k)]
                            (if-let [new-k (first (filter #(= (get m2 %) v) ks2))]
                              (conj renames [old-k new-k])
                              renames)))
                        {} ks1)
        ks1     (set/difference ks1 (set (keys renames)))
        ks2     (set/difference ks2 (set (vals renames)))
        assocs  (select-keys m2 (set/difference ks2 ks1))
        dissocs (set/difference ks1 ks2)
        updated (->> (set/intersection ks1 ks2)
                     (remove #(= (get m1 %) (get m2 %))))
        updates (zipmap updated (map #(diff (get m1 %) (get m2 %)) updated))]
    {:assoc  assocs
     :dissoc dissocs
     :rename renames
     :update updates}))

(defn- set-diff [s1 s2]
  (let [added   (set/difference s2 s1)
        removed (set/difference s1 s2)]
    {:add added :remove removed}))

(defn- seq-diff [s1 s2]
  (map-diff (seq->map s1) (seq->map s2)))

(defn diff
  "Given two forms `f1` and `f2`, returns a patch – a map representing the
   changes that must be made to `f1` in order to produce `f2`."
  [f1 f2]
  (let [forms [f1 f2]]
    (cond (= f1 f2) {:keep f1}
          (every? map? forms) (map-diff f1 f2)
          (every? set? forms) (set-diff f1 f2)
          (every? sequential? forms) (seq-diff f1 f2)
          :else {:replace f2})))
