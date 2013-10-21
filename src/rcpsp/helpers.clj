(ns rcpsp.helpers (:use clojure.set))

; Auxiliary functions
(defn set-range [from to] (set (range from (+ to 1))))

(def nat-nums (map inc (range)))

(defn range-from [from] (drop from (range)))

(defn map2 [f m] (into {} (map (fn [[k v]] [k (f k v)]) m)))

(defn unite-maps [keysym valsym maps] (apply hash-map (mapcat (fn [m] [(m keysym) (m valsym)]) maps)))

(defn sum
  ([coll] (apply + coll))
  ([f coll] (apply + (map f coll))))

;(defn assoc-to-val [m v keys] (if (empty? keys) m (apply assoc m (interleave keys (repeat v)))))
(defn assoc-to-val [m v keys] (zipmap keys (repeat v)))

(defn cons-if-empty [head tail] (if (empty? tail) (cons head tail) tail))

(defn cartesian-prod [A B] (mapcat (fn [a] (map (fn [b] [a b]) B)) A))

(defn index-of [coll elem] (ffirst (filter #(= elem (second %)) (map vector (range) coll))))

(defn tblkey->cell [tbl k]
  (let [row (first k)
        col (second k)
        col-index (+ 1(index-of (first tbl) col))
        row-index (+ 1 (index-of (rest (map first tbl)) row))]
    ((tbl row-index) col-index)))

(defn tbl->pair2num-map [tbl]
  (let [ks (cartesian-prod (rest (map first tbl)) (first tbl))]
    (apply hash-map (mapcat (fn [k] [k (tblkey->cell tbl k)]) ks))))

(def nneg? (comp not neg?))

(defn exists? [pred coll] (not (not-any? pred coll)))

(defn bool->num [cond] (if cond 1 0))

(defn where [pred f coll] (map first (filter (comp pred second) (map #(vector % (f %)) coll))))
(defn first-where [pred f coll] (first (where pred f coll)))

(defn index-ofp [pred coll] (loop [i 0] (if (pred (nth coll i)) i (recur (inc i)))))

(defn fiber [f domain] (fn [x] (where (partial = x) f domain)))

(defn elems-between [start end coll] (->> coll (drop (inc start)) (take (dec (- end start)))))

(defn read-lines [f] (seq (.split (slurp f) "\n")))