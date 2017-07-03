(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (defn helper [a b]
    (if (nil? b)
      a
      (conj a x b)))
  (if (empty? a-seq) []
      (reduce helper (vector (first a-seq)) (rest a-seq))))
(println (my-interpose [] []))
(defn my-count [a-seq]
  (defn helper [x a-seq]
    (if (nil? a-seq)
      x
      (inc x)))
  (reduce helper 0 a-seq))

(defn my-reverse [a-seq]
  (let [helper (fn [a-seq element]
                 (if (nil? element)
                   a-seq
                   (reduce conj (vector element) a-seq)))]
    (reduce helper [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-element (reduce min a-seq)
        max-element (reduce max a-seq)]
    (vector min-element max-element)))

(defn insert [sorted-seq n]
  (loop [acc []
         seq sorted-seq
         n n]
    (cond (empty? seq) (conj acc n)
          (> (first seq) n) (reduce conj (conj acc n) seq)
          :else (recur (conj acc (first seq)) (rest seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))
(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
   (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f a-seq]
  [:-])
