(ns lseq-tree.strategy
  (:require [lseq-tree.base :as b]
            [lseq-tree.identifier :as i]
            [lseq-tree.util :refer [floor pow]]))

(defrecord Strategy [boundary])

(defn strategy
  ([] (->Strategy 10))
  ([boundary] (->Strategy boundary)))

(defn paths
  [limb]
  (loop [xs [] {{path :path} :triple more :children} limb]
    (if path
      (recur (conj xs path) (first more))
      xs)))

(defn max-paths [path base]
  (map-indexed
    (fn [index value]
      [(inc index) (if (= :max value)
                     (b/depth-max base index)
                     value)])
    path))

(defn paths->digit
  [path base]
  (let [level (count path)
        reducer (fn [acc [index value]]
                  (if (= level index)
                    (+ acc value)
                    (bit-shift-left
                      (+ acc value) (b/bit base index))))]
    (reduce reducer 0
            (map-indexed max-paths path))))

(defn prefix
  [limb depth base]
  (let [paths (paths limb)
        prefixs (take (inc depth) (lazy-cat paths (repeat 0)))]
    (paths->digit prefixs base)))

(declare upper-path)

(defn postfix
  [pair depth base]
  (paths->digit (upper-path pair depth) base))

;;;; GRAVEYARD
(defrecord ExtraData [interval level])

(defn extra-data
  [[lower upper] base]
  (loop [inter 0
         level 0]
    (if (<= inter 0)
      (recur (b/interval base lower upper level)
             (inc level))
      (->ExtraData (long inter) (dec level)))))

(defn nth-id
  [limb level base]
   (loop [depth 0 limb limb acc 0]
     (if (<= depth level)
       (let [temp-acc (if-let [value (:path (:triple limb))]
                        (+' acc value) acc)
             next-acc (if (>= depth level)
                        temp-acc
                        (bit-shift-left
                          temp-acc (b/bit base (inc depth))))]
         (recur (inc depth) (first (:children limb)) next-acc))
       (long acc))))

(declare candidate->id)

(defn b+digit
  "takes a strategy, a node-pair, a extra, and a base.
  returns a digit"
  [{:keys [boundary]} [lower _] {:keys [interval level]} base]
  (let [min-step (min boundary interval)
        lower-bound (nth-id lower level base)
        rand-step (do (prn lower level base)
                      (long (floor (inc (* (rand) min-step)))))]
    (+' lower-bound rand-step)))

(defn b+
  "returns a digit"
  [strategy pair {:keys [site counter] :as id} extra base]
  (let [digit (b+digit strategy pair extra base)]
    (candidate->id (i/id digit site counter) pair extra base)))


(defn upper-bound [path base]
  (let [level (count path)]
    (letfn [(path-reducer [acc [ind value]]
              (if (= level ind)
                (+ acc value)
                (bit-shift-left (+ acc value) (b/bit base ind))))]
    (reduce path-reducer 0 (max-path path base)))))

(defn upper-path
  "takes a pair of nodes and returns the upper path with max
  eg for nodes with paths [2 5 1] and [2 5] it returns [2 5 :max]"
  [pair level]
  (loop [xs []
         [lower upper :as both] pair
         depth 0]
    (let [upper-p (:path (:triple upper))
          lower-p (:path (:triple lower))]
      (if-not (= upper-p lower-p)
          (if (nil? upper-p)
            (into xs (repeat (i/depth lower) :max))
            (conj xs (dec upper-p)))
        (if (= depth level)
          (conj xs (dec lower-p))
          (recur (conj xs upper-p)
                 (map #(first (:children %)) both)
                 (inc depth)))))))

(defn b-digit
  [{:keys [boundary]}
   pair
   {:keys [interval level]}
   base]
  (let [upper-xs (upper-path pair level)
        upper-limit (upper-bound upper-xs base)]
    upper-limit))

(defn b-
  [strategy pair {:keys [site counter]} extra base]
  (let [digit (b-digit strategy pair extra base)]
    (candidate->id (i/id digit site counter) pair extra base)))

(defn candidate->id
  "id, pair, extra-data, and base"
  ([{:keys [digit site counter]}
    [lower upper]
    {:keys [level]}
    base]
   (loop [sites [], counters []
          {{low-p :path low-s :site low-c :counter} :triple} lower
          {{up-p :path up-s :site up-c :counter} :triple} upper
          depth 0]
     (if (<= depth level)
       (let [path (i/digit->path digit base depth level)
             [next-s next-c]
             (cond
               (= path up-p) [(conj sites up-s)
                              (conj counters up-c)]
               (= path low-p) [(conj sites low-s)
                               (conj counters low-c)]
               :default [sites counters])]
         (recur next-s, next-c
                (-> lower :children first)
                (-> upper :children first)
                (inc depth)))
       (i/id digit (conj sites site) (conj counters counter))))))
