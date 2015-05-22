(ns lseq-tree.strategy
  (:require [lseq-tree.base :refer [sum bit base interval]]
            [lseq-tree.identifier :refer [id]]))

(defn pow
  [n x]
  (Math/pow n x))

(defn floor
  [n]
  (Math/floor n))

(defrecord Strategy [boundary])

(defn strategy
  ([] (->Strategy 10))
  ([boundary] (->Strategy boundary)))

(defrecord ExtraData [interval level])

(defn extra-data
  [[lower upper] base]
  (loop [inter 0
         level 0]
    (if (<= inter 0)
      (recur (interval base lower upper)
             (inc level))
      (->ExtraData inter (dec level)))))

(defn nth-id
  ([limb level] (nth-id limb level (base)))
  ([limb level base]
   (loop [depth 0 limb limb acc 0]
       (if (<= depth level)
         (let [temp-acc (if-let [value (:path (:triple limb))]
                          (+ acc value) acc)
               next-acc (if (>= depth level)
                          temp-acc
                          (bit-shift-left
                            temp-acc (bit base (inc depth))))]
           (recur (inc depth) (first (:children limb)) next-acc))
         (int acc)))))

(declare candidate->id)

(defn b+digit
  [step prev-node level base]
     (int (+ (nth-id prev-node level base)
             (floor (inc (* (rand) step))))))

(defn b+impl
  "takes a strategy, a node-pair, a extra, and a base.
  returns a digit"
  [{:keys [boundary]} [lower _] {:keys [interval level]} base]
   (let [min-step (min boundary interval)]
     (b+digit min-step lower level base)))

(defn b+
  "returns a digit"
  [strategy
   [lower upper :as pair]
   {:keys [site counter]}
   base]
  (let [extra (extra-data pair base)
        digit (b+impl strategy pair extra base)]
  (candidate->id (id digit site counter)
                 pair extra base)))

(defn b-digit
  []
  nil)


(defn candidate->id
  "id, pair, extra-data, and base"
  ([{:keys [digit site counter]}
    [lower upper]
    {:keys [level]}
    base]
   (let [sum-bit (sum base level)]
     (loop [sites []
            counters []
            {{prev-path :path
              prev-site :site
              prev-counter :counter} :triple
             prev-children :children} lower
            {{next-path :path
              next-site :site
              next-counter :counter} :triple
             next-children :children} upper
            depth 0]
       (if (<= depth level)
         (let [path
               (int (mod (bit-shift-right
                           digit (- sum-bit (sum base depth)))
                         (pow 2 (bit base depth))))
               [next-sites next-counters]
               (cond
                 (= path next-path) [(conj sites next-site)
                                     (conj counters next-counter)]
                 (= path prev-path) [(conj sites prev-site)
                                     (conj counters prev-counter)]
                 :default [sites counters])]
           (recur next-sites
                  next-counters
                  (first prev-children)
                  (first next-children)
                  (inc depth)))
         (id digit
             (conj sites site)
             (conj counters counter)))))))
