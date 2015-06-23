(ns lseq-tree.strategy
  (:require [lseq-tree.base :as l-base]
            [lseq-tree.identifier :as l-ident]
            [lseq-tree.util :refer [floor pow]]))

(defrecord Strategy [boundary])

(defn strategy
  ([] (->Strategy 10))
  ([boundary] (->Strategy boundary)))

(defn prefix
  [limb depth base]
  (loop [limb limb
         current-depth 0
         id 0]
    (if (> current-depth depth)
      id
      (if limb
        (recur (first (:children limb))
               (inc current-depth)
               (+ (bit-shift-left id (l-base/bit base current-depth))
                  (:path (:triple limb))))
        (recur nil
               (inc current-depth)
               (bit-shift-left id (l-base/bit base current-depth)))))))

(defn paths
  [limb]
  (loop [limb limb xs ()]
    (if limb
      (reverse xs)
      (recur (first (:children limb))
             (cons (:path (:triple limb)) xs)))))

(defn take-zeros
  [depth xs]
  (take depth (lazy-cat xs (repeat 0))))

(defn alloc
  [strategy lower upper base]
  (let [[depth interval],
        (loop [current-interval 0
               current-depth 0]
          (if (> current-interval 0)
            [(dec current-depth) current-interval]
            (recur (l-base/interval base lower upper current-depth)
                   (inc current-depth))))
        step (min (:boundary strategy) interval)
        digit,
        (if (= (mod depth 2) 0)
          (+ (prefix lower depth base) (inc (rand-int step)))
          (- (postfix lower upper depth base) (inc (rand-int step))))]
    [digit depth]))

(defn candidate->id
  ([site counter lower upper
    [digit depth]
    base]
   (loop [sites [], counters []
          {{low-p :path low-s :site low-c :counter} :triple} lower
          {{up-p :path up-s :site up-c :counter} :triple} upper
          current-depth 0]
     (if (<= current-depth depth)
       (let [path (l-ident/digit->path digit base current-depth depth)
             [next-s next-c]
             (cond
               (= path up-p) [(conj sites up-s)
                              (conj counters up-c)]
               (= path low-p) [(conj sites low-s)
                               (conj counters low-c)]
               :default [sites counters])]
         (recur next-s, next-c
                (first (:children lower))
                (first (:children upper))
                (inc current-depth)))
       (l-ident/id digit (conj sites site) (conj counters counter))))))

(defn tree-alloc
  [{:keys [site counter strategy base]} lower upper]
  (candidate->id
    site counter lower upper (alloc strategy lower upper base) base))

