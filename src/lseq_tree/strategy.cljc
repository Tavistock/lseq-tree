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
  "takes a strategy, a node-pair, a extra, and a base.
  returns a digit"
  [{:keys [boundary]} [lower _] {:keys [interval level]} base]
  (let [min-step (min boundary interval)]
    (int (+ (nth-id lower level base)
            (floor (inc (* (rand) min-step)))))))

(defn b+
  "returns a digit"
  [strategy pair {:keys [site counter] :as id} base]
  (let [extra (extra-data pair base)
        digit (b+digit strategy pair extra base)]
    (candidate->id (id digit site counter) pair extra base)))

(defn b-digit
  [{:keys [boundary]} [lower upper] {:keys [interval level]} base]
  (loop [lower lower, upper upper
         depth 0, acc 0, common-root true, low-greater false]
    (if (> depth level)
      (let [min-step (min boundary interval)]
      (int (- acc (- (floor (* (rand) min-step)) (if low-greater 0 1)))))
      (let [low-val (or (-> lower :triple :path) 0)
            up-val (or (-> upper :triple :path) 0)
            diverge? (and common-root (not (== low-val up-val)))
            common-root (if diverge? false common-root)
            low-greater (if diverge? (> low-val up-val) low-greater)
            next-val (if low-greater (int (dec (pow 2 (bit base depth)))) up-val)
            acc1 (+ acc next-val)
            acc2 (if-not (= depth level)
                   (bit-shift-left acc1 (bit base (inc depth)))
                   acc1)]
        (recur (-> lower :children first), (-> upper :children first)
               (inc depth) acc2, common-root, low-greater)))))


(defn b-
  [strategy pair {:keys [site counter]} base]
  (let [extra (extra-data pair base)
        digit (b-digit strategy pair extra base)]
    (candidate->id (id digit site counter) pair extra base)))

(defn digit->path
  [digit level depth base]
  (int (mod (bit-shift-right
              digit (- (sum base level) (sum base depth)))
            (pow 2 (bit base depth)))))

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
       (let [path (digit->path digit level depth base)
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
       (id digit (conj sites site) (conj counters counter))))))
