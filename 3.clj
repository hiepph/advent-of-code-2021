(require '[clojure.string :as str])

(def input
  (map
   (fn [s]
     (map #(Character/digit % 2) s))
   (str/split (slurp "input/3.txt") #"\n")))

;;
;; 1
;; Gamma rate: most common bit group by cols
;; Epsilon rate: least common bit group by cols
;;
(def cols
  (for [i (range (count (nth input 0)))]
    (map #(nth % i) input)))

(defn most-common-bit
  [coll]
  (key
   (apply max-key val (frequencies coll))))

(defn least-common-bit
  [coll]
  (key
   (apply min-key val (frequencies coll))))

(def gamma-rate
  (Integer/parseInt
   (str/join (map most-common-bit cols))
   2))

(def epsilon-rate
  (Integer/parseInt
   (str/join (map least-common-bit cols))
   2))

(println (* gamma-rate epsilon-rate))

;;
;; 2
;; oxygen rate: most common bit group by cols, but reduced gradually;
;;              1 is considered higher priority.
;; co2 rate: least common bit group by cols, but reduced gradually;
;;           0 is considered higher priority.
;;
(defn get-col
  [coll idx]
  (map #(nth % idx) coll))

(defn most-common-bit-2
  [coll]
  (let [freq (frequencies coll)]
    (if (= (get freq 1) (get freq 0))
      1
      (key (apply max-key val freq)))))

(defn least-common-bit-2
  [coll]
   (let [freq (frequencies coll)]
     (if (= (get freq 1) (get freq 0))
       0
       (key (apply min-key val freq)))))

(def oxygen-rate
  (->
   (reduce
    (fn [res i]
      (let [mcb (most-common-bit-2 (get-col res i))]
        (filter
         #(= (nth % i) mcb)
         res)))
    input
    (range (count (nth input 0))))
   first
   str/join
   (Integer/parseInt 2)))

(def co2-rate
  (->
   (reduce
    (fn [res i]
      (let [lcb (least-common-bit-2 (get-col res i))]
        (filter
         #(= (nth % i) lcb)
         res)))
    input
    (range (count (nth input 0))))
   first
   str/join
   (Integer/parseInt 2)))

(println (* oxygen-rate co2-rate))
