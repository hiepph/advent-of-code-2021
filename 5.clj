(require '[clojure.string :as str])

(def input
  (str/split (slurp "input/5.txt") #"\n"))

(defn to-coord
  "Transform line input into [x1 y1 x2 y2]"
  [line]
  (map
   #(Integer/parseInt %)
   (filter (complement empty?)
           (str/split line #"[\s|\-|>|,]"))))

(def coords
  (map
   to-coord
   input))

;;
;; 1
;;
;; Create a 2d diagram with initial 0 values.
;;
;; 1st loop: increase by 1 for each couple of corresponding coordinates
;; provided by the input.
;; (only consider where x1 = x2 or y1 = y2)
;; Note that x1 can be larger than x2, y1 can be larger than y2
;;
;; 2nd loop: count all values which are larger than 1
;;
(def n
  (inc (apply max (flatten coords))))

(def diagram
  (into [] (repeat n
                   (into [] (repeat n 0)))))

(defn mark-diagram
  [diagram coord]
  (let [[x1 y1 x2 y2] coord]
    (if (= y1 y2)
      ;; horizontally
      (let [[x-begin x-end] (sort [x1 x2])]
        (reduce
         (fn [res x]
           (update-in res [y1 x] inc))
         diagram
         (range x-begin (inc x-end))))

      ;; vertically
      (let [[y-begin y-end] (sort [y1 y2])]
        (reduce
         (fn [res y]
           (update-in res [y x1] inc))
         diagram
         (range y-begin (inc y-end)))))))

(def final-diagram
  (loop [diagram diagram
         coords (filter (fn [coord]
                          (let [[x1 y1 x2 y2] coord]
                            (or (= x1 x2)
                                (= y1 y2))))
                        coords)]
    (if (empty? coords)
      diagram
      (recur (mark-diagram diagram (first coords))
             (rest coords)))))

(def sol1
  (count (filter #(> % 1)
                 (flatten final-diagram))))

sol1


;;
;; 2
;; now also consider diagonal lines when marking in the 1st loop
;;
(defn gen-range
  [a b]
  (if (< a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn mark-diagram-2
  [diagram coord]
  (let [[x1 y1 x2 y2] coord]
    (if (or (= x1 x2)
            (= y1 y2))
      (mark-diagram diagram coord)
      ;; diagonally
      (reduce
       (fn [res [x y]]
           (update-in res [y x] inc))
       diagram
       (zipmap (gen-range x1 x2) (gen-range y1 y2))))))

(def final-diagram-2
  (loop [diagram diagram
         coords coords]
    (if (empty? coords)
      diagram
      (recur (mark-diagram-2 diagram (first coords))
             (rest coords)))))

(def sol2
  (count (filter #(> % 1)
                 (flatten final-diagram-2))))

sol2
