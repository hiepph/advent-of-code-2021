(require '[clojure.string :as str])

(def input
  (str/split (slurp "input/5.txt") #"\n"))

(defn to-couple-coord
  "Transform line input into [x1 y1 x2 y2]"
  [line]
  (map
   #(Integer/parseInt %)
   (filter (complement empty?)
           (str/split line #"[\s|\-|>|,]"))))

;; [[x1 y1 x2 y2] [...] ...]
(def couple-coords
  (map
   to-couple-coord
   input))

;;
;; 1
;;
;; Create a 2d diagram with initial 0 values.
;;
;; 1st loop: increase by 1 for each couple of corresponding couple coordinates
;; provided by the input.
;; (only consider where x1 = x2 or y1 = y2)
;; Note that x1 can be larger than x2, y1 can be larger than y2
;;
;; 2nd loop: count all values which are larger than 1
;;
(def n
  (inc (apply max (flatten couple-coords))))

(def diagram
  (into [] (repeat n
                   (into [] (repeat n 0)))))

(defn gen-range
  "Helper to generate a range, either from smaller number to larger number
  or from larger number to smaller number."
  [a b]
  (if (< a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn mark-diagram-with-coords
  "Mark diagram from a list of coords [x y]"
  [diagram coords]
  (reduce
   (fn [res [x y]]
     (update-in res [y x] inc))
   diagram
   coords))

(defn mark-diagram
  "Mark diagram derived from a couple of coord [x1 y1 x2 y2]"
  [diagram couple-coord]
  (let [[x1 y1 x2 y2] couple-coord]
    (if (= y1 y2)
      ;; horizontally
      (mark-diagram-with-coords
       diagram
       (map #(vector % y1) (gen-range x1 x2)))

      ;; vertically
      (mark-diagram-with-coords
       diagram
       (map #(vector x1 %) (gen-range y1 y2))))))

(def final-diagram
  (loop [diagram diagram
         couple-coords (filter (fn [couple-coord]
                          (let [[x1 y1 x2 y2] couple-coord]
                            (or (= x1 x2)
                                (= y1 y2))))
                        couple-coords)]
    (if (empty? couple-coords)
      diagram
      (recur (mark-diagram diagram (first couple-coords))
             (rest couple-coords)))))

(def sol1
  (count (filter #(> % 1)
                 (flatten final-diagram))))

sol1


;;
;; 2
;; now also consider diagonal lines when marking in the 1st loop
;;
(defn mark-diagram-2
  [diagram couple-coord]
  (let [[x1 y1 x2 y2] couple-coord]
    (if (or (= x1 x2)
            (= y1 y2))
      (mark-diagram diagram couple-coord)
      ;; diagonally
      (mark-diagram-with-coords
       diagram
       (zipmap (gen-range x1 x2) (gen-range y1 y2))))))

(def final-diagram-2
  (loop [diagram diagram
         couple-coords couple-coords]
    (if (empty? couple-coords)
      diagram
      (recur (mark-diagram-2 diagram (first couple-coords))
             (rest couple-coords)))))

(def sol2
  (count (filter #(> % 1)
                 (flatten final-diagram-2))))

sol2
