(require '[clojure.string :as str])

(def input
  (str/split (slurp "test/9.txt") #"\n"))

;;
;; 1
;; Find the low points - the minimum compared to 4 adjacent locations (u, d, l, r)
;;
(def locations
  (map
   (fn [line]
     (map #(- (int %) (int \0)) line))
   input))

(def limy
  (count locations))

(def limx
  (count (first locations)))

(defn adjacent-coords
  "Given a (y, x) coordinate, return list of current coordinate and
  4 adjacent coordinates: u, d, l, r"
  [coord]
  (let [[y x] coord]
    (filter
     #(not (= coord %))
     [[(max 0 (dec y)) x]
      [(min (dec limy) (inc y)) x]
      [y (max 0 (dec x))]
      [y (min (dec limx) (inc x))]])))

(defn cell-value
  "Return the cell value from [y x] coord"
  [coord]
  (let [[y x] coord]
    (nth (nth locations y) x)))

(defn low-point?
  "Provide a [y x] coordinate, check weather if it is a low point value,
  that is its value is smaller than all of adjacent coords"
  [coord]
  (let [adjacent-pos (adjacent-coords coord)
        adjacent-vals (map cell-value adjacent-pos)
        cur-value (cell-value coord)]
    (every? true? (map #(< cur-value %) adjacent-vals))))

(def low-points
  (filter low-point?
          (for [y (range limy)
                x (range limx)]
            [y x])))

(def low-values
  (map cell-value low-points))

(apply + (map inc low-values))


;;
;; 2
;;
(defn expand
  "Expand the basin from a [y x] coord.
  Returns the count of the expanded basin."
  ([coord] (expand coord {}))
  ([coord visited]
   (let [adjacent-pos (adjacent-coords coord)
         adjacent-vals (map cell-value adjacent-pos)

         cur-value (cell-value coord)

         adjacent-pos-vals (map vector adjacent-pos adjacent-vals)
         adjacent-basin-coords (map first (filter
                                           #(and (= (- (second %) cur-value) 1)
                                                 (not (get visited %)))
                                           adjacent-pos-vals))]
     (if (= (count adjacent-basin-coords) 0)
       1
       (apply + 1 (map #(expand % (assoc
                                   (assoc visited coord true)
                                   % true))
                       adjacent-basin-coords))))))
