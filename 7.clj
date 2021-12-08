(require '[clojure.string :as str])

(def input
  (slurp "input/7.txt"))

(def positions
  (map #(Integer/parseInt %)
       (str/split input #"[,|\n]")))

;; (def freq
;;   (frequencies pos))

;;
;; 1
;; brute force
;;
(def uniq-positions
  (set positions))

(def sum
  (partial apply +))

(def fuels
  (map
   (fn [pos]
     (sum (for [x positions]
            (Math/abs (- x pos)))))
   (range (apply min positions) (inc (apply max positions)))))

(apply min fuels)

;;
;; 2
;;
(defn sum-of-arithmetic-series
  [low high]
  (let [n (Math/abs (- high low))]
    (* n (/ (+ 1 n) 2))))

(def fuels-2
  (map
   (fn [pos]
     (sum (for [x positions]
            (sum-of-arithmetic-series x pos))))
   (range (apply min positions) (inc (apply max positions)))))

(apply min fuels-2)
