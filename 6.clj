(require '[clojure.string :as str])

(def input
  (slurp "input/6.txt"))

(def fishes
  (reverse (map
            #(Integer/parseInt %)
            (str/split input #"[,|\n]"))))

;;
;; 1
;; 80 days
;;
(def timers
  (reduce
   (fn [res i]
     (update res i inc))
   (into [] (repeat 9 0))
   fishes))

(defn shift-left
  "Shift the timers to the 'left', corresponding to one day,
  Increase in the new timers: #6 and #8 by the original #0.
  e.g. 1 1 2 1 0 0 0 0 0 => 1 2 1 0 0 0 1 0 1"
  [orig]
  (let [n-0 (first orig)
        new (conj (into [] (rest orig)) n-0)]
    (update new 6 + n-0)))

(defn timers-after-n-days
  [timers n]
  (first
   (drop n (take (inc n)
                 (iterate shift-left timers)))))

(apply + (timers-after-n-days timers 80))

;;
;; 2
;; 256 days
;;
(apply + (timers-after-n-days timers 256))
