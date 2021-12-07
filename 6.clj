(require '[clojure.string :as str])

(def input
  (slurp "test/6.txt"))

;;
;; 1
;; Simply traverse the fishes, decrease each timer.
;; Add 8 when the timer get to 0, then reset it to 6 in next step.
;;

(def fishes
  (reverse (map
            #(Integer/parseInt %)
            (str/split input #"[,|\n]"))))

(defn tick-one-day
  "What will happen to the fish after one day?"
  [fishes]
  (let [dec-fishes (map dec fishes)
        n-new-fishes (count (filter #(= -1 %) dec-fishes))
        new-fishes (reduce
                    #(cons %2 %1)
                    dec-fishes
                    (repeat n-new-fishes 8))]
    (map #(if (= -1 %) 6 %)
         new-fishes)))

(defn fishes-after-n-day
  [fishes n]
  (first
   (drop n
         (take (inc n) (iterate tick-one-day fishes)))))

(count (fishes-after-n-day fishes 80))


;;
;; 2
;; Don't store the fishes!
;; Switch to count the frequencies of the internal timers instead
;;
;; (count (fishes-after-n-day fishes 256))
