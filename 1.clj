(def nums
  (map
   #(Integer/parseInt %)
   (clojure.string/split
       (slurp "input/1.txt") #"\n")))

;;
;; 1
;; count the number of times a depth measurement increases
;;
(count
 (filter
  #(> (second %) (first %))
  (partition 2 1 nums)))

;;
;; 2
;; three-measurement sliding window
;;
(def sum-windows
  (map (partial apply +)
       (partition 3 1 nums)))

(count
 (filter
  #(> (second %) (first %))
  (partition 2 1 sum-windows)))
