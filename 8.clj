(require '[clojure.string :as str])

(def digits-map
  {0 "abcefg"
   1 "cf"
   2 "acdeg"
   3 "acdfg"
   4 "bcdf"
   5 "abdfg"
   6 "abdefg"
   7 "acf"
   8 "abcdefg"
   9 "abdfg"})

(def input
  (str/split (slurp "input/8.txt") #"\n"))

;;
;; 1
;; Care about only the output values which have length of 2, 3, 4 or 7
;; which will render 1, 4, 7, 8
;;
(def outputs
  (map
   (fn [line]
     (filter not-empty (str/split
                        (second (str/split line #"\|"))
                        #" ")))
   input))

(count
 (filter (fn [num]
           (some #{num} #{2 3 4 7}))
         (map
          count
          (flatten outputs))))

;;
;; 2
;;
