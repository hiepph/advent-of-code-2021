(require '[clojure.string :as str])

(def input
  (map
   (fn [line]
     (let [comps (str/split line #" ")]
       [(first comps)
        (Integer/parseInt (second comps))]))
   (str/split (slurp "input/2.txt") #"\n")))

;;
;; 1
;;
;; Simple reduction to store the result into a hash-map
;; of x (horizontal) and y (vertical) position.
;; So forward would increase x,
;;    up would decrease y,
;;    down would increase y.
;;
;; 0 ----- x
;; |
;; |
;; |
;; y
;;
;;
(def final-pos
  (reduce
   (fn [res [direction step]]
     (cond
       (= direction "forward") (update res :x + step)
       (= direction "down") (update res :y + step)
       (= direction "up") (update res :y - step)))
   {:x 0 :y 0}
   input))

(* (:x final-pos) (:y final-pos))

;;
;; 2
;; Add an additional :aim key to the map
;; So down would increase :aim,
;;    up would decrease :aim,
;;    forward would increase x pos by ~step~ AND increase y pos by ~:aim * step~
;;
(def final-pos-2
  (reduce
   (fn [res [direction step]]
     (cond
       (= direction "down") (update res :aim + step)
       (= direction "up") (update res :aim - step)
       (= direction "forward") (update
                                (update res :x + step)
                                :y + (* (:aim res) step))))
   {:x 0 :y 0 :aim 0}
   input))

(* (:x final-pos-2) (:y final-pos-2))
