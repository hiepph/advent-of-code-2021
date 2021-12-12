(require '[clojure.string :as str]
         '[clojure.set :as set])

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
(def patterns
  (map
   (fn [line]
     (filter not-empty (str/split
                        (first (str/split line #"\|"))
                        #" ")))
   input))

(defn decode-6
  [deduction s]
  (let [left-diff (set/difference s (get deduction 1))]
    (if (= (count left-diff) 5)
      (assoc deduction 6 s)
      (let [left-diff-2 (set/difference s (get deduction 4))]
        (if (= (count left-diff-2) 3)
          (assoc deduction 0 s)
          (assoc deduction 9 s))))))

(defn decode-5
  [deduction s]
  (let [left-diff (set/difference s (get deduction 1))]
    (if (= (count left-diff) 3)
      (assoc deduction 3 s)
      (let [left-diff-2 (set/difference s (get deduction 4))]
        (if (= (count left-diff-2) 3)
          (assoc deduction 2 s)
          (assoc deduction 5 s))))))

(defn decode
  "Args:
  pattern: 1 line of the patterns

  Returns:
  Deduction to the numbers from 0->9"
  [pattern]
  (let [unique-deduction (reduce
                          (fn [res s]
                            (let [len (count s)]
                              (cond
                                (= len 2) (assoc res 1 (set s))
                                (= len 3) (assoc res 7 (set s))
                                (= len 4) (assoc res 4 (set s))
                                (= len 7) (assoc res 8 (set s))
                                :else res)))
                          (into [] (repeat 10 nil))
                          pattern)
        deduction (reduce
                   (fn [res s]
                     (let [len (count s)]
                       (cond
                         (= len 6) (decode-6 res (set s))
                         (= len 5) (decode-5 res (set s))
                         :else res)))
                   unique-deduction
                   pattern)]
    (into {}
          (for [[x s] (map-indexed vector deduction)]
            [s x]))))

(defn decode-output-from-pattern
  "Args:
  pattern: array of string patterns (which corresponds to one line)
  output: array of output patterns (which corresponds to one line)

  Returns:
  Number which is represented by output"
  [pattern output]
  (let [deduction (decode pattern)
        decoded (into [] (map
                          #(get deduction (set %))
                          output))]
    (+ (get decoded 3)
       (* (get decoded 2) 10)
       (* (get decoded 1) 100)
       (* (get decoded 0) 1000))))


(apply +
       (for [[pattern output] (map vector patterns outputs)]
         (decode-output-from-pattern pattern output)))
