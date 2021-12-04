(require '[clojure.string :as str])

(def input
  (str/split (slurp "input/4.txt") #"\n\n"))

(def guesses
  (map
   #(Integer/parseInt %)
   (str/split (first input) #",")))

(def raw-boards (drop 1 input))

(defn parse-board
  [raw-board]
  (let [rows (str/split raw-board #"\n")]
    (map
     (fn [row]
       (map
        #(Integer/parseInt %)
        (filter not-empty (str/split row #" "))))
     rows)))

(def boards
  (map parse-board raw-boards))

;;
;; 1
;; For each guess:
;;   for each board:
;;      Traverse the matrix and mark all the numbers equal the guess.
;;      Do a sanity check. Stop if found a winning board
;;
(defn transpose [m]
  (apply mapv vector m))

(defn win?
  [board]
  (or
   (some true? (map #(every? true? %) board))
   (some true? (map #(every? true? %) (transpose board)))))

(defn mark-row
  "Mark a row with all the numbers equal the guess to true."
  [row guess]
  (map
   #(if (= % guess) true %)
   row))

(defn mark-board
  "Mark a board with all the numbers equal the guess to true."
  [board guess]
  (map
   #(mark-row % guess)
   board))

(defn sum-board
  [board]
  "Sum all the numbers, ignore the 'true' marks"
  (apply +
         (filter (complement true?)
                 (flatten board))))

(def bingo
  (loop [boards boards
         guesses guesses]
    (let [guess (first guesses)
          boards (map #(mark-board % guess) boards)
          win-board (some #(if (win? %) %) boards)]
      (if win-board
        [win-board guess]
        (recur boards (rest guesses))))))
(def win-board (first bingo))
(def win-guess (second bingo))

(def sol1
  (* win-guess
     (sum-board win-board)))

(println sol1)


;;
;; 2
;; Only care about the last win board
;;
(def bingo-2
  (loop [boards boards
         guesses guesses]
    (let [guess (first guesses)
          boards (map #(mark-board % guess) boards)
          not-win-boards (filter (complement win?) boards)]
      (if (empty? not-win-boards)
        [(first boards) guess]
        (recur not-win-boards (rest guesses))))))
(def guess-2 (second bingo-2))
(def last-win-board (first bingo-2))

(def sol2
  (* guess-2
     (sum-board last-win-board)))

(println sol2)
