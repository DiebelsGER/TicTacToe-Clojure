(ns clojure_exercise.core)

(def win-sets
  [[0 1 2], [3 4 5], [6 7 8], [0 3 6], [1 4 7], [2 5 8], [0 4 8], [2 4 6]])

(defn- winner-exists-on-spaces? [board spaces]
  (and
    (apply = (map #(board %) spaces))
    (not (= " " (board (first spaces))))))

(defn- winner-on-spaces [board spaces]
  (if (winner-exists-on-spaces? board spaces)
    (board (first spaces))
    nil))

(defn- indexed [coll]
  (map vector coll (iterate inc 0)))

(defn- index-filter [pred coll]
  (for [[spot i] (indexed coll) :when (pred spot)] i))

;;; public functions

(defn make-board [] (vec (repeat 9 " ")))

(defn empty-square? [square]
  (= " " square))

(defn full? [board] (not-any? empty-square? board))

(defn winner [board] (some #(winner-on-spaces board %) win-sets))

(defn game-over? [board] (boolean (or (winner board) (full? board))))

(defn place-on-board [board spot mark] (assoc board spot mark))

(defn next-mark [mark] (if (= "X" mark) "O" "X"))

(defn valid-move? [board move]
  (try (= (board move) " ")
    (catch Exception e false)))

(defn final-message [winner]
  (if (nil? winner)
    "Deuce!"
    (str winner " Wins!")))

(defn empty-squares [board]
  (index-filter empty-square? board))

;;; basic utility functions
(defn include? [coll item]
  (some #(= item %) coll))

(defn read-int []
  (try (Integer/parseInt (read-line))
    (catch NumberFormatException e nil)))

(def user-message println)
(defn user-prompt [x] (print x) (flush))

(defn- console-suffix [x]
  (cond (> x 7) nil
    (= 2 (mod x 3)) "\n-----------\n"
    :else "|"))

(defn- board-str [board]
  (apply str
    (map
      #(str " " (board %) " " (console-suffix %))
      (range 9))))

(defn- instructions-str []
  (str "\n"
    "Welcome to Clojure-Tic-Tac-Toe!\n"
    "Use the numbers below for moves:\n\n"
    (board-str (vec (range 1 10)))
    "\n"))

(defn- get-valid-move [board player]
  (let [move ((:mover player) board (:player-mark player))]
    (if (valid-move? board move)
      (place-on-board board move (:player-mark player))
      (recur board player))))

;;; public functions

(defn mover [board mark]
  (user-message)
  (user-message (board-str board))
  (user-message)
  (user-prompt "Pick your space (1-9): ")
  (flush)
  (let [input (read-int)]
    (if (and input (include? (range 1 10) input))
      (dec input)
      (recur board mark))))

(def player-marks
  { 1 [{:player-mark "X" :mover mover} {:player-mark "O" :mover mover}]})

(defn set-player-marks []
  (player-marks))

(defn console-play
  ([]
    (user-message (instructions-str))
    (let [[player1 player2] (player-marks 1)]
      (console-play (make-board) player1 player2)))
  ([board current-player next-player]
    (if (game-over? board)
      (do
        (user-message "\n" (final-message (winner board)) "\n")
        (user-message (board-str board)))
      (recur
        (get-valid-move board current-player) next-player current-player))))

(console-play)
