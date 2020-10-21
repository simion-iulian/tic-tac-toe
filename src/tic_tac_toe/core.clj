(ns tic-tac-toe.core
  (:gen-class))

(def X :x)
(def O :o)

(def statuses #{:draw :x-win :o-win :ongoing})

(defn winning-line?
  [player line]
  {:pre [(keyword? player)
         (some #{player} #{X O})
         (= 3 (count line))]}
  (or (every? (partial = player) line)))

(defn lines-to-cols [board]
  (->> board
       (apply interleave)
       (partition 3)
       (map #(into [] %))))

(defn diagonal1 
  [board]
  (let [[[c1 _ _] 
         [_  m _] 
         [_  _ c2]] board]
    [c1 m c2]))

(defn diagonal2
  [board]
  (let [[[_  _ c1]
         [_  m _]
         [c2 _ _]] board]
    [c1 m c2]))

(defn neighbors
  "Deltas local describes what the neighbors are.
   In this case they are on spot away horizontally and vertically but not diagonally
   Then it generates all the neighbor coordinates given a size(or distance) for what would be considered a neighbor"
  ([yx]
   (neighbors [;; vertical and horizontal
               [-1  0]
               [1  0]
               [0 -1]
               [0  1]
               
               ;; diagonally
               [1   1]
               [-1 -1]
               [1  -1]
               [-1  1]]
              yx))
  ([deltas yx]
   (->> deltas
        (map #(vec (map + yx %)))
        (filter (fn [new-yx]
                  (every? #(< -1 % 3) new-yx))))))

(defn analyse 
  [board]
  {:pre [(every? (fn [line] 
                   (= 3 (count line)))
                 board)]}
  (let [board-with-diagonals (-> board
                                 (conj (diagonal1 board))
                                 (conj (diagonal2 board))
                                 (concat (lines-to-cols board)))
        x-wins (some (partial winning-line? :x) board-with-diagonals)
        o-wins (some (partial winning-line? :o) board-with-diagonals)
        can-continue? (some #{:_} (apply concat board))]
    (cond x-wins
          :x

          o-wins
          :o

          (and (not x-wins)
               (not o-wins)
               can-continue?)
          :ongoing
          
          :else :draw)))

(defn add-to-board [board player mark]
  (assoc-in board mark player))

(defn next-moves
  ;; look at the empty spots. 
  ;; see if it is possible to add a move for the player at mark
  [board [x y]]
  {:pre [(every? (fn [line](= 3 (count line))) board)]}
  (let [player-at-mark (get-in board [x y])
        possible-next-moves (atom [])]
    (loop [row-idx 2]
      (loop [col-idx 2]
        (when (= (get-in board [row-idx col-idx]) player-at-mark)
          (->> (neighbors [row-idx col-idx])
               (filter (fn [coord]
                       (= (get-in board coord) :_)))
               (swap! possible-next-moves concat)))
        (if (zero? col-idx)
          col-idx
          (recur (dec col-idx))))
      (if (zero? row-idx)
        (map (partial add-to-board board player-at-mark)
             @possible-next-moves)
        (recur (dec row-idx))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
