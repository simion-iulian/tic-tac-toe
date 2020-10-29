(ns tic-tac-toe.core)

(defn is-board?
  [board]
  (every? (fn [line]
            (= 3 (count line)))
          board))

(defn winning-line?
  [player]
  {:pre [(keyword? player)
         (some #{player} #{:x :o})
         ]}
  (fn [line]
    {:pre [(= 3 (count line))]}
    (or (every? (partial = player) line))))

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
         [_  m  _]
         [c2 _  _]] board]
    [c1 m c2]))

(def all-positions
  (->> (for [x     (range 0 3)
             y     (range 0 3)
             :when (not= x y 0)]
         [x y])
       (into [])))

(defn with-diagonals 
  [board]
  (-> board
      (conj (diagonal1 board))
      (conj (diagonal2 board))
      (concat (lines-to-cols board))))

(defn analyse
  [board]
  {:pre [(is-board? board)]}
  (let [all-directions (with-diagonals board)
        x-wins (some (winning-line? :x) all-directions)
        o-wins (some (winning-line? :o) all-directions)
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

(defn add-move [board player mark]
  (assoc-in board mark player))

(defn empty-marks
  [m col line]
  (->> line
       (reduce-kv (fn [coordinate row mark]
          (if (= mark :_)
            (conj coordinate [col row])
            coordinate)) [])
       (concat m)))

(defn player-next-moves
  ;; look at the empty spots. 
  ;; see if it is possible to add a move for the player at mark
  [board player]
  {:pre [(is-board? board)
         (keyword? player)
         (some #{player} #{:x :o})]}
  (reduce-kv empty-marks [] board))


;; :x is best to start in a corner
(def o-win-move [[:x :o :_]
                 [:_ :o :_]
                 [:x :o :x]])

(player-next-moves o-win-move :x)

(def near-emtpy-board [[:x :_ :_]
                       [:_ :_ :_]
                       [:_ :_ :_]])

(def example-board2 [[:x :x :_]
                     [:x :o :o]
                     [:o :x :o]])

(defn x-next-moves
  [board]
  {:pre [(is-board? board)]}
  (let [max-player-moves (player-next-moves board :x)
        winning-boards (->> max-player-moves
                            (keep (fn [move]
                                   (when (= :x (analyse (add-move board :x move)))
                                     move))))]
    winning-boards))

(defn game
  "x y is your move and you play O. X is the starting player"
  [board [x y]]
  {:pre [(is-board? board)]}
  (let [board-with-move (add-move board :o [x y])
        state (analyse board-with-move)
        x-moves (first (x-next-moves board-with-move))
        with-x-move (if (seq x-moves)
                      (add-move board-with-move :x x-moves)
                      board-with-move)]
    (println "with x move")
    (println with-x-move)
    (case state
      :ongoing
      {:board with-x-move
       :state (analyse with-x-move)}
      ;; else
      {:board board-with-move
       :state state})))

(def example-board 
  [[:_ :_ :_]
   [:x :_ :_]
   [:x :_ :o]])

(x-next-moves example-board)

(add-move example-board :x (first (x-next-moves example-board)))

(game example-board [0 0])

(-> example-board
    (game [0 0])
    :board
    (game [0 2]))
;; => 
;; {:board 
;;  [[:o :_ :o] 
;;   [:x :x :x] 
;;   [:x :_ :o]], 
;;  :state :x }

(-> example-board
    (game [0 0])
    :board
    (game [1 2]))
;; => {
;; :board 
;; [[:o :_ :x] 
;;  [:x :x :o] 
;;  [:x :_ :o]], 
;;  :state :x}
