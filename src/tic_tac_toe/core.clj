(ns tic-tac-toe.core)

(def statuses #{:draw :x-win :o-win :ongoing})

(defn is-board?
  [board]
  (every? (fn [line]
            (= 3 (count line)))
          board))

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
         [_  m  _]
         [c2 _  _]] board]
    [c1 m c2]))

(def all-positions
  (->> (for [x     (range 0 3)
             y     (range 0 3)
             :when (not= x y 0)]
         [x y])
       (into [])))


(defn analyse
  [board]
  {:pre [(is-board? board)]}
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

(defn add-move [board player mark]
  (assoc-in board mark player))

(defn player-next-moves
  ;; look at the empty spots. 
  ;; see if it is possible to add a move for the player at mark
  [board player]
  {:pre [(is-board? board)]}
  (let [possible-next-moves (atom [])]
    (when (= :_ player)
      (throw (Exception. "No player at mark. Pick a position with a player.")))
    (loop [row-idx 2]
      (loop [col-idx 2]
        (when (= (get-in board [row-idx col-idx]) player)
          (->> all-positions
               (filter (fn [coord]
                         (= (get-in board coord) :_)))
               (swap! possible-next-moves concat)))
        (if (zero? col-idx)
          col-idx
          (recur (dec col-idx))))
      (if (zero? row-idx)
        row-idx
        (recur (dec row-idx))))
    (->> @possible-next-moves
         (into #{})
         (map (partial add-move board player)))))

;; :x is best to start in a corner
(def o-win-move [[:x :o :_]
                 [:_ :o :_]
                 [:x :o :x]])

(def near-emtpy-board [[:x :_ :_]
                       [:_ :_ :_]
                       [:_ :_ :_]])

(def example-board2 [[:x :x :_]
                     [:x :o :o]
                     [:o :x :o]])

(defn x-next-moves
  [board]
  {:pre [(is-board? board)]}
  (let [min-player-moves (player-next-moves board :o)
        deadly-boards (->> min-player-moves
                           (map (fn [board]
                                  {(analyse board) board}))
                           (keep :o))
        prevent-opposite-player-positions (atom [])]
    (mapv (fn [deadly]
            (loop [row-idx 2]
              (loop [col-idx 2]
                 ;; when discovering position add it 
                (when (and (= (get-in deadly [row-idx col-idx]) :o)
                           (= (get-in board [row-idx col-idx]) :_))
                  (swap! prevent-opposite-player-positions concat [[row-idx col-idx]]))
                (if (zero? col-idx)
                  col-idx
                  (recur (dec col-idx))))
              (if (zero? row-idx)
                row-idx
                (recur (dec row-idx)))))
          deadly-boards)
    (set @prevent-opposite-player-positions)))'; 

(defn game
  "x y is your move and you play O. X is the starting player"
  [board [x y]]
  {:pre [(is-board? board)]}
  (let [board-with-move (add-move board :o [x y])
        state (analyse board-with-move)]
    (case state
      :ongoing
      {:board (add-move  board-with-move :x (first (x-next-moves board-with-move)))
       :state (analyse (add-move  board-with-move :x (first (x-next-moves board-with-move))))}
      ;; else
      {:board board-with-move
       :state state})))

(def example-board 
  [[:_ :_ :_]
   [:x :_ :_]
   [:x :_ :o]])

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
