(ns game-of-life.core
  (:gen-class))

;; validations for life
;; should expect {:alive bool :life-index int 0-8} as input
(defn rule-1
  [{alive :alive n :life-index :as cell}]
  (if (and alive (< n 2))
    {:alive false}))

(defn rule-2
  [{alive :alive n :life-index :as cell}]
  (if (and alive (<= 2 n) (< n 4))
    {:alive true}))

(defn rule-3
  [{alive :alive n :life-index :as cell}]
  (if (and alive (< 3 n))
    {:alive false}))

(defn rule-4
  [{alive :alive n :life-index :as cell}]
  (if (and (not alive) (= n 3))
    {:alive true}))

(def rules [rule-1 rule-2 rule-3 rule-4])

;; filename should be passed as arg

;; board query/manipulation
;; if pos mod width = 0 then it on left edge
;; if pos mod width = width - 1 then it on right edge
;; if pos / width = 0 then it on right edge
;; if (int (/ pos width)) = rows - 1 then it on right edge

;; can just compose left - above to get left above
(defn find-above-neighbor
  [{:keys [cells width]} {:keys [id] :as cell}]
  (if (or (nil? cell) (>= 0 (int (/ id width))))
    nil
    (nth cells (- id width))))

(defn find-below-neighbor
  [{:keys [cells width height]} {:keys [id] :as cell}]
  (if (or (nil? cell) (<= (dec height) (int (/ id width))))
    nil
    (nth cells (+ id width))))

(defn find-left-neighbor
  [{:keys [cells width]} {:keys [id] :as cell}]
  (if (or (nil? cell) (= 0 (mod id width)))
    nil
    (nth cells (dec id))))

(defn find-right-neighbor
  [{:keys [cells width]} {:keys [id] :as cell}]
  (if (or (nil? cell) (= (dec width) (mod id width)))
    nil
    (nth cells (inc id))))

(defn gen-finds
  [board]
  (let [p-left (partial find-left-neighbor board)
        p-right (partial find-right-neighbor board)
        p-above (partial find-above-neighbor board)
        p-below (partial find-below-neighbor board)]
    [p-left
      p-right
      p-above
      p-below
      (comp p-left p-above)
      (comp p-left p-below)
      (comp p-right p-above)
      (comp p-right p-below)]))

(defn find-neighbors
  "Returns list of a cell's neighbors"
  [board cell]
  (let [p-finds (gen-finds board)]
    (filter (complement nil?) (map #(% cell) p-finds))))

(defn fill-cell-life-index
  [board cell]
  (assoc cell :life-index (count (filter :alive (find-neighbors board cell)))))

(defn fill-board-life-index
  [{:keys [cells] :as board}]
  (assoc board :cells (map (partial fill-cell-life-index board) cells)))

;; should really try to use or macro
(defn update-cell
  [cell]
  (apply merge (cons cell (map #(% cell) rules))))

(defn tick
  [{:keys [cells] :as board}]
  (let [{:keys [cells]} (fill-board-life-index board)]
    (assoc board :cells (into [] (map update-cell cells)))))

;; game creation
(defn read-cell
  [id cell]
  (let [base-cell {:id id :life-index 0}
        input (str cell)]
    (or
     (and (= "o" input) (assoc base-cell :alive true))
     (and (= "." input) (assoc base-cell :alive false)))))

(defn str->cells
  [st]
  (into [] (map read-cell (range 0 (count st)) st)))

(defn read-board
  "Reads file a returns a parsed board"
  [filename]
  (let [file (slurp filename)
        [width height & rows] (clojure.string/split-lines file)]
    {:width (Integer. width)
     :height (Integer. height)
     :cells (str->cells (clojure.string/trim (apply str rows)))}))

;; output
(defn render-cell
  [width {:keys [id alive]}]
  (let [out (if alive "o" ".")
        newl (if (= (dec width) (mod id width)) "\n")]
    (str out newl)))

(defn render-board
  [{:keys [width cells]}]
  (clojure.string/join (map #(render-cell width %) cells)))

;; should probably allow for sleep time to be changed
(defn -main
  [filename & args]
  (let [inital-board (read-board filename)]
    (loop [board inital-board]
      (println (render-board board))
      (Thread/sleep 500)
      (recur (tick board)))))
