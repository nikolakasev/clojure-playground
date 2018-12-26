(ns advent-of-code-2018.dec8
  (:require [clojure.string :as str]))

(def license [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(def license-2 [2 3 1 1 0 1 99 2 0 3 10 11 12 1 1 2])

(second license)

(apply + (take-last (second license) license))

(drop 2 (drop-last 3 license))

(def history [[3 6] [1 1] [2 3] [2 5]])

(vec (drop-last 2 history))

(conj (drop-last 2 [[3 6] [1 1] [2 3] [2 5]]) [1 3])

(conj (vec (drop-last 2 history)) [(dec (first (second (reverse history)))) (second (second (reverse history)))])

(defn mark-as-read
  "Drops the last entry from the history and marks a child as read in the parent entry."
  [history]
  (if (and (= 1 (count history)) (= 0 (first (first history))))
    []
    (conj (vec (drop-last 2 history)) [(dec (first (second (reverse history)))) (second (second (reverse history)))])))

(mark-as-read history)

(mark-as-read [[0 3] [1 2]])

(defn get-meta
  [license]
  (loop [history [[(first license) (second license)]]
         rest-of-license (drop 2 license)
         sum 0]
    (if (empty? history)
      sum
      (let [child-nodes (first (last history))
            metadata-entries (second (last history))]
        (if (= 0 child-nodes)
          (recur (mark-as-read history) (drop metadata-entries rest-of-license) (+ sum (apply + (take metadata-entries rest-of-license))))
          (recur (conj history [(first rest-of-license) (second rest-of-license)]) (drop 2 rest-of-license) sum))))))

(get-meta license)

(def input (map read-string (str/split (slurp "src/advent_of_code_2018/input-dec8.txt") #" ")))

;36027, solved P1
(get-meta input)

(def graph {:A [:B :C]
            :B [:A]
            :C [:A :D]})

(defn visited?
  "Predicate which returns true if the node v has been visited already, false otherwise."
  [v coll]
  (some #(= % v) coll))

(defn find-neighbors
  "Returns the sequence of neighbors for the given node"
  [v coll]
  (get coll v))

(defn graph-dfs
  "Traverses a graph in Depth First Search (DFS)"
  [graph v]
  (loop [stack   (vector v) ;; Use a stack to store nodes we need to explore
         visited []]        ;; A vector to store the sequence of visited nodes
    (if (empty? stack)      ;; Base case - return visited nodes if the stack is empty
      visited
      (let [v           (peek stack)
            neighbors   (find-neighbors v graph)
            not-visited (filter (complement #(visited? % visited)) neighbors)
            new-stack   (into (pop stack) not-visited)]
        (if (visited? v visited)
          (recur new-stack visited)
          (recur new-stack (conj visited v)))))))

(graph-dfs graph :A)

;calculate the value from the history and metadata
(apply + (filter #(not (nil? %)) (map #(get {:1 33 :2 0 :3 2} ((comp keyword str) %)) [1 1 4])))

(get {:1 1 :2 1 :3 2} (keyword "2"))

(get (conj {:1 1} {:2 33}) :2)

;the last child has the highest key number
(last (sort (map (comp read-string name key) {:1 1 :2 1 :3 2})))

(defn add-value-to-node
  [history
   value]
  (let [parent (last history)
        latest-child-if-any (last (sort (map (comp read-string name key) (:values parent))))
        latest-child (if latest-child-if-any latest-child-if-any 0)
        ;add the value and decrease the number of children
        parent-with-value (update (update parent :values #(conj % {((comp keyword str) (inc latest-child)) value})) :children dec)]
    (conj (vec (drop-last history)) parent-with-value)));(conj (vec (drop-last 2 history)) [(dec (first (second (reverse history)))) (second (second (reverse history)))])))

(def hist [{:children 2 :metadata 1 :values {:1 33}} {:children 1 :metadata 1 :values {:1 2}} {:children 1 :metadata 1 :values {:1 3}}])

(defn value-from-metadata
  [value-map
   references]
  ;filter out nil from non-existent keys
  (apply + (filter #(not (nil? %))
                   ;turn the number into string, convert into a key after that
                   (map #(get value-map ((comp keyword str) %))
                        references))))

;sum the first node twice with the second and the ninth
;the first node value is 33, the second is 0, the ninth doesn't exist
(value-from-metadata {:1 33 :2 0} [1 1 2 9])

(defn get-value
  [license]
  (loop [history [{:children (first license) :metadata (second license) :values {}}]
         rest-of-license (drop 2 license)]
    (if (and (= 1 (count history)) (= 0 (:children (first history))))
      (value-from-metadata (:values (first history)) (take (:metadata (first history)) rest-of-license))
      (let [parent (last history)
            metadata-entries (:metadata parent)]
        (if (= 0 (:children parent))
          (if (empty? (:values parent))
            ;this is a leaf node, it's value is the sum of its metadata entries
            (recur (add-value-to-node (drop-last history) (apply + (take metadata-entries rest-of-license)))
                   (drop metadata-entries rest-of-license))
            ;calculate using the metadata entries as references to children
            (let [references (take metadata-entries rest-of-license)
                  value (value-from-metadata (:values parent) references)]
              (recur (add-value-to-node (drop-last history) value)
                     (drop metadata-entries rest-of-license))))
          (recur (conj history {:children (first rest-of-license) :metadata (second rest-of-license) :values {}})
                 (drop 2 rest-of-license)))))))

;23960 solved P2
(get-value input)
