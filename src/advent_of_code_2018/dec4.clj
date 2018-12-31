(ns advent-of-code-2018.dec4
  (:require [clojure.string :as str]))

(def input-small (str/split-lines "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"))

(defn guard-awakes
  [guard
   asleep-minute
   slept-minutes
   guard-map]
  (update-in (update-in guard-map [guard :asleep] (fnil #(+ % slept-minutes) 0))
             [guard :minutes] (fnil #(concat % (range asleep-minute (+ asleep-minute slept-minutes))) ())))

(def m {{99 {:minutes (list 11 12 13), :asleep 3}} {10 {:minutes (), :asleep 8}}})

(guard-awakes 99 36 10 m)

(sort-by (comp :asleep second) (guard-awakes 99 36 10 {9 {:asleep 5 :minutes (list 1)}}))

(defn watch-guards
  [journal-entries]
  (let [guard-regex #".* Guard #(\d+) .*"
        asleep-regex #".*:0?(\d+)] falls asleep"
        wakes-regex #".*:0?(\d+)] wakes up"]
    (loop [guard nil
           asleep nil
           guard-map {}
           entries-left journal-entries]
      (if (empty? entries-left)
        (let [guard (last (sort-by (comp :asleep second) guard-map))]
          [(* (first guard)
              (first (last (sort-by second (frequencies ((comp :minutes second) guard))))))
           guard-map])
        (let [entry (first entries-left)
              guard? (re-matches guard-regex entry)
              asleep? (re-matches asleep-regex entry)
              awake? (re-matches wakes-regex entry)]
          (if guard?
            (recur (read-string (last guard?))
                   nil
                   guard-map
                   (rest entries-left))
            (if asleep?
              (recur guard
                     (read-string (last asleep?))
                     guard-map
                     (rest entries-left))
              (if awake?
                (let [awake (read-string (last awake?))]
                  (recur guard
                         nil
                         (guard-awakes guard asleep (- awake asleep) guard-map)
                         (rest entries-left)))))))))))

(read-string (last (re-matches #".* Guard #(\d+) .*" "[1518-11-05 00:03] Guard #99 begins shift")))
(re-matches #".*:0?(\d+)] falls asleep" "[1518-11-05 00:15] falls asleep")
(re-matches #".*:0?(\d+)] wakes up" "[1518-11-05 00:55] wakes up")

(watch-guards input-small)

(def input (str/split-lines (slurp "src/advent_of_code_2018/input-dec4.txt")))
;125444 solves P1
(watch-guards input)

(def g (second (watch-guards input)))

;(* 733 25) solves P2
(last (sort-by (comp second second)
               (map (fn [g] [(first g) (last ((partial sort-by second)
                                              (frequencies ((comp :minutes second) g))))])
                    g)))

;how to update in a nested structure
(update-in {:a {:b {:c 2}}} [:a :b :c] + 20)
