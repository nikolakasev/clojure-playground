(ns advent-of-code-2018.dec4
  (:require [clojure.string :as str]))

(re-seq #"\d+" "[1518-02-27 23:58] Guard #1667 begins shift")

(def regex #"\d+")

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
  guard-map)

(defn watch-guards
  [journal-entries]
  (let [guard-regex #".* Guard #(\d+) .*"
        asleep-regex #".*:(\d+)] falls asleep"
        wakes-regex #".*:(\d+)] wakes up"]
    (loop [guard nil
           asleep nil
           awake nil
           guard-map {}
           entries-left journal-entries]
      (if (empty? entries-left)
        guard-map
        (let [entry (first entries-left)
              guard? (re-matches guard-regex entry)
              asleep? (re-matches asleep-regex entry)
              awake? (re-matches wakes-regex entry)]
          (if guard?
            (recur (read-string (last guard?))
                   nil
                   nil
                   guard-map
                   (rest entries-left))
            (if asleep?
              (recur guard
                     (read-string (last asleep?))
                     nil
                     guard-map
                     (rest entries-left))
              (if awake?
                (let [awake (read-string (last awake?))]
                  (recur guard
                         asleep
                         awake
                         (guard-awakes guard asleep (- awake asleep) guard-map)
                         (rest entries-left)))))))))))

(read-string (last (re-matches #".* Guard #(\d+) .*" "[1518-11-05 00:03] Guard #99 begins shift")))
(re-matches #".*:(\d+)] falls asleep" "[1518-11-05 00:45] falls asleep")
(re-matches #".*:(\d+)] wakes up" "[1518-11-05 00:55] wakes up")

(watch-guards input-small)

(def m {1 {:value 0, :active false}, 2 {:value 0, :active false}})

(update-in m [1] assoc :value 1 :active true)

(update-in {:a {:b {:c 2}}} [:a :b :c] + 20)
