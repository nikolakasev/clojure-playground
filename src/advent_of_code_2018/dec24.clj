(ns advent-of-code-2018.dec24
  (:require [clojure.string :as str]))

(def regex #"(\d*) units each with (\d*) hit points( \([^)]*\))? with an attack that does (\d*) (\w*) damage at initiative (\d*)")

(re-matches regex "3476 units each with 37466 hit points (immune to cold; weak to radiation) with an attack that does 20 slashing damage at initiative 12")

;(immune|weak) to (\w*)(\, (\w*))?(); )?
