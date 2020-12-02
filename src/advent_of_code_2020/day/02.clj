(ns advent-of-code-2020.day.02
  (:require [advent-of-code-2020.utilities :as u]
            [clojure.string :as s]))

;; Your flight departs in a few days from the coastal airport; the easiest way down
;; to the coast from here is via toboggan.

;; The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
;; "Something's wrong with our computers; we can't log in!" You ask if you can take
;; a look.

;; Their password database seems to be a little corrupted: some of the passwords
;; wouldn't have been allowed by the Official Toboggan Corporate Policy that was in
;; effect when they were chosen.

;; To try to debug the problem, they have created a list (your puzzle input) of
;; passwords (according to the corrupted database) and the corporate policy when
;; that password was set.

;; For example, suppose you have the following list:

(def example
  ["1-3 a: abcde"
   "1-3 b: cdefg"
   "2-9 c: ccccccccc"])

;; Each line gives the password policy and then the password. The password policy
;; indicates the lowest and highest number of times a given letter must appear for
;; the password to be valid. For example, `1-3 a` means that the password must
;; contain `a` at least `1` time and at most `3` times.

;; In the above example, `2` passwords are valid. The middle password, `cdefg`, is
;; not; it contains no instances of `b`, but needs at least `1`. The first and
;; third passwords are valid: they contain one `a` or nine `c`, both within the
;; limits of their respective policies.

(def example-solution 2)

;; How many passwords are valid according to their policies?

(def passwords (u/read-edn-file "day02_input.edn"))

;;;;;;;;;; Part 1 ;;;;;;;;;;

(defn is-valid? [line]
  (let [[policy password] (s/split line #": ")
        [range c] (s/split policy #" ")
        [mi mx] (->> (s/split range #"-") (map #(Integer/parseInt %)))
        freq (get (frequencies password) (-> c seq first))]
    (and freq (>= freq mi) (<= freq mx))))

(defn solution-part-1
  [input]
  (-> (filter is-valid? input)
      count))


;;;;;;;;;; Part 2 ;;;;;;;;;;

;; Each policy actually describes two positions in the password, where 1 means
;; the first character, 2 means the second character, and so on. (Be careful;
;; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one
;; of these positions must contain the given letter. Other occurrences of the
;; letter are irrelevant for the purposes of policy enforcement.

;; Given the same example list from above:

;; * 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
;; * 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
;; * 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.

(def example-solution-2 1)

;; How many passwords are valid according to the new interpretation of the policies?

(defn is-valid-2? [line]
  (let [[policy password] (s/split line #": ")
        [positions c] (s/split policy #" ")
        [a b] (->> (s/split positions #"-") (map #(-> (Integer/parseInt %) dec)))
        in-a (= (get password a) (-> c seq first))
        in-b (= (get password b) (-> c seq first))]

    (or (and in-a (not in-b))
        (and (not in-a) in-b))))

(defn solution-part-2
  [input]
  (-> (filter is-valid-2? input)
      count))

(defn print-answers []
  (println "---- Part 1 ----")
  (->> (solution-part-1 passwords)
       (println "Answer:"))

  (println "---- Part 2 ----")
  (->> (solution-part-2 passwords)
       (println "Answer:")))
