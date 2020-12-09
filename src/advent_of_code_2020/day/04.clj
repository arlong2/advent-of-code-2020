(ns advent-of-code-2020.day.04
  (:require [advent-of-code-2020.utilities :as u]
            [clojure.string :as s]))

;; The automatic passport scanners are slow because they're having trouble
;; detecting which passports have all required fields. The expected fields are
;; as follows:
;;
;;  - byr (Birth Year)
;;  - iyr (Issue Year)
;;  - eyr (Expiration Year)
;;  - hgt (Height)
;;  - hcl (Hair Color)
;;  - ecl (Eye Color)
;;  - pid (Passport ID)
;;  - cid (Country ID)

(def example
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

;; Count the number of valid passports - those that have all required fields.
;; Treat cid as optional. In your batch file, how many passports are valid?

(def required-keys ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(def examle-solution 2)

(def all-passports (u/read-edn-file "day04_input.edn"))

;;;;;;;;;; Part 1 ;;;;;;;;;;

(defn pass->map
  [passports]
  (let [kv->map (fn [m kv]
                  (let [[k v] (s/split kv #":")]
                    (assoc m k v)))]
    (->> (s/split passports #"\n\n") (filter seq)

         (map #(->> (s/split % #"\n")
                    (map (fn [s] (s/split s #" ")))
                    (apply concat)))
         (map #(reduce kv->map {} %)))))

(defn solution-part-1
  [passports keys]
  (->> (pass->map passports)
       (map (fn [pass] (map #(contains? pass %) keys)))
       (map #(every? true? %))
       (filter true?)
       count))

;;;;;;;;;; Part 2 ;;;;;;;;;;

;; The line is moving more quickly now, but you overhear airport security
;; talking about how passports with invalid data are getting through. Better
;; add some data validation, quick!
;;
;; You can continue to ignore the cid field, but each other field has strict
;; rules about what values are valid for automatic validation:
;;
;;  - byr (Birth Year) - four digits; at least 1920 and at most 2002.
;;  - iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;;  - eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;;  - hgt (Height) - a number followed by either cm or in:
;;      - If cm, the number must be at least 150 and at most 193.
;;      - If in, the number must be at least 59 and at most 76.
;;  - hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;;  - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;;  - pid (Passport ID) - a nine-digit number, including leading zeroes.
;;  - cid (Country ID) - ignored, missing or not.
;;
;; Your job is to count the passports where all required fields are both present
;; and valid according to the above rules. Here are some example values:

(defn validate-number
  [length mi mx value]
  (and (= (count value) length)
       (re-matches #"^[0-9]*$" value)
       (>= (Integer/parseInt value) mi)
       (<= (Integer/parseInt value) mx)))

(defn validate-height
  [value]
  (when (>= (count value) 4)
    (let [v (->> (drop-last 2 value) (apply str))]
      (or (and (= (take-last 2 value) [\i \n])
               (re-matches #"^[0-9]{2}$" v)
               (>= (Integer/parseInt v) 59)
               (<= (Integer/parseInt v) 76))
          (and (= (take-last 2 value) [\c \m])
               (re-matches #"^[0-9]{3}$" v)
               (>= (Integer/parseInt v) 150)
               (<= (Integer/parseInt v) 193))))))

(def validations
  {"byr" (partial validate-number 4 1920 2002)
   "iyr" (partial validate-number 4 2010 2020)
   "eyr" (partial validate-number 4 2020 2030)
   "hgt" validate-height
   "hcl" (fn [v] (some? (re-matches #"^#[0-9|a-f]{6}$" v)))
   "ecl" (fn [v] (some? (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v)))
   "pid" (fn [v] (= (count v) 9))})

(defn validate-pass
  [keys pass]
  (map #(and (contains? pass %)
             ((get validations %) (get pass %))) keys))

(defn solution-part-2
  [passports keys]
  (->> (pass->map passports)
       (map (partial validate-pass keys))
       (map #(every? true? %))
       (filter true?)
       count))
