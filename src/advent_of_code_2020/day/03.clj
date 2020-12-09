(ns advent-of-code-2020.day.03
  (:require [advent-of-code-2020.utilities :as u]
            [clojure.string :as s]))

;; Due to the local geology, trees in this area only grow on exact integer
;; coordinates in a grid. You make a map (your puzzle input) of the open
;; squares (.) and trees (#) you can see. For example:

(def example
  ["..##.......",
   "#...#...#..",
   ".#....#..#.",
   "..#.#...#.#",
   ".#...##..#.",
   "..#.##.....",
   ".#.#.#....#",
   ".#........#",
   "#.##...#...",
   "#...##....#",
   ".#..#...#.#"])

;; From your starting position at the top-left, check the position that is
;; right 3 and down 1. Then, check the position that is right 3 and down 1
;; from there, and so on until you go past the bottom of the map.

;; Starting at the top-left corner of your map and following a slope of
;; right 3 and down 1, how many trees would you encounter?

(def example-solution 7)

(def main-slope (u/read-edn-file "day03_input.edn"))

;;;;;;;;;; Part 1 ;;;;;;;;;;

(defn solution-part-1
  [slope right down]
  (let [width (count (first slope))
        height (count slope)]
    (loop [x 0 y 0 trees 0]
      (if (>= y height)
        trees
        (let [p (-> slope (nth y) (nth x))]
          (recur (mod (+ x right) width)
                 (+ y down)
                 (if (= p \#) (inc trees) trees)))))))

;;;;;;;;;; Part 2 ;;;;;;;;;;

;; Determine the number of trees you would encounter if, for each of the
;; following slopes, you start at the top-left corner and traverse the map
;; all the way to the bottom:
;;
;;     Right 1, down 1.
;;     Right 3, down 1. (This is the slope you already checked.)
;;     Right 5, down 1.
;;     Right 7, down 1.
;;     Right 1, down 2.

;; What do you get if you multiply together the number of trees encountered
;; on each of the listed slopes?

(defn solution-part-2
  [slope]
  (* (solution-part-1 slope 1 1)
     (solution-part-1 slope 3 1)
     (solution-part-1 slope 5 1)
     (solution-part-1 slope 7 1)
     (solution-part-1 slope 1 2)))
