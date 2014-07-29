(ns thaiocr.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [thaiocr.patterns :as charpatterns]
            [thaiocr.patternsfontlarge :as charpatternslarge]
            [me.raynes.fs :as fs]
            )

;(:use [clojure.string :only (split trim)])
;(:use [clojure.java.io :as [io]])
;(:use [clojure.contrib.shell-out :only (sh)])
;(:use [clojure.contrib.math :only (sqrt)])
)


                                        ;speed ideas
                                        ;split by letter and scan that
                                        ;substring searches


;basically it's trying to find pattern d for some reason when there is no D


;(def temp-outfile "/home/jared/clojureprojects/thaiocr/clj-converted.txt")
;(def ones-zeros  "/home/jared/clojureprojects/thaiocr/ones-zeros.txt")



(defn find-patterns-in-range
  "takes
all-pattern-maps=sorted-by-position which is all the maps of the reamining patterns
pattern-range=the range of x axis places the pattern may be"
  [all-pattern-maps pattern-range]
  ;(spit "/home/jared/clojureprojects/thaiocr/vecpatterns.txt" "started" :append true)
  ;(spit "/home/jared/clojureprojects/thaiocr/vecpatterns.txt" all-pattern-maps :append true)
  ;(spit "/home/jared/clojureprojects/thaiocr/vecpatterns.txt" pattern-range :append true)
  ;(println all-pattern-maps)
  (let [patterns-found (for [one-pattern all-pattern-maps]
                         (if (some #(= % (:x one-pattern)) pattern-range)
                           one-pattern
                           nil
                           )
                         )
        patterns-found-no-nil (filter #(not (nil? %)) patterns-found)
        maybe-vec (if (not (empty? patterns-found-no-nil))
                    (vec patterns-found-no-nil)
                    nil
                    )
        ]
    maybe-vec
                                        ;(spit "/home/jared/clojureprojects/thaiocr/vecpatterns.txt" vec-patterns-found :append true)

    )
  )

(defn make-sentence-vector
  "width-of-ones-zeros = the width of the whole one-zeros-file
put each character in the proper order and print out a string
"
  [width-of-ones-zeros]
  (let [all-letters (read-string (slurp "/home/jared/clojureprojects/thaiocr/patternsfound.txt")) ;the vector of characters that we found
        sorted-by-position (sort-by :x all-letters) ; sort the vectory of characters found by position on the x axis
        ]
    (loop [sorted-by-position sorted-by-position
           to-write []
           ]

      (if (empty? sorted-by-position)
        to-write
        (do
          (let [first-pattern (first sorted-by-position) ;the first letter map
                first-pattern-x (:x first-pattern)
                first-pattern-x-minus-7 (if (< 0 (- first-pattern-x 7))
                                          (- first-pattern-x 7)
                                          0
                                          )
                first-pattern-x-plus-7 (if (< width-of-ones-zeros (+ first-pattern-x 7))
                                         width-of-ones-zeros
                                         (+ first-pattern-x 7)
                                         )
                first-pattern-range (range first-pattern-x-minus-7 first-pattern-x-plus-7)
                all-patterns-in-this-range (if (not (some #(= % (:pattern-name first-pattern)) charpatternslarge/patterns-level-five))
                                             (find-patterns-in-range sorted-by-position first-pattern-range)
                                             (vector first-pattern))
                how-many-patterns (count all-patterns-in-this-range)
                ]
                                        ;(println (str "first-pattern " first-pattern))
                                        ;(println (str "all-patterns-in-this-range " all-patterns-in-this-range))
            (recur (drop how-many-patterns sorted-by-position)
                   (conj to-write all-patterns-in-this-range)
                   )
            )
          )
        )
      )
    )
  )

(defn grouping-one? [pattern-name]
  (some #(= % pattern-name) charpatternslarge/patterns-level-one)
  )

(defn grouping-two? [pattern-name]
  (some #(= % pattern-name) charpatternslarge/patterns-level-two)
  )

(defn grouping-three? [pattern-name]
  (some #(= % pattern-name) charpatternslarge/patterns-level-three)
  )

(defn grouping-four? [pattern-name]
  (some #(= % pattern-name) charpatternslarge/patterns-level-four)
  )

(defn grouping-five? [pattern-name]
  (some #(= % pattern-name) charpatternslarge/patterns-level-five)
  )



(defn write-sentences [all-sentences]
 ; (println all-sentences)
  (for [one-grouping all-sentences]
    (let [one (:actual-letter (first (filter #(not (nil? (grouping-one? (:pattern-name %)))) one-grouping)))
          two (:actual-letter (first (filter #(not (nil? (grouping-two? (:pattern-name %)))) one-grouping)))
          three (:actual-letter (first (filter #(not (nil? (grouping-three? (:pattern-name %)))) one-grouping)))
          four (:actual-letter (first (filter #(not (nil? (grouping-four? (:pattern-name %)))) one-grouping)))
          five (:actual-letter (first (filter #(not (nil? (grouping-five? (:pattern-name %)))) one-grouping)))
          ]

      (str three two four one five))
    )
  )


(defn test-the-rest-of-the-pattern [two-dimensional-array pattern-to-match two-dimensional-array-row-position row-position pattern-row-count pattern-row the-number-of-rows-in-the-whole-image]
  (loop [two-dimensional-array two-dimensional-array ; the entire pattern as an array
         pattern-to-match pattern-to-match ; the letter or symbol
         two-dimensional-array-row-position (+ 1 two-dimensional-array-row-position)
         ;was +1
        ;where we are as far as row position for the whole array
         row-position row-position ; the position in the row for the whole two dimensional array
         pattern-row-count pattern-row-count ; how many items are in one row of the pattern
         row-number-of-pattern 1 ; the row number we're on in the pattern
         ]
   ; (println (str row-number-of-pattern "-" (count pattern-to-match)))
   ;

    (if (not (< row-number-of-pattern (count (:pattern pattern-to-match))))
       (if (= (count (:pattern pattern-to-match)) row-number-of-pattern)
         (spit "/home/jared/clojureprojects/thaiocr/patternsfound.txt" (str {:pattern-name (:pattern-name pattern-to-match) :actual-letter (:actual-letter pattern-to-match) :x row-position :y two-dimensional-array-row-position} " ") :append true)
         )
       (if (< two-dimensional-array-row-position the-number-of-rows-in-the-whole-image)
         (do
;           (println two-dimensional-array-row-position)
           (let [row-to-check (nth two-dimensional-array two-dimensional-array-row-position)
                 row-section-to-check (vec (take pattern-row-count (drop row-position row-to-check)))
                 pattern-section-to-check (nth (:pattern pattern-to-match) row-number-of-pattern)
                 matching (= row-section-to-check pattern-section-to-check)
                 ]
             ;(println "row-to-check" row-to-check)
                                        ;(println (str row-section-to-check "-" pattern-section-to-check))
                                        ;(println (str ))
                                        ;    (println row-number-of-pattern)
                                        ; (println (str two-dimensional-array-row-position "-" row-position))
             (if matching
               (recur two-dimensional-array
                      pattern-to-match
                      (+ 1 two-dimensional-array-row-position)
                      row-position
                      pattern-row-count
                      (+ 1 row-number-of-pattern)
                      )
               )
             )
           ))

      )
    )
  )

(defn pattern-handler
  "two-dimensional-array = the entire sentence as a two dimensional array
pattern-to-match = one of ALL the patterns that we have defined in patterns.clj"
  [two-dimensional-array pattern-to-match]
  (let [the-number-of-rows-in-the-whole-image (count two-dimensional-array)]
    ;(println the-number-of-rows-in-the-whole-image)
                                        ;  (println "pattern-handler")
    (loop [the-whole-two-dimensional-array two-dimensional-array
           two-dimensional-array-row-position 0]
                                        ;    (println two-dimensional-array-row-position)
      (when (not (empty? the-whole-two-dimensional-array))
        (let [first-part-of-two-dimensional-array (first the-whole-two-dimensional-array)]
          (loop [one-row first-part-of-two-dimensional-array
                 first-row-of-pattern (first (:pattern pattern-to-match))
                 pattern-row-count (count first-row-of-pattern)
                 row-position 0
                 matches []
                 ]
            ;(println one-row)
            (when (>= (count one-row) pattern-row-count)
              (let [piece-of-the-row (take pattern-row-count one-row)
                    possible-match (if (= piece-of-the-row first-row-of-pattern)
                                     (test-the-rest-of-the-pattern two-dimensional-array pattern-to-match two-dimensional-array-row-position row-position pattern-row-count 1 the-number-of-rows-in-the-whole-image)
                                     nil
                                     )
                    matches (if (not (nil? possible-match))
                              (conj matches possible-match)
                              matches
                              )
                    ]

                (recur (drop 1 one-row)
                       first-row-of-pattern
                       pattern-row-count
                       (+ 1 row-position)
                       matches
                       )
                )
              )
            )
          )
        (recur (drop 1 the-whole-two-dimensional-array)
               (+ 1 two-dimensional-array-row-position)
               )
        )
                                        ;(for [one-element one-row])
      ))
  )

(defn pattern-reader-main
  "ones-zeros-pattern is the entire two dimensional sentence as a string"
  [ones-zeros-pattern]
  (spit "/home/jared/clojureprojects/thaiocr/patternsfound.txt" "")
  (spit "/home/jared/clojureprojects/thaiocr/patternsfound.txt" "[" :append true)
  ; (pattern-handler (make-the-array ones-zeros) charpatterns/y-pattern)
  (doall (for [one-pattern charpatternslarge/all-patterns] (pattern-handler (make-the-array ones-zeros-pattern) one-pattern)))
   (spit "/home/jared/clojureprojects/thaiocr/patternsfound.txt" "]" :append true)
   (let [width-of-ones-zeros (count (first (clojure.string/split ones-zeros-pattern #"\n")))
         all-sentences (make-sentence-vector width-of-ones-zeros)
         sentence-list (write-sentences all-sentences)
         ]
     (apply str sentence-list)
     ;(filter #(not (empty %)) sentence-list)
     )
  )

; Input handling




(defn main []
  (let [images-directory "/home/jared/clojureprojects/thaiocr/testbmp/"
        temp-directory "/home/jared/clojureprojects/thaiocr/testbmptempdir/"
        all-images-in-directory (fs/list-dir images-directory)
        ]
    (for [one-image all-images-in-directory]
      (do
        (reset-get-each-sentence)
        (convert-image-black-white-then-to-text (str images-directory one-image) (str temp-directory one-image) (str temp-directory one-image ".txt"))
        (load-text-image (str temp-directory one-image ".txt") (str temp-directory one-image "oneszeros.txt"))
        (split-text-image-into-lines (slurp (str temp-directory one-image "oneszeros.txt")))
        (let [all-sentences (get-each-sentence)]
          (for [one-line all-sentences]
            (pattern-reader-main one-line)
            )
          )
        )
      )
    )
)

(defn test-counts []
  (let [images-directory "/home/jared/clojureprojects/thaiocr/minimumtest/"
        all-images-in-directory (filter #(= (apply str (drop 10 %)) "bmponeszeros.txt") (fs/list-dir images-directory))
        ]
    (for [one-image all-images-in-directory]
      (split-text-image-into-lines (slurp (str images-directory one-image)))
      )
    )
  )

(defn spit-test [one-sentence]
  (spit "/home/jared/clojureprojects/thaiocr/testtest.txt" one-sentence)
  )

(defn find-text-portion
"given a string that starts with text"
  [string-split-by-line line1]
  (loop [string-split-by-line string-split-by-line
         one-text-portion []
         ]
    (let [first-line (first string-split-by-line)]
                                        ;in case we just have some remaining bottom check to make sur it's long enough so the nth's don't indexoutofbound
      ;(println (str "find-text-portion-" (count string-split-by-line)))
      (if (not (< 3 (count string-split-by-line)))
        (do
          (update-get-each-sentence (apply str (map #(str % "\n") one-text-portion)))
          ;(spit "/home/jared/clojureprojects/thaiocr/testtest.txt" (apply str (map #(str % "\n") one-text-portion)) :append true)
          (drop 3 string-split-by-line)
          ) ; we return the part that we didn't take

          (if (or
               (and
                (= line1 first-line)
                (= line1 (nth string-split-by-line 1))
                (= line1 (nth string-split-by-line 2))
                ;(= line1 (nth string-split-by-line 3))
                )
               (nil? first-line))
                                        ;if the next 4 lines are all blank
                                        ;then we have our sentence found already and we can put it into the atom
            (do
;              (println "one line")
              (update-get-each-sentence (apply str (map #(str % "\n") one-text-portion)))
               ;(spit "/home/jared/clojureprojects/thaiocr/testtest.txt" (apply str (map #(str % "\n") one-text-portion)) :append true)
                (drop 3 string-split-by-line)
                ) ; we return the part that we didn't take
            (do
              (recur (drop 1 string-split-by-line)
                     (conj one-text-portion (apply str (take 1 string-split-by-line)))
                     )
              )
            )
          )
        )
      )
  )

(defn split-text-image-into-lines
  "take a string of the entire 1s0s image.txt and split it into lines"
  [file-string]
  (let [string-split-by-line (clojure.string/split-lines file-string)
        width (count (first string-split-by-line))
        width-minus-one (- width 1)
        line0 (str "1" (apply str (take width-minus-one (repeat "0"))))
        line1 (apply str (take width (repeat "0")))
        ]
    (loop [string-split-by-line string-split-by-line]
      ;drop empty lines until we find some text on the line
      (when (and (not (empty? string-split-by-line)) (not (nil? string-split-by-line)))
        (let [;first-line (first string-split-by-line)
              new-string-split-by-line (find-text-portion string-split-by-line line1)
              ]
;          (println (count new-string-split-by-line))
          ;(spit "/home/jared/clojureprojects/thaiocr/testtest.txt" (str (vec new-string-split-by-line) "\n") :append true)
          (recur new-string-split-by-line)
          )
        )
      )
    )
  )
