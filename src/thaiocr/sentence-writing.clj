(ns thaiocr.sentence-writing
;  (:require [thaiocr.character-reading :as character-reading :refer character-level])
  )

(comment
  "if there are maps within range"
  "-take all existing maps within range"
  "-else stop process"
  "find a new range and add 5"
  )

(defn find-all-connected-maps
"find maps within the same range"
[starting-maps]
;(println "starting-maps " starting-maps)
(let [starting-maps-no-nil (filter #(not (nil? %)) starting-maps)
      lowest-row-map (first (sort-by :lowest-row starting-maps-no-nil))
        ;starting-lowest-row (:lowest-row lowest-row-map)
        starting-highest-row (:highest-row lowest-row-map)
        ]
    (loop [keep-going true
           remaining-maps starting-maps-no-nil
           highest-row-maximum (+ 5 starting-highest-row)
           vector-of-maps []
           ]
      (if (false? keep-going)
        [(flatten vector-of-maps) remaining-maps]
        (let [all-maps-within-range (filter #(<= (:lowest-row %) highest-row-maximum) remaining-maps)
              all-maps-not-within-range (filter #(> (:lowest-row %) highest-row-maximum) remaining-maps)
              ]
          ;(println "all-maps-within-range " all-maps-within-range)
          (if (and (not (empty? all-maps-within-range)) (< highest-row-maximum 1000))
            (let [new-highest-row-maximum (:highest-row (last (sort-by :highest-row all-maps-within-range)))]
              (recur true
                     all-maps-not-within-range
                     (+ 5 new-highest-row-maximum)
                     (conj vector-of-maps all-maps-within-range)
                     ))
            (recur false
                   all-maps-not-within-range
                   0
                   vector-of-maps
                   )
            )
          )
        )
      )
    )
  )

(defn split-by-rows
  [group-of-letter-maps]
  (loop [remaining-maps group-of-letter-maps
         rows []]
                                        ;    (println "remaininig-maps " remaining-maps)
   ; (println remaining-maps)
    (if (empty? remaining-maps)
      rows
      (let [[new-rows new-remaining-maps] (doall (find-all-connected-maps remaining-maps))
            new-rows-conjed (conj rows new-rows)
            ;new-remaining-maps-no-nil (filter #(not (nil? %)) new-remaining-maps)
            ]
        ;(println "new-remaining-maps-no-nil " new-remaining-maps-no-nil)
        (recur new-remaining-maps
               new-rows-conjed
               )
        )
      )
    )
  )



(defn find-patterns-in-range
  "finds the intersections of column locations to determine if each character are in the same column"
  [all-pattern-maps pattern-range]

  (let [pattern-range-as-set (set pattern-range)
        patterns-found (for [one-pattern all-pattern-maps]
                         (let [one-pattern-range (range (:lowest-column one-pattern) (:highest-column one-pattern))
                               count-one-pattern-range (count one-pattern-range)
                               one-pattern-range-as-set (set one-pattern-range)
                               intersections (clojure.set/intersection one-pattern-range-as-set pattern-range-as-set)
                               intersections-count (count intersections)
                               ]
                           (if (>= (/ intersections-count count-one-pattern-range) 0.5)
                             one-pattern
                             nil
                             ))
                         )
        patterns-found-no-nil (filter #(not (nil? %)) patterns-found)
        maybe-vec (if (not (empty? patterns-found-no-nil))
                    (vec patterns-found-no-nil)
                    nil
                    )
        ]
   ; (println maybe-vec)
   ; (println pattern-range)
    maybe-vec
                                        ;(spit "/home/jared/clojureprojects/thaiocr/vecpatterns.txt" vec-patterns-found :append true)

    )
  )

(defn write-it
  "take a vector of vectors, which contain maps"
  [vec-of-vectors]
  (apply str (for [one-vector vec-of-vectors]
               (let [the-vector-sorted (sort-by :level one-vector)]
                 (apply str (map #(:character %) the-vector-sorted))
                 )
               )
         )
  )


(defn handle-special-cases
  "search map for special cases and correct them basically remove the character following"
  [maps-sorted-by-position]
 ; (println maps-sorted-by-position)
  (loop [maps-sorted-by-position maps-sorted-by-position
         new-maps []
         ;number-to-drop 0
         ]
    (if (empty? maps-sorted-by-position)
      (do
       ; (println new-maps)
        new-maps)
      (let [first-character-map (first maps-sorted-by-position)
            first-character (:character first-character-map)
            ]
        (cond
         (= first-character "ำ" )
         (let [a-to-remove (filter #(= "า" (:character %)) maps-sorted-by-position)
               as-sorted (sort-by :lowest-column a-to-remove)
               a-removed (remove #(= (first as-sorted) %) maps-sorted-by-position)
               removed-sorted (sort-by :lowest-column a-removed)
                ]
           ; (println "two-removed " removed-sorted)
            (recur (drop 1 removed-sorted) ; drop 2 because the next one should be removed from the collection
                   (conj new-maps first-character-map)
                   ))
         (or
             (= first-character "ะ") ;delete the next value in the map
             (= first-character "ญ") ;delete the next value in the map
             (= first-character "ฐ") ;delete the next value in the map
             )
         (do
           (recur (drop 2 maps-sorted-by-position)
                  (conj new-maps first-character-map)
                  ))
          (and (= first-character "เ") (= "เ" (:character (second maps-sorted-by-position))))
            ;untested
          (recur (drop 2 maps-sorted-by-position)
                 (conj new-maps (assoc first-character-map :character "แ"))
                 )
          :else (recur (drop 1 maps-sorted-by-position)
                 (conj new-maps first-character-map)
                 )
                                        ;(= new-character-map-found-or-edit "i") ;ignore
                                        ;(println "i")
                                        ;(= new-character-map-found-or-edit "j") ;ignore
                                        ;(println "j")
          )
        )
      )
    )

  )

(defn make-sentence-vector
  "width-of-ones-zeros = the width of the whole one-zeros-file
put each character in the proper order and print out a string
"
  [group-of-letter-maps]
 ; (println "group-of-letter-maps" group-of-letter-maps)
  (for [one-sentence-line (doall (split-by-rows group-of-letter-maps))]
    (let [use-one-sentence-line one-sentence-line  ;the vector of characters that we foun
          sorted-by-position (sort-by :lowest-column use-one-sentence-line) ; sort the vectory of characters found by position on the x axis
          all-maps-sorted-by-position-with-special-cases-handled (doall (handle-special-cases sorted-by-position))
          all-maps-sorted-by-position-no-level-2-3-4 (filter #(not (or (= 2 (:level %)) (= 3 (:level %)) (= 4 (:level %)) (= 6 (:level %)))) all-maps-sorted-by-position-with-special-cases-handled)
          ]
     ; (println all-maps-sorted-by-position-with-special-cases-handled)
      (loop [sorted-by-position all-maps-sorted-by-position-no-level-2-3-4
             to-write []
             ]
        (if (empty? sorted-by-position)
          (write-it to-write)
          (do
           ; (println (first sorted-by-position))
            (let [first-pattern (first sorted-by-position) ;the first letter map
                  ;first-pattern-x (:highest-column first-pattern)
                  ;first-pattern-x-minus-7 (- first-pattern-x 7)
                  ;first-pattern-x-plus-7 (+ first-pattern-x 3)
                  first-pattern-range (range (:lowest-column first-pattern) (+ 1 (:highest-column first-pattern)))
                  all-patterns-in-this-range (if (= 5 (:level first-pattern))
                                                 [first-pattern]
                                                 (doall (find-patterns-in-range all-maps-sorted-by-position-with-special-cases-handled first-pattern-range))
                                                 )
                  ;how-many-patterns (count all-patterns-in-this-range)
                  ]
      ;        (println all-patterns-in-this-range)
              (recur (drop 1 sorted-by-position)
                     (conj to-write all-patterns-in-this-range)
                     )
              )
            )
          )
        )
      )
    )
  )
