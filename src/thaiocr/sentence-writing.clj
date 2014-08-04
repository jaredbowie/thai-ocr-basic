(ns thaiocr.sentence-writing
;  (:require [thaiocr.character-reading :as character-reading :refer character-level])
  )

(comment
  "if there are maps within range"
  "-take all existing maps within range"
  "-else stop process"
  "find a new range and add 5"
  )

(defn find-all-connected-maps [starting-maps]
  (let [lowest-row-map (first (sort-by :lowest-row starting-maps))
        ;starting-lowest-row (:lowest-row lowest-row-map)
        starting-highest-row (:highest-row lowest-row-map)
        ]
    (loop [keep-going true
           remaining-maps starting-maps
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
    (if (empty? remaining-maps)
      rows
      (let [[new-rows new-remaining-maps] (find-all-connected-maps remaining-maps)
            new-rows-conjed (conj rows new-rows)
            ]
        (recur new-remaining-maps
               new-rows-conjed
               )
        )
      )
    )
  )



(defn find-patterns-in-range
  "takes
all-pattern-maps=sorted-by-position which is all the maps of the reamining patterns
pattern-range=the range of x axis places the pattern may be"
  [all-pattern-maps pattern-range]
  (let [patterns-found (for [one-pattern all-pattern-maps]
                         (if (some #(= % (:lowest-column one-pattern)) pattern-range)
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

(defn make-sentence-vector
  "width-of-ones-zeros = the width of the whole one-zeros-file
put each character in the proper order and print out a string
"
  [group-of-letter-maps]
  (for [one-sentence-line (split-by-rows group-of-letter-maps)]
    (let [use-one-sentence-line one-sentence-line  ;the vector of characters that we foun
          sorted-by-position (sort-by :lowest-column use-one-sentence-line) ; sort the vectory of characters found by position on the x axis
          ]
      (loop [sorted-by-position sorted-by-position
             to-write []
             ]
        (if (empty? sorted-by-position)
          (write-it to-write)
          (do
            (let [first-pattern (first sorted-by-position) ;the first letter map
                  first-pattern-x (:lowest-column first-pattern)
                  first-pattern-x-minus-7 (- first-pattern-x 7)
                  first-pattern-x-plus-7 (+ first-pattern-x 7)
                  first-pattern-range (range first-pattern-x-minus-7 first-pattern-x-plus-7)
                  all-patterns-in-this-range (if (= 5 (:level first-pattern))
                                               (find-patterns-in-range sorted-by-position first-pattern-range)
                                               (vector first-pattern))
                  how-many-patterns (count all-patterns-in-this-range)
                  ]
              (recur (drop how-many-patterns sorted-by-position)
                     (conj to-write all-patterns-in-this-range)
                     )
              )
            )
          )
        )
      ))
  )
