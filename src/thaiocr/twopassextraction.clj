(ns thaiocr.twopassextraction
  (:require [clojure.java.shell :refer [sh]]
            [me.raynes.fs :as fs]
            )

  )


(def each-sentence (atom {:each-line []}))

(defn get-each-sentence [] (@each-sentence :each-line))

(defn update-get-each-sentence [one-sentence]
   (swap! each-sentence update-in [:each-line] merge one-sentence)
   )

(defn reset-get-each-sentence []
  (reset! each-sentence {:each-line []})
  )



(defn get-string-as-numbers [the-string]
    (vec (map #(Character/getNumericValue %) the-string))
  )


(defn make-the-2d-map
  [zero-one-string]
  (let [file-split-by-line (clojure.string/split zero-one-string #"\n")
        vector-row-number (range (count file-split-by-line))
        vector-column-number (range (count (first file-split-by-line)))
        all-the-maps (for [one-row-index-number vector-row-number]
                       (sorted-map (keyword (str one-row-index-number))
                                   (apply conj (for [one-column-index-number vector-column-number]
                                                 (sorted-map (keyword (str one-column-index-number)) (Character/getNumericValue (nth (nth file-split-by-line one-row-index-number) one-column-index-number)))
                                                 )
                                          )
                                   )
                  )
        ]
                                        ;    (println all-the-maps)
    (spit "/home/jared/clojureprojects/thaiocr/temptemptemptemptemp.txt" (str (vec all-the-maps)))
    (apply conj all-the-maps)
    )
  )


(def graph-map (ref nil))
(def labels (ref nil))
(def relationships (ref nil))
(def characters-letters-as-strings (ref nil))

(defn neighbor-check [row-keyword column-keyword west-row-keyword west-column-keyword north-west-row-keyword north-west-column-keyword north-row-keyword north-column-keyword north-east-row-keyword north-east-column-keyword]
  (let [west-val (west-column-keyword (west-row-keyword @graph-map))
        north-west-val (north-west-column-keyword (north-west-row-keyword @graph-map))
        north-val (north-column-keyword (north-row-keyword @graph-map))
        north-east-val (north-east-column-keyword (north-east-row-keyword @graph-map))
        as-list (list west-val north-west-val north-val north-east-val)
        as-list-without-nil (filter #(not (or (nil? %) (= 0 %))) as-list)
        ]
    (if (not (empty? as-list-without-nil))
      (let [the-min-val (apply min as-list-without-nil) ;we found the minimum value from the surroundings and will assign that to our current pixel
            all-values-as-set (set as-list-without-nil) ;a set of all the values that surround our pixel for the union set
            ]
        (loop [all-the-values all-values-as-set]
          ;add every value to each value so that we know about all their relationships
          (when (not (empty? all-the-values))
            (let [first-value (first all-the-values)]
              (dosync (alter relationships update-in [first-value] clojure.set/union all-values-as-set)))
              ;add all the surrounding values to the current value of the pixel so we know that they're all touching
            (recur (drop 1 all-the-values)))
          )
        (dosync (alter graph-map assoc-in [row-keyword column-keyword] the-min-val))
        ;set current pixel to the lowest value we know about that it's touching
        )
      false
      )
    )
  )

(defn pass1 [the-zero-one-map]
  "take an array and pass through it looking for connected patterns"
  (dosync (ref-set graph-map the-zero-one-map))
  (dosync (ref-set labels (range 100 999)))
  (dosync (ref-set relationships {}))
  (dosync (ref-set characters-letters-as-strings {}))

  (loop [number-of-rows (range (count the-zero-one-map))
         number-of-columns (range (count (val (first the-zero-one-map))))
         ]
    (when (not (empty? number-of-rows))

      (let [first-row-number (first number-of-rows)
            first-row-number-north (- first-row-number 1)
            row-keyword (keyword (str first-row-number))
            west-row-keyword row-keyword
            north-row-keyword (keyword (str first-row-number-north))
            north-west-row-keyword north-row-keyword
            north-east-row-keyword north-row-keyword]
        (loop [number-of-columns number-of-columns]
          (when (not (empty? number-of-columns))
            (let [one-column-x (first number-of-columns)
                  column-keyword (keyword (str one-column-x))
                  west-column-x (- one-column-x 1)
                  west-column-keyword (keyword (str west-column-x))
                  north-column-x one-column-x
                  north-column-keyword column-keyword
                  north-west-column-keyword west-column-keyword
                  north-east-column-x (+ one-column-x 1)
                  north-east-column-keyword (keyword (str north-east-column-x))
                  ]
      ;        (println one-column-x "/" (count number-of-columns))
              (if (not= 0 (column-keyword (row-keyword @graph-map)))
                (do ;(println "not 0")
                  (if (false? (neighbor-check row-keyword column-keyword west-row-keyword west-column-keyword north-west-row-keyword north-west-column-keyword north-row-keyword north-column-keyword north-east-row-keyword north-east-column-keyword))
                    ;if there aren't any neighbors
                      (do
                        (dosync (alter graph-map assoc-in [row-keyword column-keyword] (first (take 1 @labels))))
                        ;make a new label and set it to that
                        (dosync (alter relationships merge {(first (take 1 @labels)) #{(first (take 1 @labels))}}))
                        ;make a new relationship with this label being related to itself

                        (dosync (ref-set labels (drop 1 @labels)))
                        ;remove label from possible labels to use
                        )
                      )
                    )
                )
              )
            (recur (drop 1 number-of-columns))
            )
          )
        (recur (drop 1 number-of-rows)
               number-of-columns)
        )
      )
    )
;  (filter #(> (count (val %)) 1) @relationships)
  )


(defn find-all-relationship-similarities
"take one map and find any other maps that have it's value
 return the new relationships and any of the other maps that we found that need to be removed
"
  [initial-relationship all-other-relationships]
 ; (println "initial-relationship!!!!!!!!1 " initial-relationship)
  (loop [all-other-relationships (drop 1 all-other-relationships) ; so we don't remove our initial-relationship from all the relationships
         relationship-set (first (vals initial-relationship))
         maps-to-remove [] ;because we already grabbed their values
         ]
   ; (println "all-other-relationships " all-other-relationships)
   ; (println "relationship-set " relationship-set)
   ; (println "maps-to-remove " maps-to-remove)
    (if (empty? all-other-relationships)
      (do
       ; (println "relationship-set find-all-relationship-similarities" initial-relationship)
       ; (println "maps-with-used-removed find-all-relationship-similarities" maps-to-remove)
        [relationship-set maps-to-remove])
      (let [first-other (first all-other-relationships)]
       ; (println "initial-relationship " initial-relationship)
       ; (println "relationship-set " relationship-set)
       ; (println "maps-with-used-removed " maps-with-used-removed)
       ; (println (clojure.set/intersection (first (vals first-other)) relationship-set))
        (if (not (empty? (clojure.set/intersection (first (vals first-other)) relationship-set)))
                                        ;if we find another pair that has some of the same values as the pair we're inspecting then we want to add all of it's values, look for new values and explore those for now we'll not redo the whole operation and hope it works
          (do
           ; (println "intersection!")
            ;(println first-other)
            (recur (drop 1 all-other-relationships)
                   (clojure.set/union relationship-set (first (vals first-other)))
                   (conj maps-to-remove first-other)
                   ))
          (recur (drop 1 all-other-relationships)
                 relationship-set
                 maps-to-remove
                 )
          )
        )
      )
    )
  )

(defn form-new-relationships [sorted-current-relationships to-remove]
  (let [the-return-value (for [one-relationship sorted-current-relationships]
                           (if (not (some #(= one-relationship %) to-remove))
                             one-relationship
                             )
                           )]
   ; (spit "/home/jared/clojureprojects/thaiocr/debugging.txt" (vec the-return-value))
    (filter #(not (nil? %)) the-return-value)
    )
  )

(defn remake-relations [all-new-relationships]
  ;(println all-new-relationships)
  (apply merge (for [one-relationship all-new-relationships]
                 (apply merge (for [one-value (first (vals one-relationship))]
                                (hash-map one-value (apply min (first (vals one-relationship))))))))
  )

(defn slightly-alter-relationships
  "currently the relationships sometimes have leftover values or values that are touching not linked up we will fix that"
  []
  (let [current-relationships @relationships
        sorted-current-relationships (map #(hash-map (key %) (val %)) (sort @relationships)) ; sort the relationships by number
        ]
    (loop [sorted-current-relationships sorted-current-relationships
           all-new-relationships []
           ]
      (if (empty? sorted-current-relationships)
        all-new-relationships ; make relations again with all current values
        ;find any other maps that have this key in it's values
        ;take those values and add them to this key and remove those values from the map
        (do ;(spit "/home/jared/clojureprojects/thaiocr/debugging.txt" (first (keys (first sorted-current-relationships))))
            (let [first-relationship (first sorted-current-relationships)
                  the-current-key (first (keys first-relationship))
                  the-current-vals (first (vals first-relationship))
                  lowest-val (apply min the-current-vals)
                  [all-relationships to-remove] (find-all-relationship-similarities first-relationship sorted-current-relationships)
                  new-current-relationships (if (not (empty? to-remove))
                                              (form-new-relationships sorted-current-relationships to-remove)
                                              sorted-current-relationships)
                  ]
             ; (println "new-current-relationships slightly-alter " new-current-relationships)
             ; (println "sorted-current-relationships " sorted-current-relationships)
             ; (println "all-relationships-to-remove " to-remove)
             ; (println "all-relationships slightly-alter " all-relationships)
                                        ;if the current key is bigger than the lowest value then it should be associated with the lowest value instead
                                        ;search all ke
              (recur (drop 1 new-current-relationships)
                     (conj all-new-relationships {the-current-key all-relationships})
                     )
              ))
        )
      )
    )
)
(defn pass2
  "flooding regions"
  [the-zero-one-map]
;  (println "slightly-altered " )
  (println "@relationships " @relationships)
  (let [new-relationships (remake-relations (slightly-alter-relationships)) ;
        ]
    (println "new-relationships " new-relationships)
    (loop [number-of-rows (range (count the-zero-one-map))
           number-of-columns (range (count (val (first the-zero-one-map))))]
      ;loop through each row
      (when (not (empty? number-of-rows))
      ; (spit "/home/jared/clojureprojects/thaiocr/testlabels.txt" "\n" :append true)
        (let [first-row (first number-of-rows)
              first-row-keyword (keyword (str first-row))]
          (loop [columns-of-one-row number-of-columns]
            ;loop through each column of each row
            (when (not (empty? columns-of-one-row))
              (let [first-column (first columns-of-one-row)
                    first-column-keyword (keyword (str first-column))
                    current-value (first-column-keyword (first-row-keyword @graph-map)) ;the label of the current pixel we're on
                    lowest-relationship-value (get new-relationships current-value)] ;in pass1 we added all possible relationships this label had, now we can choose the lowest from that set

                (if (> current-value 0)
                  (do
               ;     (spit "/home/jared/clojureprojects/thaiocr/testlabels.txt" lowest-relationship-value :append true)
                    (dosync (alter graph-map assoc-in [first-row-keyword first-column-keyword] lowest-relationship-value))
                    ;set current pixel to lowest value we have for it's relationship
                    (dosync (alter characters-letters-as-strings update-in [lowest-relationship-value :column] clojure.set/union #{first-column}))
                    ;set one of the column positions for our current label
                    (dosync (alter characters-letters-as-strings update-in [lowest-relationship-value :row] clojure.set/union #{first-row}))
                    )
               ;   (spit "/home/jared/clojureprojects/thaiocr/testlabels.txt" "000" :append true)
                  )
                )
              (recur (drop 1 columns-of-one-row))
              )
            )
          )
        (recur (drop 1 number-of-rows)
               number-of-columns
               )
        )
      )
    )
  )

(defn character-extraction []
  (loop [all-character-numbers @characters-letters-as-strings
         all-characters []
         ]
    ;find lowest x highest x lowest y highest y
                                        ; for now make a string and print it
    (if (empty? all-character-numbers)
      all-characters
      (do
        (let [first-character-number (first all-character-numbers)
              lowest-row (apply min (:row (val first-character-number)))
              highest-row (apply max (:row (val first-character-number)))
              lowest-column (apply min (:column (val first-character-number)))
              highest-column (apply max (:column (val first-character-number)))
              column-range (range lowest-column highest-column)
              row-range (range lowest-row highest-row)
              one-newly-found-character
              (loop [row-range row-range
                     new-character ""]
                (if (empty? row-range)
                  new-character
                  (let [first-row (first row-range)
                        one-column
                        (loop [column-range column-range
                               current-character-column ""
                               ]
                          (if (empty? column-range)
                            current-character-column
                            (let [first-column (first column-range)
                                  row-keyword (keyword (str first-row))
                                  column-keyword (keyword (str first-column))]
                              (if (= (column-keyword (row-keyword @graph-map)) (key first-character-number))
                                (recur (drop 1 column-range)
                                       (str current-character-column "1")
                                       )
                                (recur (drop 1 column-range)
                                       (str current-character-column "0")
                                       )
                                )
                              )
                            )
                          )]
                    (recur (drop 1 row-range)
                           (str new-character "\n" one-column)
                           ))
                  )
                )]
          (recur (drop 1 all-character-numbers)
                 (conj all-characters one-newly-found-character))))
      )
    )
  )

(defn read-text-image-line [line]
  (if (= "white" (last (clojure.string/split line #"[,:\s]+")))
    "0"
    "1")
  )

(defn load-text-image
  "Loads a black and white image stored in imagemagick's text format
into a bitmap with '0' representing white and '1' black."
  [in out]
  (let [lines (clojure.string/split (slurp in) #"\n" )
        image-size (+ 1 (read-string (first (clojure.string/split (last lines) #","))))
        converted (map read-text-image-line lines)]
    (spit out (apply str (map #(str (apply str %) "\n") (partition image-size converted))))))

(defn convert-image-black-white-then-to-text
  "Convert any image into the format required by the classifier."
  [in out out-text]
  (sh "convert" in "-colorspace" "gray" "+dither" "-colors" "2"
      "-normalize" out)
  (sh "convert" out out-text)
 ; (write-lines out (load-text-image out))
  )


(defn blob-extraction [the-ones-zeros-string]
  (let [the-2d-map (make-the-2d-map the-ones-zeros-string)]
    (pass1 the-2d-map)
    (pass2 the-2d-map)
    (spit "/home/jared/clojureprojects/thaiocr/debugging.txt" (character-extraction))
                                        ;  @characters-letters-as-strings
    )
  )

(defn main []
  (let [images-directory "/home/jared/clojureprojects/thaiocr/testbmp/"
        temp-directory "/home/jared/clojureprojects/thaiocr/testbmptempdir/"
        all-images-in-directory (fs/list-dir images-directory)
        ]
    (for [one-image all-images-in-directory]
      (do
        (convert-image-black-white-then-to-text (str images-directory one-image) (str temp-directory one-image) (str temp-directory one-image ".txt"))
        (load-text-image (str temp-directory one-image ".txt") (str temp-directory one-image "oneszeros.txt"))
       ; (println (slurp (str temp-directory one-image "oneszeros.txt")))
        (blob-extraction (slurp (str temp-directory one-image "oneszeros.txt")))
        )
      )
    )
  )
