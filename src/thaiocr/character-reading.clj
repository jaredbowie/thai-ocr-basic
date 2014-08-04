(ns thaiocr.character-reading
  (:require
   [thaiocr.sentence-writing :as sentence-writing :refer [make-sentence-vector]]
   [thaiocr.twopassextraction :as twopassextraction :refer [character-extraction-main]]
   [monger.core :as mgcore]
   [monger.collection :as mgcoll]
   [monger.db :as mgdb :refer [get-collection-names]]
   [me.raynes.fs :as fs]
            )
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types ObjectId]
           )
  )

(declare ask-what-each-character-is)

; special cases are like
                                        ; ญ which is not connected to itself
                                        ; ำ
                                        ; ฐ


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Basic DB functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Document Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  {:character-string-ones-zeros "0000001" :columns 7 :rows 1 :associated-character "f" :level 1}
  )

(defn character-level [one-character]
  (let [all-levels-map {"ก" 3 "ข" 3 "ฃ" 3 "ค" 3 "ฅ" 3 "ฆ" 3 "ง" 3 "จ" 3 "ฉ" 3 "ช" 3 "ซ" 3 "ฌ" 3 "ญ" 3 "ฎ" 3 "ฏ" 3 "ฐ" 3 "ฑ" 3 "ฒ" 3 "ณ" 3 "ด" 3 "ต" 3 "ถ" 3 "ท" 3 "ธ" 3 "น" 3 "บ" 3 "ป" 3 "ผ" 3 "ฝ" 3 "พ" 3 "ฟ" 3 "ภ" 3 "ม" 3 "ย" 3 "ร" 3 "ล" 3 "ว" 3 "ศ" 3 "ษ" 3 "ส" 3 "ห" 3 "ฬ" 3 "อ" 3 "ฮ" 3 "ฯ" 5 "ะ" 5 "ั" 2 "า" 5 " ำ" 3 "ิ" 2 "ี" 2 "ึ" 2 "ื" 2 "ุ" 4 "ู" 4 "฿" 5 "เ" 5 "แ" 5 "โ" 5 "ใ" 5 "ไ" 5 "ๅ" 3 "ๆ" 5 "็" 1 "่" 1 "้" 1 "๊" 1 "๋" 1 "์" 2 "๐" 5 "๑" 5 "๒" 5 "๓" 5 "๔" 5 "๕" 5 "๖" 5 "๗" 5 "๘" 5 "๙" 5 "a" 5 "b" 5 "c" 5 "d" 5 "e" 5 "f" 5 "g" 5 "h" 5 "i" 5 "j" 5 "k" 5 "l" 5 "m" 5 "n" 5 "o" 5 "p" 5 "q" 5 "r" 5 "s" 5 "t" 5 "u" 5 "v" 5 "w" 5 "x" 5 "y" 5 "z" 5 "A" 5 "B" 5 "C" 5 "D" 5 "E" 5 "F" 5 "G" 5 "H" 5 "I" 5 "J" 5 "K" 5 "L" 5 "M" 5 "N" 5 "O" 5 "P" 5 "Q" 5 "R" 5 "S" 5 "T" 5 "U" 5 "V" 5 "W" 5 "X" 5 "Y" 5 "Z" 5 "1" 5 "2" 5 "3" 5 "4" 5 "5" 5 "6" 5 "7" 5 "8" 5 "9" 5 "0" 5}]
    (get all-levels-map one-character)
      )
  )

(defn make-map-for-one-character [ones-zeros-string associated-character split-by split-at]
  (let [ones-zeros-string-split-by-line (clojure.string/split-lines ones-zeros-string)
        number-of-columns (count ones-zeros-string-split-by-line)
        number-of-rows (count (second ones-zeros-string-split-by-line))
        level (character-level associated-character)
        ]
     {:character-string-ones-zeros ones-zeros-string :columns number-of-columns :rows number-of-rows :associated-character associated-character :level level :split-by split-by :split-at split-at}
    )
  )
;should have a map made for special characters telling where to split

(defn store-special-characters [document-map conn db]
  (let [;conn (mgcore/connect)
        ;db (mgcore/get-db conn "thai-ocr")
        id (ObjectId.)
                                        ;current-timestamp (t/now)
        new-document-map-with-id (merge {:_id id} document-map)
        ]
    (mgcoll/insert db "special-characters" new-document-map-with-id)
    )
  )

(defn insert-document [document-map conn db]
  (let [;conn (mgcore/connect)
        ;db (mgcore/get-db conn "thai-ocr")
        id (ObjectId.)
        ;current-timestamp (t/now)
        new-document-map-with-id (merge {:_id id} document-map)
        ]
    (mgcoll/insert db "all-documents" new-document-map-with-id)
    )
  )

(defn view-all-documents []
  (let [conn (mgcore/connect)
        db (mgcore/get-db conn "thai-ocr")
        all-maps (mgcoll/find-maps db "all-documents")
        ]
    (for [one-map all-maps]
      one-map
      ) )
  )

(defn view-special-characters
  []
  (let [conn (mgcore/connect)
        db (mgcore/get-db conn "thai-ocr")
        all-maps (mgcoll/find-maps db "special-characters")
        ]
    (for [one-map all-maps]
      one-map
      )
    )
  )

(defn view-all-collections-in-db [conn db]
  (mgdb/get-collection-names db)

  )

(defn view-all-dbes [conn]
  (mgcore/get-db-names conn)
  )

(defn remove-all-docs []
  (let [conn (mgcore/connect)
        db (mgcore/get-db conn "thai-ocr")
        ]
     (mgcoll/remove db "all-documents")
    )
  )

(defn remove-special-characters []
  (let [conn (mgcore/connect)
        db (mgcore/get-db conn "thai-ocr")
        ]
    (mgcoll/remove db "special-characters"))
  )

(defn character-match?
  "checks if a character matches if it does returns the character otherwise returns false"
  [one-string number-of-rows number-of-columns conn db]
   (let [;conn (mgcore/connect)
         ;db (mgcore/get-db conn "thai-ocr")
         map-matches (mgcoll/find-maps db "all-documents" {:columns number-of-columns :rows number-of-rows})
         perfect-match (first (filter #(= (:character-string-ones-zeros %) one-string) map-matches))
         ]
     (if (not (empty? perfect-match))
       {:character (:associated-character perfect-match) :level (:level perfect-match)}
       false
       )
    )
  )

(defn special-character-match?
  "checks if a character matches if it does returns the character otherwise returns false
returns map of special-character
"
  [one-string number-of-rows number-of-columns conn db]
   (let [;conn (mgcore/connect)
         ;db (mgcore/get-db conn "thai-ocr")
         map-matches (mgcoll/find-maps db "special-characters" {:columns number-of-columns :rows number-of-rows})
         perfect-match (first (filter #(= (:character-string-ones-zeros %) one-string) map-matches))
         ]
     (if (not (empty? perfect-match))
       perfect-match
       false
       )
    )
  )


(defn split-the-character [ones-zeros-string by-row-or-by-column row-or-column-number]
  (let [string-split-by-lines (clojure.string/split-lines ones-zeros-string)
        row-count (count string-split-by-lines)
        column-count (count (second string-split-by-lines))
        ]
   ; (println ones-zeros-string)
    (if (= "byrow" by-row-or-by-column)
      (vector (apply str (map #(str % "\n") (take row-or-column-number string-split-by-lines)))
              (apply str (map #(str % "\n") (take-last (- row-count row-or-column-number) string-split-by-lines))))
      (vector (apply str (map #(str (apply str (take row-or-column-number %)) "\n") string-split-by-lines))
              (apply str (map #(str (apply str (take-last (- column-count row-or-column-number) %)) "\n")  string-split-by-lines)))
      ))
  )


(defn determine-each-character-placement
  "determine two characters new positions after a split"
  [lowest-row highest-row lowest-column highest-column by-row-or-by-column row-or-column-split-number]
  (if (= by-row-or-by-column "byrow")
    (let [lowest-row-first lowest-row
          highest-row-first row-or-column-split-number
          lowest-column-first lowest-column
          highest-column-first highest-column
          lowest-row-second (+ 1 row-or-column-split-number)
          highest-row-second highest-row
          lowest-column-second lowest-column
          highest-column-second highest-column
          ]
      [lowest-row-first highest-row-first lowest-column-first highest-column-first lowest-row-second highest-row-second lowest-column-second highest-column-second]
      )
    (let [lowest-row-first lowest-row
          highest-row-first highest-row
          lowest-column-first lowest-column
          highest-column-first row-or-column-split-number
          lowest-row-second lowest-row
          highest-row-second highest-row
          lowest-column-second (+ 1 row-or-column-split-number)
          highest-column-second highest-column
          ]
      [lowest-row-first highest-row-first lowest-column-first highest-column-first lowest-row-second highest-row-second lowest-column-second highest-column-second]
      )
    )
  )

(defn send-back-characters-and-placement
  "for use with 2 strings split from one
new-characters is a vector of 2 strings
"
  [lowest-row highest-row lowest-column highest-column by-row-or-by-column row-or-column-split-number new-characters]
 ; (println lowest-row highest-row lowest-column highest-column by-row-or-by-column row-or-column-split-number new-characters)
  (let [[lowest-row-first highest-row-first lowest-column-first highest-column-first lowest-row-second highest-row-second lowest-column-second highest-column-second] (determine-each-character-placement lowest-row highest-row lowest-column highest-column by-row-or-by-column row-or-column-split-number)]
    [[{:character (first new-characters) :lowest-row lowest-row-first :highest-row highest-row-first :lowest-column lowest-column-first :highest-column highest-column-first}
      {:character (second new-characters) :lowest-row lowest-row-second :highest-row highest-row-second :lowest-column lowest-column-second :highest-column highest-column-second}]
     by-row-or-by-column
     row-or-column-split-number]
    )
  )

(defn split-ones-zeros-string
  "we can split a ones-zeros-string vertically or horizontally and then reevaluate those characters
takes one ones-zero-string that we assume is a combination of two characters
returns a vector of two characters, the split column or row and the split place
"
  [ones-zeros-string-map]
  (let [ones-zeros-string (:character ones-zeros-string-map)
        lowest-row (:lowest-row ones-zeros-string-map)
        highest-row (:highest-row ones-zeros-string-map)
        lowest-column (:lowest-column ones-zeros-string-map)
        highest-column (:highest-column ones-zeros-string-map)
        split-at ()
        ]
    (loop [try-again true]
      (do
        (println ones-zeros-string-map)
        (println "type 'byrow' or 'bycolumn' to tell us where to split")
        (let [by-column-or-by-row (read-line)]
          (println "now type the row or column to split at")
          (let [row-or-column-number (read-string (read-line))
                new-characters (split-the-character ones-zeros-string by-column-or-by-row row-or-column-number)
                ]
            (doall (for [one-character new-characters]
                     (println one-character)
                     ))
            (println "Looks OK? yes/no")
            (let [looks-ok (read-line)]
              (if (= looks-ok "yes")
                (send-back-characters-and-placement lowest-row highest-row lowest-column highest-column by-column-or-by-row row-or-column-number new-characters)
                (recur true)
                )
              )
            )
          )
        )

      )
    )
  )

(defn do-one-ones-zeros
  "take one string and add it's corresponding character to the db"
  [first-character-map conn db]
;  (println first-character-map)
  (let [one-string (:character first-character-map)
        ones-zeros-string-split-by-line (clojure.string/split-lines one-string)
        number-of-columns (count ones-zeros-string-split-by-line)
        number-of-rows (count (second ones-zeros-string-split-by-line))
        matched-character (character-match? one-string number-of-rows number-of-columns conn db)
        special-character-match (special-character-match? one-string number-of-rows number-of-columns conn db)
        #_(comment to-print (str (if (not (false? matched-character))
                                 matched-character
                                 )
                               (if (not (false? special-character-match))
                                 special-character-match
                                 )
                               ))
        ]
    (if matched-character
      {:character (:character matched-character) :level (:level matched-character) :lowest-row (:lowest-row first-character-map) :highest-row (:highest-row first-character-map) :lowest-column (:lowest-column first-character-map) :highest-column (:highest-column first-character-map)}
      (if (false? special-character-match)
        (do
          (println one-string)
          (println "if special character insert ###")
          (println "if one of these three please type them ำ ญ ฐ")
          (let [string-association (read-line)]
            (cond
             (= string-association "###")
             (let [[strings-split-maps split-by split-at] (split-ones-zeros-string first-character-map)
                   the-map (make-map-for-one-character one-string string-association split-by split-at)
                  ; string-map-a {:character (first strings-split) :lowest-row :highest-row :lowest-column :highest-column}
                  ; string-map-b {:character (second strings-split) :lowest-row :highest-row :lowest-column :highest-column}
                    ]
                    ;check if character is in db
                    ;check if there is a split for the character
                    ;if there is a split; split it and send each character back in
                (store-special-characters the-map conn db)
                (println "strings split " strings-split-maps)
                (println "next ")
                (doall (ask-what-each-character-is strings-split-maps conn db)
                 )
                )
             (= string-association "ะ") (println "ะ")
             (= string-association "ำ") (println "ำ")
             (= string-association "ญ") (println "ญ")
             (= string-association "ฐ") (println "ฐ")
             (= string-association "i") (println "i")
             (= string-association "j") (println "j")
             :else (let [the-map (make-map-for-one-character one-string string-association nil nil)]
                    ; (println the-map)
                     (insert-document the-map conn db)
                     nil
                )
              )
            )
          )
        (doall
         (let [split-characters-vector (split-the-character (:character first-character-map) (:split-by special-character-match) (:split-at special-character-match))
               [split-characters-maps by-row-or-by-column row-or-column-split] (send-back-characters-and-placement (:lowest-row first-character-map) (:highest-row first-character-map) (:lowest-column first-character-map) (:highest-column first-character-map) (:split-by special-character-match) (:split-at special-character-match) split-characters-vector)]
;           (println "split-char " split-characters-maps)
           (doall (ask-what-each-character-is split-characters-maps conn db)
                  )
           )
         )
        )
      )
    )
  )

(defn ask-what-each-character-is-check-param
  [vec-of-maps-strings conn db]
  (if (vector? vec-of-maps-strings)
    (if (empty? (filter #(not (map? %)) vec-of-maps-strings))
      (if (not (empty? (filter #(and (contains? % :character) (contains? % :lowest-row) (contains? % :highest-row) (contains? % :lowest-column) (contains? % :highest-column)) vec-of-maps-strings)))
        true
        "maps have wrong parameters"
        )
      "inside the vector is not only maps"
      ;["inside the vector is not only maps" vec-of-maps-strings]
      )
    ["not passed a vector" vec-of-maps-strings]
    )
  )

(defn ask-what-each-character-is
  "take a vector of strings each containing one chracter in 1s and 0s and ask the user what that chracter is
accepts: vec-of-maps-strings, maps of type {:character new-character :lowest-row lowest-row :highest-row highest-row :lowest-column lowest-column :highest-column highest-column}
connection to db and the db we are connected to
"
  [vec-of-maps-strings conn db]
  (let [params-check (ask-what-each-character-is-check-param vec-of-maps-strings conn db)]
    (if (true? params-check)
      (do
 ;       (println "vec-of-maps-string " vec-of-maps-strings)
        (let [with-no-empty-strings (filter #(not (or (empty? %) (= "\n1" (:character %)) (= "11" (:character %)))) vec-of-maps-strings)]
          (doall (loop [with-no-empty-strings with-no-empty-strings
                        character-maps-found []
                        ]
                   (if (empty? with-no-empty-strings)
                     character-maps-found
                     (let [first-character-map (first with-no-empty-strings)]
                       (recur (drop 1 with-no-empty-strings)
                              (conj character-maps-found (do-one-ones-zeros first-character-map conn db))))
                     )
                   )
                 )
          )
        )
      params-check
      )
    )
  )


(defn character-identification-main [string-groups-of-ones-zeros-character]
;  (println string-groups-of-ones-zeros-character)
  (doall
   (let [conn (mgcore/connect)
         db (mgcore/get-db conn "thai-ocr")
         vector-of-character-maps (for [one-string-group string-groups-of-ones-zeros-character]
                                    (do
                                      (let [group-of-characters (first one-string-group)
                                            the-whole-ones-zero-pattern (second one-string-group)]
                                        (doall (ask-what-each-character-is group-of-characters conn db))))
           )]
     ;(let [])
     (doall (sentence-writing/make-sentence-vector (flatten vector-of-character-maps)))
     )
   )
  )


(defn -main []
 (let [images-directory "/home/jared/clojureprojects/thaiocr/testbmp/"
       temp-directory "/home/jared/clojureprojects/thaiocr/testbmptempdir/"
       all-images-in-directory (fs/list-dir images-directory)
       ]
   ;(println all-images-in-directory)
   (doall (for [one-image all-images-in-directory]
            (do
              ;(println one-image)
              (let [vector-of-character-maps [(doall (twopassextraction/character-extraction-main one-image images-directory temp-directory))]]
                (doall (character-identification-main vector-of-character-maps))
                )
              )
            )
          )
    )
  )
