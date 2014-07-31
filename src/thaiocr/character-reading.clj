(ns thaiocr.character-reading
  (:require
   [thaiocr.twopassextraction :as twopassextraction :refer [main]]
   [monger.core :as mgcore]
   [monger.collection :as mgcoll]
   [monger.db :as mgdb :refer [get-collection-names]]
            )
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types ObjectId]
           )
  )

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

(defn view-all-documents [conn db]
  (mgcoll/find-maps db "all-documents")
  )

(defn view-special-characters
  []
  (let [conn (mgcore/connect)
        db (mgcore/get-db conn "thai-ocr")
        all-maps (mgcoll/find-maps db "special-characters")
        ]
    (for [one-map all-maps]
      (println (:character-string-ones-zeros one-map))
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
       (:associated-character perfect-match)
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


(defn split-the-character [ones-zeros-string vertical-or-horizontal row-or-column-number]
  (let [string-split-by-lines (clojure.string/split-lines ones-zeros-string)
        row-count (count string-split-by-lines)
        column-count (count (second string-split-by-lines))
        ]
;    (println (second string-split-by-lines))
    (if (= "byrow" vertical-or-horizontal)
      (vector (apply str (map #(str % "\n") (take row-or-column-number string-split-by-lines)))
              (apply str (map #(str % "\n") (take-last (- row-count row-or-column-number) string-split-by-lines))))
      (vector (apply str (map #(str (apply str (take row-or-column-number %)) "\n") string-split-by-lines))
              (apply str (map #(str (apply str (take-last (- column-count row-or-column-number) %)) "\n")  string-split-by-lines)))
      ))
  )

(defn split-ones-zeros-string
  "we can split a ones-zeros-string vertically or horizontally and then reevaluate those characters
takes one ones-zero-string that we assume is a combination of two characters
returns a vector of two characters, the split column or row and the split place
"
  [ones-zeros-string]
  (loop [try-again true]
    (do
      (println ones-zeros-string)
      (println "type 'byrow' or 'bycolumn' to tell us where to split")
      (let [vertical-or-horizontal (read-line)]
        (println "now type the row or column to split at")
        (let [row-or-column-split (read-string (read-line))
              new-characters (split-the-character ones-zeros-string vertical-or-horizontal row-or-column-split)
              ]
          (doall (for [one-character new-characters]
                   (println one-character)
                   ))
          (println "Looks OK? yes/no")
          (let [looks-ok (read-line)]
            (if (= looks-ok "yes")
              [new-characters vertical-or-horizontal row-or-column-split]
              (recur true)
              )
            )
          )
        )
      )
    )
  )


(defn do-one-ones-zeros
  "take one string and add it's corresponding character to the db"
  [one-string conn db]
;  (println "b")
  (let [ones-zeros-string-split-by-line (clojure.string/split-lines one-string)
        number-of-columns (count ones-zeros-string-split-by-line)
        number-of-rows (count (second ones-zeros-string-split-by-line))
        matched-character (character-match? one-string number-of-rows number-of-columns conn db)
        special-character-match (special-character-match? one-string number-of-rows number-of-columns conn db)
        to-print (str (if (not (false? matched-character))
                        matched-character
                        )
                      (if (not (false? special-character-match))
                        special-character-match
                        )
                      )
        ]
    (if (false? matched-character)
      (if (false? special-character-match)
        (do
          (println one-string)
          (println "if special character insert ###")
          (println "if one of these three please type them ำ ญ ฐ")
          (let [string-association (read-line)]
            (cond
             (= string-association "###")
             (let [[strings-split split-by split-at] (split-ones-zeros-string one-string)
                    the-map (make-map-for-one-character one-string string-association split-by split-at)
                    ]
                                        ;check if character is in db
                                        ;check if there is a split for the character
                                        ;if there is a split; split it and send each character back in

                (store-special-characters the-map conn db)
                (println "strings split " strings-split)
                (doall
                 (for [one-of-the-characters-returned strings-split]
                         (do-one-ones-zeros one-of-the-characters-returned conn db)
                         ))
                )
              :else (let [the-map (make-map-for-one-character one-string string-association nil nil)]
                (insert-document the-map conn db)
                )
              )
            )
          )
        (doall
         ;(println special-character-match)
         (let [split-character (split-the-character (:character-string-ones-zeros special-character-match) (:split-by special-character-match) (:split-at special-character-match))]
           ;(print (first split-character))
           (for [one-of-the-characters-returned split-character]
             (do-one-ones-zeros one-of-the-characters-returned conn db)
             ))
         )
        )
      (println matched-character)
      )
    )
  )

(defn ask-what-each-character-is
  "take a vector of strings each containing one chracter in 1s and 0s and ask the user what that chracter is"
  [vector-of-strings conn db]
;  (println "a")
  (let [with-no-empty-strings (filter #(not (or (empty? %) (= "\n1" %))) vector-of-strings)]
    (doall (loop [with-no-empty-strings with-no-empty-strings]
             (when (not (empty? with-no-empty-strings))
               (let [one-string (first with-no-empty-strings)]
                  (do-one-ones-zeros one-string conn db)
                 (recur (drop 1 with-no-empty-strings)))
               )
                                        ; (println (read-line))
             )
           )
    )
  )


(defn -main []
  (doall
   (let [string-groups-of-ones-zeros-character (twopassextraction/main)
         conn (mgcore/connect)
         db (mgcore/get-db conn "thai-ocr")
         ]
     (for [one-string-group string-groups-of-ones-zeros-character]
       (doall (ask-what-each-character-is one-string-group conn db))
       )
     ;(mgcore/disconnect conn)
     )
   )
  )
