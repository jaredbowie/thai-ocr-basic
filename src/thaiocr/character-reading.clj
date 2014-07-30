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
;  ปั which collides
;  ก็ไ which collides
;  ญ which is not connected to itself
; ด้โ
; ห้ใ


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
  (let [all-levels-map {"ก" 3 "ข" 3 "ฃ" 3 "ค" 3 "ฅ" 3 "ฆ" 3 "ง" 3 "จ" 3 "ฉ" 3 "ช" 3 "ซ" 3 "ฌ" 3 "ญ" 3 "ฎ" 3 "ฏ" 3 "ฐ" 3 "ฑ" 3 "ฒ" 3 "ณ" 3 "ด" 3 "ต" 3 "ถ" 3 "ท" 3 "ธ" 3 "น" 3 "บ" 3 "ป" 3 "ผ" 3 "ฝ" 3 "พ" 3 "ฟ" 3 "ภ" 3 "ม" 3 "ย" 3 "ร" 3 "ล" 3 "ว" 3 "ศ" 3 "ษ" 3 "ส" 3 "ห" 3 "ฬ" 3 "อ" 3 "ฮ" 3 "ฯ" 5 "ะ" 5 "ั" 2 "า" 5 " ำ" 3 "ิ" 2 "ี" 2 "ึ" 2 "ื" 2 "ุ" 4 "ู" 4 "฿" 5 "เ" 5 "แ" 5 "โ" 5 "ใ" 5 "ไ" 5 "ๅ" 3 "ๆ" 5 "็" 1 "่" 1 "้" 1 "๊" 1 "๋" 1 "์" 2 "๐" 5 "๑" 5 "๒" 5 "๓" 5 "๔" 5 "๕" 5 "๖" 5 "๗" 5 "๘" 5 "๙" 5}]
    (get all-levels-map one-character)
      )
  )

(defn make-map-for-one-character [ones-zeros-string associated-character]
  (let [ones-zeros-string-split-by-line (clojure.string/split-lines ones-zeros-string)
        number-of-columns (count ones-zeros-string-split-by-line)
        number-of-rows (count (second ones-zeros-string-split-by-line))
        level (character-level associated-character)
        ]
     {:character-string-ones-zeros ones-zeros-string :columns number-of-columns :rows number-of-rows :associated-character associated-character :level level}
    )
  )


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
  [conn db]
  (mgcoll/find-maps db "special-characters")

  )

(defn view-all-collections-in-db [conn db]
  (mgdb/get-collection-names db)

  )

(defn view-all-dbes [conn]
  (mgcore/get-db-names conn)
  )

(defn remove-all-docs [conn db]
  (mgcoll/remove db "all-documents")
  )

(defn remove-special-characters [conn db]
  (let [;conn (mgcore/connect)
        ;db (mgcore/get-db conn "thai-ocr")
        ]
    (mgcoll/remove db "special-characters"))
  )

(def sample ["\n0000011111110000\n0001111111111100\n0111100000011110\n0111000000001111\n1110000000000111\n1110001111000111\n1110011111100111\n1110011001100111\n1110011001100111\n1110011001100111\n0111001111000111\n0111001110000111\n0111011100000111\n0011111000000111\n0011110000000111\n0011110000000111\n0011100000000111" "\n0000111111110000\n0011111111111100\n1111100000011110\n0111100000000111\n0011110000000111\n0001111000000111\n0011110000000111\n0011100000000111\n0011100000000111\n0011100000000111\n0011100000000111\n0011100000000111\n0011100000000111\n0011100000000111\n0011100000000111\n0011100000000111\n0011100000000111" "\n0111110000001\n1111111000001\n1100011000001\n1100011000011\n1111111000111\n0111111001110\n0001110111100\n0011111110000\n1111110000000" "\n0000000000011100000000000\n0000000000011110000111000\n0011111111111110001111100\n0111111111111111001111100\n1111000000000111011111100\n1110000000000011111011100\n1110110111100001110011100\n1110111111110001110011000\n1111111100110000100011000\n0111100111110000000111000\n0011100011100000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111000\n0000000000000000000111110\n0000000000000000000111111\n0000000000000000000110011\n0000000000000000000110011\n0000000000000000000111111\n0000000000000000000011110" "\n1"]
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
  "checks if a character matches if it does returns the character otherwise returns false"
  [one-string number-of-rows number-of-columns conn db]
   (let [;conn (mgcore/connect)
         ;db (mgcore/get-db conn "thai-ocr")
         map-matches (mgcoll/find-maps db "special-characters" {:columns number-of-columns :rows number-of-rows})
         perfect-match (first (filter #(= (:character-string-ones-zeros %) one-string) map-matches))
         ]
     (if (not (empty? perfect-match))
       (:associated-character perfect-match)
       false
       )
    )
  )


(defn ask-what-each-character-is
  "take a vector of strings each containing one chracter in 1s and 0s and ask the user what that chracter is"
  [vector-of-strings conn db]
  (let [with-no-empty-strings (filter #(not (or (empty? %) (= "\n1" %))) vector-of-strings)]
    (doall (loop [with-no-empty-strings with-no-empty-strings]
             (when (not (empty? with-no-empty-strings))
               (do
                 (let [one-string (first with-no-empty-strings)
                       ones-zeros-string-split-by-line (clojure.string/split-lines one-string)
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
                   (if (not (and (false? matched-character) (false? special-character-match)))

                     (println to-print)
                     (do
                       (println one-string)
                       (println "if special character insert ###")
                       (let [string-association (read-line)

                             map-for-character (make-map-for-one-character one-string string-association)
                             ]
                         (let [the-map (make-map-for-one-character one-string string-association)]
                           (if (= string-association "###")
                             (store-special-characters the-map conn db)
                             (insert-document the-map conn db)

                                                 ))
                         )
                       )
                     )
                   )
                 (recur (drop 1 with-no-empty-strings))
                 )
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
