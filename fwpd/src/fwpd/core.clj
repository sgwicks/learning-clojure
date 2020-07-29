(ns fwpd.core)
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str)
  )

(def conversions { ;associates a key with a function
  :name identity   ;identity simply returns its given argument
  :glitter-index str->int ;str->int converts a string to a number
  })

(defn convert
  "Get the relevant function from the conversions map, apply it to the given value"
  [vamp-key value]
  ((get conversions vamp-key) value) 
  )

(defn parse
  "Convert a CSV row into rows of columns"
  [string]
  (map #(clojure.string/split % #",") (clojure.string/split string #"\n"))
  )

(defn mapify 
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]                                     ;takes a list of rows and applies a reduce function to turn them into maps
        (reduce (fn [row-map [vamp-key value]]                ;takes a list of vectors, and reduces them into an initial (empty) map
          (assoc row-map vamp-key (convert vamp-key value)))  ;takes a map (row-map) and adds a vamp-key and a converted value (i.e. "3" -> 3)
          {}
          (map vector vamp-keys unmapped-row)))               ;takes unmapped row and returns a list of vectors like ([:name "Bella Swan"] [:glitter-index "0"])
    rows)
  )

(defn glitter-filter
  [minimum-glitter records]
  (map #(get % :name) (filter #(>= (:glitter-index %) minimum-glitter) records)))

(defn append
  [new-suspect suspects]
  (conj suspects new-suspect)
  )

;takes a suspect in the form {:name "Sam Wicks" :glitter-index 1}
;appends it to a list of suspects in the form ({} {} {})