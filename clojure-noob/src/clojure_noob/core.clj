(ns clojure-noob.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I'm a big teapot"))

(defn no-params
  []
  "I have no parameters")

(defn x-chop
  ([name chop]
  (str "I " chop "-chop " name))
  
  ([name]
    (str "I karate-chop " name))

  ([]
    (str "I karate-chop John"))
    )

  (defn like
    [thing]
    (str "I like " thing))

  (defn things-i-like
    [item & things]
    (nth (map like things) item))

  (defn inc-maker
    [inc-by]
    #(+ % inc-by))

  (defn verbose-inc-maker
    [inc-by]
    (fn [num] (+ num inc-by)))

  (def asym-hobbit-body-parts [
      {:name "head" :size 3}
      {:name "left-eye" :size 1}
      {:name "left-ear" :size 1}
      {:name "mouth" :size 1}
      {:name "nose" :size 1}
      {:name "neck" :size 2}
      {:name "left-shoulder" :size 3}
      {:name "left-upper-arm" :size 3}
      {:name "chest" :size 10}
      {:name "back" :size 10}
      {:name "left-forearm" :size 3}
      {:name "abdomen" :size 6}
      {:name "left-kidney" :size 1}
      {:name "left-hand" :size 2}
      {:name "left-knee" :size 2}
      {:name "left-thigh" :size 4}
      {:name "left-lower-leg" :size 3}
      {:name "left-achilles" :size 1}
      {:name "left-foot" :size 2}
      ])

  (defn matching-part
    [part]
    {:name (clojure.string/replace (:name part) #"^left-" "right-")
    :size (:size part)})

  (defn symmetrize-body-parts
    [parts-vector]
    (loop [remaining-parts parts-vector
      final-body-parts []]
      (if (empty? remaining-parts)
        final-body-parts
        (let [[part & remaining] remaining-parts]
          (recur remaining
          (into final-body-parts
            (set [part (matching-part part)])))))))

  (defn reduce-body-parts
    [body-parts]
    (reduce (fn [final-body-parts part]
        (into final-body-parts (set [part (matching-part part)])))
      []
      body-parts))

  (defn hit
    [asym-body-parts]
    (let [
      sym-parts (reduce-body-parts asym-body-parts)
      body-part-size-sum (reduce + (map :size sym-parts))
      target (rand body-part-size-sum)
        ]
      (loop [
        [part & remaining] sym-parts
        accumulated-size (:size part)
          ]
          (if (> accumulated-size target)
            part
          (recur remaining (+ accumulated-size (:size (first remaining))))))))

  (defn plus-100
    [num]
    (+ num 100))

  (defn dec-maker
    [dec-by]
    #(- % dec-by))

  (defn verbose-dec-maker
    [dec-by]
    (fn [num] (- num dec-by)))

  (defn mapset
    [func vector]
    (set (map func vector))
    )

  (def alien-body-parts [
    {:name "first-leg"}
    {:name "first-eye"}
    {:name "first-arm"}
    {:name "head"}
    {:name "torso"}
  ])

  (defn new-part
    [part prefix]
    {:name (clojure.string/replace (:name part) #"^first-" prefix)}
    )

  (defn complete-alien
    [body-parts]
    (loop [
      remaining-parts body-parts
      final-body-parts []
        ]
      (if (empty? remaining-parts)
        final-body-parts
        (let [
          [part & remaining] remaining-parts
        ]
          (recur remaining
            (into final-body-parts (set [
              part
              (new-part part "second-") 
              (new-part part "third-")
              (new-part part "fourth-")
              (new-part part "fifth-")
                ]))
            )
          )
        )
      )
    )

  (def old-map
    {:a "include this" :b "include this" :c "not this"}
    )

  (defn create-new-map
    [new-map [key value]]
  (if (= "include this" value)
    (assoc new-map key value)
    new-map
  )
    )

  (def maps
    [{:a 1 :b 2} {:a 2 :b 3} {:a 3 :b 4}]
    )

  (defn add-values-to-map
    [map [key value]]
    (assoc map key value)
    )

  (defn add-values-to-maps
    [maps values]
    (reduce (fn [new-maps map]
      (conj new-maps (add-values-to-map map values)))
      []
      maps)
    )

    ; (defn reduce-body-parts2
    ;   [body-parts]
    ;   (reduce (fn [final-body-parts part]
    ;       (into final-body-parts (set [part (matching-part part)])))
    ;     []
    ;     body-parts))

    (def vampire-database
      {0 {:makes-blood-puns? false, :has-pulse? true  :name "McFishwich"}
       1 {:makes-blood-puns? false, :has-pulse? true  :name "McMackson"}
       2 {:makes-blood-puns? true,  :has-pulse? false :name "Damon Salvatore"}
       3 {:makes-blood-puns? true,  :has-pulse? true  :name "Mickey Mouse"}})
    
    (defn vampire-related-details
      [social-security-number]
      (Thread/sleep 1000)
      (get vampire-database social-security-number))
    
    (defn vampire?
      [record]
      (and (:makes-blood-puns? record)
           (not (:has-pulse? record))
           record))
    
    (defn identify-vampire
      [social-security-numbers]
      (first (filter vampire?
                     (map vampire-related-details social-security-numbers))))
    
    (def not-vampire? (complement vampire?))

    (defn identify-humans
      [social-security-numbers]
      (filter (complement vampire?)
        (map vampire-related-details social-security-numbers)
      ))