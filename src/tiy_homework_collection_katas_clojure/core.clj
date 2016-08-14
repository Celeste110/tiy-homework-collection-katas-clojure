(ns tiy-homework-collection-katas-clojure.core)

;; Given an vector of integers, return true if 6 appears as either the first
;; or last element in the vector. The vector will be length 1 or more.
(defn firstLast6 [a]
  (if (or (= (first a) 6) (= (peek a) 6))
    true
    false))


;; Given an vector of integers, return true if the vector is length 1 or more,
;; and the first element and the last element are equal.
(defn sameFirstLast [a]
  (if (and (>= (count a) 1) (= (first a) (peek a)))
    true
    false))



;; Given an vector of integers length 3, return an vector with the elements
;; "rotated left" so [1, 2, 3] yields [2, 3, 1].
(defn rotateLeft3 [a]
  (subvec (conj a (first a)) 1))

;; Given an vector of Integers length 3, return a new vector with the elements
;; in reverse order, so [1, 2, 3] becomes [3, 2, 1].
(defn reverse3 [a]

  (loop [aList a
         b []
         x (count a)]

    (if (> x 0)
      (do
        (recur (pop aList) (conj b (peek aList)) (- x 1)))  ;; loop until elements are transferred into new vector

      b)))                                                  ;; (implicitly) return vector containing elements in reverse order

;; Given an vector of integers of at least length 2, return the sum of the first element
;; in the vector and the second from last element. If the vector length is less than 2,
;; just sum up the elements that exist, returning 0 if the vector is length 0.
(defn sumFirstPenultimate [a]

  (if (>= (count a) 2)
    (do
      (+ (first (pop a)) (peek (pop a))))                   ;; REMEMBER: the pop function returns a list! It does not modify the original list!!!
    ;; do returns the value of its last expression.
    (do
      (if (= (count a) 0)
        0)

      (loop [i (count a)
             sum 0
             aList a]

        (if (< i 2)
          (recur (- i 1) (+ (peek aList) sum) (pop aList))))))) ;; loop and sum if false

;; Modify and return the given map as follows: if the key "a" has a value, set the key "b"
;; to have that value, and set the key "a" to have the value "". Basically "b" is a bully,
;; taking the value of "a".
(defn mapBully [a]
  (if (not= (get a :a) nil)
    (assoc (assoc a :b (get a :a)) :a (str ""))             ;; NOTE: the assoc-in function is used to update value of keys
    ;; takes a function as its argument!
    )
  )

;; Modify and return the given map as follows: if the key "a" has a value, set the key "b"
;; to have that same value. In all cases remove the key "c", leaving the rest of the map unchanged.
(defn mapShare [a]
  (if (not= (get a :a) nil)
    (dissoc (assoc a :b (get a :a)) :c)
    (dissoc a :c)                                           ;; dissoc[iate]. returns a new map of the same type, that does not contain mapping for key(s)
    )

  )

;; Modify and return the given map as follows: for this problem the map may or may not contain
;; the "a" and "b" keys. If both keys are present, append their 2 string values together and
;; store the result under the key "ab".
(defn mapAB [a]
  (if (and (not= (get a :a) nil) (not= (get a :b) nil))
    (assoc a :ab (str (get a :a) (get a :b)))
    a
    )
  )

;; Given an vector of strings, return a map containing a key for every different string in the
;; vector, and the value is that string's length.
(defn wordLen [a]

  (loop [aList a
         b {}
         x (count a)
         last-word-in-list (peek aList)
         ]

    (if (> x 0)
      (recur (pop aList) (assoc-in b [last-word-in-list] (count last-word-in-list)) (- x 1) (peek aList)) ;; loop until elements are transferred into new vector
      (assoc-in b [last-word-in-list] (count last-word-in-list)))) ;; add last word (index 0) to the map
  )

;; Given an vector of words, return a map containing a key for every word's first letter.
;; The value for the key will be an vector of all words in the list that start with that letter.
;; An empty string has no first letter so don't add a key for it.
(defn indexWords [a]

  (if (= (count a) 0)
    {}
    )

  (if (= (count a) 1)
    (get a 0)


    (loop [b {}
           x 0
           aList a
           ]

      (if (< x (count a))
        (recur (assoc-in b [(subs (peek aList) 0 1)] (conj (get b (subs (peek aList) 0 1)) (peek aList)))
               (+ x 1) (pop aList))
        b)
      )))

(defn -main []
  ;; 1. call firstLast6
  (print "(firstLast6 [1, 2, 6]) -> ")
  (println (firstLast6 [1, 2, 6]))
  (print "(firstLast6 [6, 1, 2, 3]) -> ")
  (println (firstLast6 [6, 1, 2, 3]))
  (print "(firstLast6 [13, 6, 1, 2, 3]) -> ")
  (println (firstLast6 [13, 6, 1, 2, 3]))

  ;; 2. call sameFirstLast
  (print "\n(sameFirstLast [1, 2, 3]) -> ")
  (println (sameFirstLast [1, 2, 3]))
  (print "(sameFirstLast [1, 2, 3, 1]) -> ")
  (println (sameFirstLast [1, 2, 3, 1]))
  (print "(sameFirstLast [1, 2, 1]) -> ")
  (println (sameFirstLast [1, 2, 1]))

  ;; 3. call rotateLeft3
  (print "\n(rotateLeft3 [1, 2, 3]) -> ")
  (println (rotateLeft3 [1, 2, 3]))
  (print "(rotateLeft3 [5, 11, 9]) -> ")
  (println (rotateLeft3 [5, 11, 9]))
  (print "(rotateLeft3 [7, 0, 0]) -> ")
  (println (rotateLeft3 [7, 0, 0]))

  ;; 4. call reverse3
  (print "\n(reverse3 [1, 2, 3]) -> ")
  (println (reverse3 [1, 2, 3]))
  (print "(reverse3 [5, 11, 9]) -> ")
  (println (reverse3 [5, 11, 9]))
  (print "(reverse3 [7, 0, 0]) -> ")
  (println (reverse3 [7, 0, 0]))

  ;; 5. call sumFirstPenultimate
  (print "\n(sumFirstPenultimate [1, 2, 3]) -> ")
  (println (sumFirstPenultimate [1, 2, 3]))
  (print "(sumFirstPenultimate [1, 1]) -> ")
  (println (sumFirstPenultimate [1, 1]))
  (print "(sumFirstPenultimate [1, 1, 1, 1]) -> ")
  (println (sumFirstPenultimate [1, 1, 1, 1]))
  (print "(sumFirstPenultimate [1, 2, 3, 4]) -> ")
  (println (sumFirstPenultimate [1, 2, 3, 4]))


  ;; 6. call mapBully
  (print "\n(mapBully {\"b\": \"dirt\", \"a\": \"candy\"}) -> ")
  (println (mapBully {:b "dirt"
                      :a "candy"}))
  (print "(mapBully {\"a\": \"candy\"}) -> ")
  (println (mapBully {:a "candy"}))
  (print "(mapBully {\"b\": \"carrot\", \"c\": \"meh\", \"a\": \"candy\"}) -> ")
  (println (mapBully {:b "carrot"
                      :c "meh"
                      :a "candy"}))

  ;; 7. call mapShare
  (print "\n(mapShare {\"b\": \"bbb\", \"c\": \"ccc\", \"a\": \"aaa\"}) -> ")
  (println (mapShare {:b "bbb"
                      :c "ccc"
                      :a "aaa"}))
  (print "(mapShare {\"b\": \"xyz\", \"c\": \"ccc\"}) -> ")
  (println (mapShare {:b "xyz"
                      :c "ccc"}))
  (print "(mapShare {\"d\": \"hi\", \"c\": \"meh\", \"a\": \"aaa\"}) -> ")
  (println (mapShare {:d "hi"
                      :c "meh"
                      :a "aaa"}))
  ;; 8. call mapAB
  (print "\n(mapAB {\"b\": \"There\", \"a\": \"Hi\"}) -> ")
  (println (mapAB {:b "There"
                   :a "Hi"
                   }))
  (print "(mapAB {\"a\": \"Hi\"}) -> ")
  (println (mapAB {:a "Hi"}))
  (print "(mapAB {\"b\": \"There\"}) -> ")
  (println (mapAB {:b "There"}))

  ;; 9. call wordLen
  (print "\n(wordLen [\"a\", \"bb\", \"a\", \"bb\"]) -> ")
  (println (wordLen ["a", "bb", "a", "bb"]))
  (print "(wordLen [\"this\", \"and\", \"that\", \"and\"]) -> ")
  (println (wordLen ["this", "and", "that", "and"]))
  (print "(wordLen [\"code\", \"code\", \"code\", \"bug\"]) -> ")
  (println (wordLen ["code", "code", "code", "bug"]))

  ;; 10. call indexWord
  (print "\n(indexWords [\"aardvark\", \"apple\", \"zamboni\", \"phone\"]) -> ")
  (println (indexWords ["aardvark", "apple", "zamboni", "phone"]))
  (print "(indexWords [\"elephant\"]) -> ")
  (println (indexWords ["elephant"]))
  (print "(indexWords []) -> ")
  (println (indexWords []))
  (print "(indexWords [\"\"]) -> ")
  (println (indexWords [""]))
  )
(-main)
