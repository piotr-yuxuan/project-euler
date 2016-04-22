(ns project-euler.core
  (:use clojure.test)
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd])
  (:gen-class))

(defn -main [& args])

(defn- sign-of
  "Return 1 if positive number, -1 if negative number, 0 otherwise"
  [a]
  (if (and (number? a) (not= a 0))
    (if (>= a 0) 1 -1)
    0))

(defn- diff
  "Return the absolute difference between two numbers."
  [a b]
  (Math/abs (- b a)))

(defn pow
  "Raise x to power n"
  [x n]
  (if (zero? n) 1
      (* x (pow x (dec n)))))

(deftest- test-pow
  (testing "Edge-effect with 0."
    (is (= 1 (pow 0 0)))
    (is (= 0 (pow 0 5) (pow 0 42))))
  (testing "Usual case"
    (is (= 1 (pow 1 1)))
    (is (= 4 (pow 2 2)))
    (is (= 81 (pow 3 4)))))

(defn- format-int
  "Add leading 0 to the list `int` which represents some binary number
  such as the list fits a length. "
  [int length]
  (assert (>= length (count int)))
  (concat (repeat (- length (count int)) 0) int))

(deftest- test-format-int
  (testing "Empty case"
    (is (= [] (format-int [] 0))))
  (testing "Usual case"
    (is (= [0 0 1 0 1] (format-int [1 0 1] 5)))
    (is (= [0 0 0 0 1] (format-int [0 0 1] 5)))
    (is (= [0 0 0 0 0] (format-int [0 0 0] 5))))
  (testing "Expected output length is too short"
    (is (= java.lang.AssertionError (try (format-int [1 0 1] 2)
                                         (catch java.lang.AssertionError e (type e)))))))

(defn- int-to-binary-list
  "Get an integer and return its binary representation as a list."
  ([n]
   (map #(-> % str Integer/parseInt) (Integer/toBinaryString n)))
  ([n length]
   (format-int (int-to-binary-list n) length)))

(defn- count-in
  "count value in a list"
  [value list]
  (count (filter #(= value %) list)))

(defn- acceptable-path?
  "Returns true if the path has correct length and leads to the expected
  ends, false otherwise"
  [path vertical horizontal]
  (and (= vertical (count-in 1 path))
       (= horizontal (count-in 0 path))
       (= (+ vertical horizontal) (count path))))

(def axis
  "The convention used throughout the code"
  {0 :right 1 :top})

(defn- format-path
  [dictionary path]
  (map #(get dictionary %)
       path))

(defn- paths
  "Returns all possible paths."
  [vertical horizontal]
  (let [vertical (dec vertical)
        horizontal (dec horizontal)
        loop-start (- (pow 2 (+ vertical horizontal)) (pow 2 horizontal))
        loop-end (dec (pow 2 vertical))]
    (loop [result []
           current loop-start]
      (if (< 0 current)
        (let [candidate (int-to-binary-list current (+ vertical horizontal))]
          (recur (if (acceptable-path? candidate vertical horizontal)
                   (conj result (format-path axis candidate))
                   result)
                 (dec current)))
        result))))

(defn- shift-position
  [direction [vertical horizontal]]
  (cond (= direction :right) [vertical (inc horizontal)]
        (= direction :top) [(dec vertical) horizontal]
        (= direction :left) [vertical (dec horizontal)]
        (= direction :bottom) [(inc vertical) horizontal]))

(defn- value-of-position
  [m [vertical horizontal]]
  (nth (nth m vertical) horizontal))

(defn- matrix-dim
  [ma]
  (let [vertical (count ma)
        horizontal (count (first ma))]
    [vertical horizontal]))

(defn value-of-path
  [m path]
  (loop [position [(-> m matrix-dim first dec) 0]
         path path
         result (value-of-position m position)]
    (if (nil? path)
      result
      (let [[direction & path] path
            position (shift-position direction position)
            value (value-of-position m position)]
        (recur position
               path
               (+ result value))))))

(defn task3
  "ma is the matrix. The algorithm is to generate all paths from from [0
  0] to [M-1 N-1]. A path is a list of 0 and 1. 0 means go right and 1
  means go up."
  [matrix]
  (let [dimensions (matrix-dim matrix)]
    (if (= 2 (reduce + dimensions))
      (value-of-position matrix [0 0])
      (apply max (map (partial value-of-path matrix)
                      (apply paths (matrix-dim matrix)))))))

(deftest thorough-test-for-task3
  (testing "Minimal matrix works"
    (is (= 7 (task3 [[7]]))))
  (testing "Compute both start and end positions"
    (is (= 5 (task3 [[0 2]
                     [3 0]]))))
  (testing "Give back the highest score"
    (is (= 27 (task3 [[1 2 3]
                      [6 5 4]
                      [9 6 1]]))))
  (testing "Accept rectangular matrices"
    (is (= 27 (task3 [[1 2 3 0]
                      [6 5 4 0]
                      [9 6 1 0]])))
    (is (= 27 (task3 [[0 0 0]
                      [1 2 3]
                      [6 5 4]
                      [9 6 1]])))))

(defn -main
  "Only run tests."
  [& args]
  (run-tests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Follow stub of something else ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- multiple?
  "Is a num multiple of div?"
  [num div]
  (zero? (mod num div)))

(def euler1
  (reduce + (filter #(or (multiple? % 3)
                         (multiple? % 5))
                    (range 1000))))

(def fib-seq
  ((fn rfib [a b]
     (lazy-seq (cons a (rfib b (+ a b)))))
   0 1))

(def euler2
  (reduce + (filter even? (take-while #(< % 4000000) fib-seq))))

(defn primefactors
  ([n]
   (primefactors n 2 '()))
  ([n candidate acc]
   (cond (<= n 1) (distinct acc)
         (zero? (rem n candidate)) (recur (/ n candidate)
                                          candidate
                                          (cons candidate acc))
         :else (recur n (inc candidate) acc))))

(def euler3
  (apply max (primefactors 600851475143)))

(defn palindrome
  "Output a string. Take a string"
  [^java.lang.String n]
  (apply str (reverse n)))

(defn palindrome?
  "Output true if the given number is a palindrome"
  [n]
  (= (str n) (palindrome (str n))))

(defn find-euler4-divisors
  [number]
  (l/run* [q]
    (l/fresh [a b]
      (fd/in a b (fd/interval 100 999))
      (fd/* a b number)
      (l/== q [a b]))))

(def euler4
  (->> (range (* 100 100) (* 999 999))
       reverse
       (filter palindrome?)
       (filter #(not-empty (find-euler4-divisors %)))
       first))

(defn factors
  ([n]
   (factors n 2 '()))
  ([n candidate acc]
   (cond (<= n 1) acc
         (zero? (rem n candidate)) (recur (/ n candidate)
                                          candidate
                                          (cons candidate acc))
         :else (recur n (inc candidate) acc))))

(defn factors-map
  [number]
  (reduce #(assoc % %2 (inc (get % %2 0)))
          {}
          (factors number)))

(defn top-hull-factors-euler5
  [n]
  (reduce *
          (map #(pow (first %)
                     (second %))
               (apply merge-with max (map factors-map
                                          (range 1 (inc n)))))))

(def euler5
  (top-hull-factors-euler5 20))

(defn sum-square
  "Difference between the sum of the squares of the first n integers and
  the square of the sum."
  [n]
  (int (* 1/12
          (- n 1)
          n
          (+ n 1)
          (+ 2 (* 3 n)))))

(def euler6
  (sum-square 100))

(defn gross-nth-prime
  [n]
  (int (+ (* n
             (Math/log n))
          (* n
             (Math/log (Math/log n))))))

(defn erastothene-sieve
  ([n]
   (let [upper-bound (gross-nth-prime n)]
     (erastothene-sieve n
                        1
                        (range 2 (int upper-bound))
                        (Math/sqrt upper-bound))))
  ([n ith lazy milestone]
   (let [[current & lazy] lazy]
     (if (< current milestone)
       (recur n
              (inc ith)
              (filter #(not= 0 (mod % current)) lazy)
              milestone)
       (nth lazy (- n ith 1))))))

(defn prime?
  [n]
  (cond (< n 1) false
        (= n 1) true
        :else (= 1 (count (factors n)))))

(comment
  (def euler7-easy-answer
    (nth (filter prime? (range)) 10001)))

(def euler7
  (erastothene-sieve 10001))

(def euler8-input
  (str
   "73167176531330624919225119674426574742355349194934"
   "96983520312774506326239578318016984801869478851843"
   "85861560789112949495459501737958331952853208805511"
   "12540698747158523863050715693290963295227443043557"
   "66896648950445244523161731856403098711121722383113"
   "62229893423380308135336276614282806444486645238749"
   "30358907296290491560440772390713810515859307960866"
   "70172427121883998797908792274921901699720888093776"
   "65727333001053367881220235421809751254540594752243"
   "52584907711670556013604839586446706324415722155397"
   "53697817977846174064955149290862569321978468622482"
   "83972241375657056057490261407972968652414535100474"
   "82166370484403199890008895243450658541227588666881"
   "16427171479924442928230863465674813919123162824586"
   "17866458359124566529476545682848912883142607690042"
   "24219022671055626321111109370544217506941658960408"
   "07198403850962455444362981230987879927244284909188"
   "84580156166097919133875499200524063689912560717606"
   "05886116467109405077541002256983155200055935729725"
   "71636269561882670428252483600823257530420752963450"))

(defn shift-lazy
  [coll n] (repeat 5 rest)
  (reduce #(%2 %) coll (repeat n rest)))

(defn s2i
  [s]
  (-> s
      str
      Integer/parseInt
      int))

(defn shap
  "Shift and append"
  [coll n]
  (conj (vec (rest (vec coll))) n))

(defn append
  [coll n]
  (conj (vec coll) n))

(defn- set-bits
  [bandwidth bits index]
  (cond (zero? index) []
        (< (count bits) bandwidth) (append bits index)
        (= (count bits) bandwidth) (shap bits index)))

(defn- get-max
  [max-sequence bits]
  (if (> (reduce * bits)
         (reduce * max-sequence))
    bits
    max-sequence))

(defn max-adjacent-product
  ([bandwidth input]
   (let [[index & buffer] input
         index (s2i index)
         bits []]
     (max-adjacent-product bandwidth buffer bits index [index])))
  ([bandwidth buffer bits index max-sequence]
   (if (empty? buffer)
     (get-max max-sequence (shap bits index))
     (let [bits (set-bits bandwidth bits index)
           [index & buffer] buffer]
       (recur bandwidth
              buffer
              bits
              (s2i index)
              (get-max max-sequence bits))))))

(def euler8-smarter
  "Not mine but found in the forum"
  (time (apply max
               (map (fn [charseq] (reduce * (map #(-> % str Integer/parseInt)
                                                charseq)))
                    (partition-all 13 1 euler8-input)))))

(def euler8
  "Elapsed time: 10.636675 msecs"
  (time (reduce *
                (max-adjacent-product 13
                                      euler8-input))))
