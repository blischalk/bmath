(ns bmath.core)

(defn square [n]
  (* n n))

(defn mean
  "Calculates the aritmetic mean of a sequence of values"
  [vals]
  (/ (apply + vals) (count vals)))

(mean [6 6 2 6 6 3 2 6])

(defn factorial
  "Calculates the factorial of a number using a iterative
  recursive process."
  ([n] (factorial n n))
  ([n prev]
   (cond (= n 0)       1
         (= (dec n) 0) prev
         :else         (recur (dec n)
                              (* prev
                                 (dec n))))))

;; Permutations of letters in word "slime" is factorial of 5

;; If you have 6 reindeer but only 4 can fly
;; 6 can go in the first slot
;; 5 in the second
;; 4 in the third
;; 3 in the fourth
;; So 6 * 5 * 4 * 3 would be one wat to solve it
;; Another would be (/ 6! (- 6 4)!) = 360
;; Exclamation means factorial function e.g n * n-1 * n-n

(defn gcd [a b]
  (if (= 0 b) a
      (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& eles]
  (reduce lcm eles))

(defn lcm-eles [& eles]
  (letfn [(gcd [a b]
            (if (= 0 b) a
                (recur b
                       (mod a b))))
          (lcm [a b]
            (/ (* a b)
               (gcd a b)))]
         (reduce lcm eles)))
