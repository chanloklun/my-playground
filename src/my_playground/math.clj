;
; Stuff related to math
;

(ns my-playground.math)

;;;; Fibonacci

;
; Returns an infinite sequence of Fibonacci numbers.  Note that F(0)=F(1)=1.
;
(defn fibonacci-seq
  ([] (fibonacci-seq [0 1]))
  ([[x y]] (lazy-seq (cons y (fibonacci-seq [y (+ y x)])))))

;
; Returns the nth Fibonacci number.  Note that F(0)=F(1)=1.
;
(defn fibonacci [n]
  (nth (fibonacci-seq) n))

;;;; Combinatorics

;
; Takes a set of elements s = #{x1 x2 x3 ... xn} and returns a sequence of
; 2-element vectors in the form of [x s\{x}] for each x in s.
;
; Example:
;   s = #{1 2 3 4},
;   (split s) = ([1 #{2 3 4}] [2 #{1 3 4}] [3 #{1 2 4}] [4 #{1 2 3})
;
(defn split [s]
  (map (fn [x] [x (disj s x)]) s))

;
; Returns a sequence with element e conj'ed to each collection in the sequence
; of collections colls.
;
; Example:
;   e = 4, colls = ([1 2] [2 3] [1 3])
;   (conj-elem-to-colls e colls) = ([1 2 4] [2 3 4] [1 3 4])
;
(defn conj-elem-to-colls [e colls]
  (map #(conj % e) colls))

;
; Produces a sequence of n-permutations of elements in s.  If length is not 
; specified, it defaults to size of set.
;
; Example:
;   s = #{1 2 3}
;   (permutations s) = ([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])
;   (permutations s 2) = ([1 2] [1 3] [2 1] [2 3] [3 1] [3 2])
;
; Implementation Notes:
;   The idea is to generate all permutations recursively.  Suppose you have
;   a set s = #{x1 x2 x3 ... xm}.  First, take x1 out, and generate all
;   permutations of length (n-1) for #{x2 x3 ... xm}.  You then attach x1 to
;   the beginning of each length (n-1) permutations to obtain all length n
;   permutations beginning with x1.  Repeat same procedure for x2, x3 ... xm.
;
(defn permutations
  ([s] (permutations s (count s)))
  ([s n] (cond
           (or (neg? n) (> n (count s))) (throw (IllegalArgumentException. "Invalid length n"))
           (or (zero? n) (empty? s)) [[]]
           :else (mapcat (fn [[x remaining]]
                           (conj-elem-to-colls x (permutations remaining (dec n))))
                         (split s)))))

;
; Produces a sequence of n-combinations of elements in s.  Each combination 
; is itself a set.
;
; Example:
;   s = #{1 2 3 4}
;   (combinations s 2) = (#{1 2} #{1 3} #{1 4} #{2 3} #{2 4} #{3 4})
;
; Implementation Notes:
;   Again, we use recursion.  Combinations of length n for set s = #{x1 x2 ...
;   xm} is given by all combinations with x1 present union with all combinations
;   without x1.  So, the algorithm proceeds with removing x1 from s, generating
;   all length (n-1) combinations for set #{x2 x3 ... xm} and add x1 to each.
;   Finally, union this first set of results with all combinations of length n
;   for set #{x2 x3 ... xm}.
;
(defn combinations [s n]
  (cond
    (or (neg? n) (> n (count s))) (throw (IllegalArgumentException. "Invalid length n"))
    (zero? n) [#{}]
    (= n (count s)) [s]
    :else (let [x (first s)
                remaining (disj s x)]
            (concat (conj-elem-to-colls x (combinations remaining (dec n)))
                    (combinations remaining n)))))

;;;; Newton's Method

;
; As someone who comes from Java, this is truly eye opening. With just 30 lines
; of code, I have written a generic equation solver that utilizes the Newton's
; method.  By generic, I mean I don't need to write a specific program to
; solve, say x^2 = 0, and another one for, say, 7x^3 + 2x^2 + x - 19 = 0.  With
; Clojure, my equation solver below takes an "equation" as a function and
; generates functions necessary to compute via Newton's Method.  It's not just
; code size but also its expressiveness. I don't even know if it's possible to
; do this in Java.
;

;
; A generator takes a function g and returns a sequence generating function
; that takes a single input, x, and produces
;     (g(x), g(g(x)), g(g(g(x))) ...)
;
(defn generator [g]
  (fn next-val [x]
    (lazy-seq (cons x (next-val (g x))))))

;
; In calculus, the derivative of function f is defined as
;     limit (f(x+h) - f(x)) / h
;     h-> 0
; The expression inside the limit is called the Newton Quotient.  This Clojure
; function returns the Newton Quotient for a given function f.
;
(defn newton-quotient [f]
  (fn [h x]
    (/ (- (f (+ x h)) (f x)) h)))

;
; limit takes a function df that represents the Newton Quotient defined above
; and produces a function that computes the limit as h -> 0.  It also takes an
; option parameter, epsilon, that will be used as the convergence criteria.
; If epsilon is omitted, it takes the value of 1.0e-6 (i.e. 1/1000000).
;
; The implementation first produces a sequence of numbers that tends to zero.
; For simplicity, I use (1, 0.1, 0.01, 0.001, 0.0001 ...).  The values in
; this sequence are successively used as values of h to compute the Newton
; Quotient, thus simulating the limit process.  The process stops (i.e. we
; found a limit) when the absolute difference between successive calculations
; of the Newton Quotient is within epsilon.
;
; TODO:
; Upon further testing, it doesn't always work and it depends on the function
; in concern.  The problem seems to be the inexact nature of floating point
; arithmetic.  When h is super small (e.g. 1.0e-12), f(x+h)-f(x) returns 0 even
; though it's not zero but because there's not enough precision to deal with
; small floating point values.  This results in derivative being 0 and the
; Newton's method formula x-f(x)/f'(x) throwing a nasty divide by 0 exception.
; I tried fixing it with big decimal but that doesn't quite work either.  Need
; further investigation.
;
#_(defn limit
  ([df] (limit df 1.0e-6))
  ([df epsilon]
   (fn [x]
     (let [tends-to-zero (generator #(/ % 10.0))]
       (reduce
         (fn [prev curr]
           (if (<= (Math/abs (- curr prev)) epsilon)
             (reduced curr) curr))
         (map #(df % x) (tends-to-zero 1)))))))

;
; derivative returns the derivative function of f
;
; TODO:
; See item above.  Right now, just use a small enough h and avoid simulating
; the limit process.
;
#_(defn derivative [f]
  (limit (newton-quotient f)))
(defn derivative [f]
  (partial (newton-quotient f) 1.0e-6))

;
; newton-method uses an initial guess and produces a sequence of successively
; better approximations as solutions to the equation f(x)=0 using the Newton's
; Method.  In short, if you have an approximate solution, x0, for f(x) = 0,
; Newton's Method tells you that a better approximation is given by
; x1 = x0 - f(x0)/f'(x0) where f'(x0) is the derivative of f evaluated at x0.
; Consequently, you can compute better and better solutions by applying
; the same formula to each successive approximation.
;
(defn newton-method [f guess]
  (let [df (derivative f)
        g (fn [x] (- x (/ (f x) (df x))))]
    ((generator g) guess)))

;
; solve-by-newton takes a function f, an initial guess, an optional epsilon
; and solves the equation f(x)=0 using Newton's Method.  As explained above,
; Newton's Method is an algorithm that produces better and better approximate
; solutions to the equation.  The parameter epsilon serves as the exit criteria
; to the algorithm.  When a solution x satisfies abs(f(x)) <= epsilon, it
; returns x as the solution.  If epsilon is omitted, it takes the default value
; of 1.0e-6 (i.e. 1/1000000).
;
; Example:
;   Say we want to solve x^2 = 2.  We first rewrite it as x^2 - 2 = 0 and let
;   f(x) = x^2 - 2.  In Clojure, this function is written as #(- (* % %) 2)
;   or (fn [x] (- (* x x) 2)).  And we set our initial guess to be 1.
;
;   (solve-by-newton #(- (* % %) 2) 1)
;   ;; => 1.4142156871995384
;
(defn solve-by-newton
  ([f guess] (solve-by-newton f guess 1.0e-6))
  ([f guess epsilon]
   (first (filter #(<= (Math/abs (f %)) epsilon)
                  (newton-method f guess)))))
