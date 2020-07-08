;
; Stuff related to math
;

(ns my-playground.math)

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
;   (permutation-gen s) = ([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])
;   (permutation-gen s 2) = ([1 2] [1 3] [2 1] [2 3] [3 1] [3 2])
;
; Implementation Notes:
;   The idea is to generate all permutations recursively.  Suppose you have
;   a set s = #{x1 x2 x3 ... xm}.  First, take x1 out, and generate all
;   permutations of length (n-1) for #{x2 x3 ... xm}.  You then attach x1 to
;   the beginning of each length (n-1) permutations to obtain all length n
;   permutations beginning with x1.  Repeat same procedure for x2, x3 ... xm.
;
(defn permutation-gen
  ([s] (permutation-gen s (count s)))
  ([s n] (cond
           (or (neg? n) (> n (count s))) (throw (IllegalArgumentException. "Invalid length n"))
           (or (zero? n) (empty? s)) [[]]
           :else (mapcat (fn [[x remaining]]
                           (conj-elem-to-colls x (permutation-gen remaining (dec n))))
                         (split s)))))

;
; Produces a sequence of n-combinations of elements in s.  Each combination 
; is itself a set.
;
; Example:
;   s = #{1 2 3 4}
;   (combination-gen s 2) = (#{1 2} #{1 3} #{1 4} #{2 3} #{2 4} #{3 4})
;
; Implementation Notes:
;   Again, we use recursion.  Combinations of length n for set s = #{x1 x2 ...
;   xm} is given by all combinations with x1 present union with all combinations
;   without x1.  So, the algorithm proceeds with removing x1 from s, generating
;   all length (n-1) combinations for set #{x2 x3 ... xm} and add x1 to each.
;   Finally, union this first set of results with all combinations of length n
;   for set #{x2 x3 ... xm}.
;
(defn combination-gen [s n]
  (cond
    (or (neg? n) (> n (count s))) (throw (IllegalArgumentException. "Invalid length n"))
    (zero? n) [#{}]
    (= n (count s)) [s]
    :else (let [x (first s)
                remaining (disj s x)]
            (concat (conj-elem-to-colls x (combination-gen remaining (dec n)))
                    (combination-gen remaining n)))))

