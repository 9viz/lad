;; lad - lunar (dismal) arithmetic repl thingy

#lang racket

(define (prepend-digits num-digits prep-digit lst)
  (append (make-list num-digits prep-digit) lst))

(define (handle-nums num1 num2 prep-digit)
  (let ([lst1 (extract-digits num1)]
        [lst2 (extract-digits num2)])
    (let ([diff (- (length lst1) (length lst2))])
      (cond
        [(positive? diff) (values lst1 (prepend-digits diff prep-digit lst2))]
        [else (values (prepend-digits (- diff) prep-digit lst1) lst2)]))))

(define (list->num lst)
  (foldl
   (lambda (val dig)
     (+ (* dig 10) val))
   0
   lst))

(define extract-digits
  (lambda (num [digits '()])
    (cond
      [(= num 0) (reverse digits)]
      [else
       (extract-digits (quotient num 10)
                       (append digits [list (modulo num 10)]))])))

(define (ladd n1 n2)
  (let-values ([(d1 d2) (handle-nums n1 n2 0)])
    (list->num
    (map (lambda (num1 num2) [max num1 num2])
         d1 d2))))

(define (lmult n1 n2)
  '())
