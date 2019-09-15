#lang racket

(provide (all-defined-out))

(define (list->num lst)
  (foldl
   (lambda (val dig) [+ (* dig 10) val])
   0
   lst))

(define (between? n a b)
  (and [>= n a]
       [<= n b]))

(define extract-digits
  (lambda (num [digits '()])
    (cond
      [(= num 0) (reverse digits)]
      [else
       (extract-digits (quotient num 10)
                       (append digits [list (modulo num 10)]))])))
