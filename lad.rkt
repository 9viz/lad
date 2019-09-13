;; lad - lunar (dismal) arithmetic repl thingy

#lang racket

(define extract-digits
  (lambda (num [digits '()])
  (cond
    [(= num 0) (reverse digits)]
    [else
     (set! digits (append digits
                          [list (modulo num 10)]))
     (extract-digits (quotient num 10)
                     digits)])))
