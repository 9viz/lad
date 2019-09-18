#lang racket

(provide (all-defined-out))

(define (lmult/append-zeros num-digits lst)
  (append lst (make-list num-digits 0)))

(define (lmult/mult-single ns n)
  (map (λ (d) [min d n]) ns))
