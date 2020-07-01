#lang racket/base

(define (l+ . y)
  (define (helper a b c a0 b0)
    (cond
      ((zero? b) (+ (* a a0) c))
      ((zero? a) (+ (* b b0) c))
      (else (helper
             (quotient a 10)
             (quotient b 10)
             (+ c (* b0
                     (max (remainder a 10)
                          (remainder b 10))))
             (* 10 a0)
             (* b0 10)))))
  (foldl (λ (s t)
           (helper s t 0 1 1))
         (car y)
         (cdr y)))

(define (l* . y)
  (define (helper a b c a0 b0)
    (cond
      ((zero? b) (+ (* a a0) c))
      ((zero? a) (+ (* b b0) c))
      (else (helper
             (quotient a 10)
             (quotient b 10)
             (+ c (* b0
                     (min (remainder a 10)
                          (remainder b 10))))
             (* 10 a0)
             (* b0 10)))))
  (foldl (λ (s t)
           (helper s t 0 1 1))
         (car y)
         (cdr y)))
