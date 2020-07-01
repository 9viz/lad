#lang racket/base

(define (l+ . y)
  (define (helper a b c d)
    (cond
      ((zero? b) (+ (* a d) c))
      ((zero? a) (+ (* b d) c))
      (else (helper
             (quotient a 10)
             (quotient b 10)
             (+ c (* d
                     (max (remainder a 10)
                          (remainder b 10))))
             (* 10 d)))))
  (foldl (λ (s t)
           (helper s t 0 1))
         (car y)
         (cdr y)))

(define (l* . y)
  (define (helper a b c d)
    (cond
      ((zero? b) (+ (* a d) c))
      ((zero? a) (+ (* b d) c))
      (else (helper
             (quotient a 10)
             (quotient b 10)
             (+ c (* d
                     (min (remainder a 10)
                          (remainder b 10))))
             (* d 10)))))
  (foldl (λ (s t)
           (helper s t 0 1))
         (car y)
         (cdr y)))
