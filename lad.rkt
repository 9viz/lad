#lang racket/base

(provide l+ l*)

(define (lad/*helper* a b c d condition)
  (cond
    ((zero? b) (+ (* a d) c))
    ((zero? a) (+ (* b d) c))
    (else (lad/*helper*
           (quotient a 10)
           (quotient b 10)
           (+ c (* d
                   (condition (remainder a 10)
                              (remainder b 10))))
           (* 10 d)
           condition))))

(define (l+ . n)
  (foldl (λ (s t)
           (lad/*helper* s t 0 1 max))
         (car n) (cdr n)))

(define (l* . n)
  (foldl (λ (s t)
           (lad/*helper* s t 0 1 min))
         (car n) (cdr n)))
