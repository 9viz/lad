;; lad - lunar (dismal) arithmetic repl
;; licensed under bsd 2 clause simplified license

#lang racket

(require "lib/parse.rkt")

(let loop ()
  (display "> ")
  (printf "~a\n"
          (leval (open-input-string
                  (read-line (current-input-port) 'any))))
  (loop))
