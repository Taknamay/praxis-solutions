#!/usr/bin/env chibi-scheme

(import (scheme base)
        (srfi 28)
        (srfi 113)
        (srfi 114))

;; This is a program I wrote in Python a while ago, ported to Scheme

(define (power-set comp s)
  ;; This is a procedure to return the power-set of s
  (if (set-empty? s)
      (set set-comparator)
      (let loop ((list-in (set->list s))
                 (set-out (set set-comparator (set comp))))
        (if (null? list-in)
            set-out
            (loop (cdr list-in)
                  (set-union set-out
                             (set-map (lambda (s)
                                        (set-adjoin s (car list-in)))
                                      set-comparator
                                      set-out)))))))

(define s (set integer-comparator 1 2 3))

(define pow-s (power-set integer-comparator s))

(display (map set->list (set->list pow-s)))
(newline)

