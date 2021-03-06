#!/usr/bin/env chibi-scheme

;; Solution for Nines and Zeros

;; Given an integer n, find the smallest number consisting only of the
;; digits zero and nine that is divisible by n.

;; I am using a brute-force algorithm to start with

;; Example: 23 --> 990909

(import (scheme base)
        (scheme write)
        (srfi 1))

(define (number->digits n)
  (string->list (number->string n)))

(define (only-9-0? n)
  (null? (delete #\0 (delete #\9 (number->digits n)))))

(define (find-9-0-multiple n)
  (let loop ((i 1))
    (if (only-9-0? (* i n))
        (* i n)
        (loop (+ i 1)))))

(display (find-9-0-multiple 23))
(newline)

