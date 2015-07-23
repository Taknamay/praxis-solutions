
;; A Number Puzzle

;; Find a 10-digit number with:
;; - All digits unique
;; - First n digits are divisible by n

;; Example: 345
;; 3   is divisible by 1
;; 34  is divisible by 2
;; 345 is divisible by 3

;; Some notes before I get started, based on well-known divisibility rules:

;; (Indexed by 1)
;; The tenth digit is 0.
;; The fifth digit is 5.
;; Even-indexed digits are even.
;; Odd-indexed digits are odd.

;; With this information, we will only need 4! * 4! trials.

(import (scheme base)
        (scheme write)
        (srfi 1))

(define (check? digits)
  (let loop ((i 1)
             (n (car digits))
             (in-list (cdr digits)))
    (if (= 0 (modulo n i))
        (if (null? in-list)
            #t
            (loop (+ i 1)
                  (+ (* n 10) (car in-list))
                  (cdr in-list)))
        #f)))

(define (puzzle)
  (define (all-permutations l)
    ;; Only works if l has no repeated elements
    (if (null? l)
        '(())
        (let loop ((out-list '())
                   (in-list l))
          (if (null? in-list)
              out-list
              (loop (append (map (lambda (x) (cons (car in-list) x))
                                 (all-permutations (delete (car in-list) l)))
                            out-list)
                    (cdr in-list))))))
  (define odd-perms (all-permutations '(1 3 7 9)))
  (define even-perms (all-permutations '(2 4 6 8)))
  (define (combine-digits odds evens)
    (list (first odds)
          (first evens)
          (second odds)
          (second evens)
          5
          (third evens)
          (third odds)
          (fourth evens)
          (fourth odds)))
  (let loop ((i 0)
             (next-odds odd-perms)
             (next-evens even-perms))
    (define result (combine-digits (car next-odds) (car next-evens)))
    (if (check? result)
        ;; Return the result as a number
        (let loop ((n 0)
                   (in-list (append result '(0))))
          (if (null? in-list)
              n
              (loop (+ (* n 10) (car in-list))
                    (cdr in-list))))
        (if (< i 23)
            (loop (+ i 1) next-odds (cdr next-evens))
            (loop 0 (cdr next-odds) even-perms)))))

(display (puzzle))
(newline)
