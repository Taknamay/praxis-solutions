
;; Random total

;; Generate k random positive integers that add up to a total n

(import (scheme base)
        (scheme write)
        (srfi 27))

(define (rand-to-sum k n)
  (if (or (<= k 0)
          (<= n 0)
          (> k n))
      (error "rand-to-sum" "Bad parameters"))
  (let loop ((i 1)
             (total 0)
             (out-list '()))
    (if (< i k)
        (let ((next-num (+ 1 (random-integer (- (- n total) (- k i))))))
          (loop (+ i 1)
                (+ total next-num)
                (cons next-num out-list)))
        (cons (- n total) out-list))))

(define (disp-info l)
  (display l)
  (newline)
  (display (length l))
  (newline)
  (display (apply + l))
  (newline))

(disp-info (rand-to-sum 5 1000))
