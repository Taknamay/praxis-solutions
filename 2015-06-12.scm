
;; Random total

;; Generate k random positive integers that add up to a total n

(import (scheme base)
        (scheme write)
        (srfi 27))

(define (shuffle in-vect)
  (define sz (vector-length in-vect))
  (define vect (vector-copy in-vect))
  (let loop ((i 0))
    (when (< i sz)
          (let* ((v (vector-ref vect i))
                 (i-nx (random-integer sz))
                 (v-nx (vector-ref vect i-nx)))
            (vector-set! vect i v-nx)
            (vector-set! vect i-nx v))
          (loop (+ i 1))))
  vect)

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

(disp-info
 (vector->list
  (shuffle
   (list->vector
    (rand-to-sum 20 1000)))))
