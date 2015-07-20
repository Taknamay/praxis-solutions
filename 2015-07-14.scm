#!/usr/bin/env chibi-scheme

;; In the dictionary, find the longest ordered word
;; An ordered word is one which has its letter in alphabetical order

(import (scheme base)
        (scheme char)
        (scheme file)
        (scheme write)
        (srfi 95)) ;; For 'sorted?'

(define dictionary
  (open-input-file "/usr/share/dict/words"))

(define (next-word)
  ;; Words with punctuation will be automatically rejected
  (let ((next (read-line dictionary)))
    (if (eof-object? next)
        (begin
          (close-input-port dictionary)
          next)
        (string-downcase next))))

(define-record-type type-word-list
  (word-list size contents)
  word-list?
  (size wl-size)
  (contents wl-contents))

(define (update-wl wl word)
  (if (and (>= (string-length word) (wl-size wl))
           (sorted? (string->list word) char<?))
      (word-list (string-length word)
                 (if (> (string-length word) (wl-size wl))
                     (list word)
                     (cons word (wl-contents wl))))
      wl))

(define (make-null-wl)
  (word-list 0 '("")))

(define longest-sorted
  (let loop ((wl (make-null-wl))
             (next (next-word)))
    (if (eof-object? next)
        wl
        (loop (update-wl wl next)
              (next-word)))))

(display (wl-contents longest-sorted))
(newline)


