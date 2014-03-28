#lang racket/base
(require slideshow
         slideshow/play)

(provide pretty-slide)

(define (pretty-slide #:title [title ""] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   #:title title
   (animate-slide
    'next
    'alts
    `(,data ()))))
