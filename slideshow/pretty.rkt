#lang racket/base

(require slideshow
         slideshow/play
         slideshow/code
         slideshow/latex
         slideshow/pict
         (for-syntax syntax/stx))

(provide (all-defined-out))

; Library
(define (medium-text txt)
  (text txt (current-main-font) 50))

(define (medium-large-text txt)
  (text txt (current-main-font) 65))

(define (large-text txt)
  (text txt (current-main-font) 80))

(define (massive-text txt)
  (text txt (current-main-font) 120))

(define (double-massive-text txt)
  (text txt (current-main-font) 240))

(define (uber-massive-text txt)
  (text txt (current-main-font) 600))

(define (medium-$$ txt)
  (scale/improve-new-text ($$ txt) 1.5))

(define (large-$$ txt)
  (scale/improve-new-text ($$ txt) 2))

(define (massive-$$ txt)
  (scale/improve-new-text ($$ txt) 3))

(define (double-massive-$$ txt)
  (scale/improve-new-text ($$ txt) 7))

;(define (medium-$$ txt)
;  (medium-text txt))
;
;(define (large-$$ txt)
;  (large-text txt))
;
;(define (massive-$$ txt)
;  (massive-text txt))
;
;(define (double-massive-$$ txt)
;  (double-massive-text txt))
;
(define medium-size 1.5)
(define large-size 2)
(define massive-size 3)
(define double-massive-size 7)

(define medium-$$-size 1.5)
(define large-$$-size 2)
(define massive-$$-size 3)
(define double-massive-$$-size 7)

(define (scode #:append [append 'left] . str)
  (define (proper-append v1 v2)
    ((match append
      ['left vl-append]
      ['center vc-append])
     v1 v2))
  (define-values (picture row)
    (for/fold ([picture (blank)]
             [row (blank)])
            ([s str])
    (match s
      ["\n" (values (proper-append picture row) (blank))]
      [s* #:when (string? s*) (values picture (hc-append row (tt s*)))]
      [else (values picture (hc-append row s))])))
  (proper-append picture row))
  ;(define str* (apply string-append str))
  ;(apply (match append
  ;         ['left vl-append]
  ;         ['center vc-append])
  ;       `(0 ,@(map (λ (n) (tt n))
  ;                  (string-split str* "\n")))))

(define (dot str #:small [small #t] #:pretty [pretty #t])
  (if small
      (bitmap (string-append str "-small.png"))
      (if pretty
          (bitmap (string-append str "-notugly.png"))
          (bitmap (string-append str ".png")))))

(define (title-slide . data)
  (play-n
   #:skip-last? #t
   (animate-slide
    'next
    'alts
    `(,data ()))))

(define (pretty-slide #:title [title ""] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   #:title title
   (animate-slide
    'next
    'alts
    `(,data ()))))

(define (rotate-slide #:title [title ""]
                      #:rotate-in [rotate-in #t] #:rotate-out [rotate-out #t]
                      #:distance [distance 0] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   #:title title
   (λ (n1 n2)
      (rotate
       (apply vc-append `(,distance ,@data))
       (* 3 (+ (- 1 n1) (- n2)))))))

(define (pretty->rotate-slide #:title [title ""]
                              #:fade-in [fade-in #t] #:rotate-out [rotate-out #t]
                              #:distance [distance 0] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   #:title title
   (λ (n1 n2)
      (fade-pict
       (if fade-in n1 1)
       (t "")
       (rotate
        (apply vc-append `(,distance ,@data))
        (if rotate-out (* 3 (- n2)) 0))))))

(define (rotate->pretty-slide #:title [title ""]
                              #:rotate-in [rotate-in #t] #:fade-out [fade-out #t]
                              #:distance [distance 0] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   #:title title
   (λ (n1 n2)
      (fade-pict
       (if fade-out n2 0)
       (rotate
        (apply vc-append `(,distance ,@data))
        (if rotate-in (* 3 (- 1 n1)) 0))
       (t "")))))

(define (flip-slide #:title [title ""]
                    #:flip-in [flip-in #t] #:flip-out [flip-out #t]
                    #:distance [distance 0] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   #:title title
   (λ (n1 n2)
      (scale
       (apply vc-append `(,distance ,@data))
       (max 0.001 (* n1 (- 1 n2))) 1))))

(define (pretty->flip-slide #:title [title ""]
                            #:fade-in [fade-in #t] #:flip-out [flip-out #t]
                            #:distance [distance 0] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   #:title title
   (λ (n1 n2)
      (fade-pict
       (if fade-in n1 1)
       (t "")
       (scale
        (apply vc-append `(,distance ,@data))
        (max 0.001 (- 1 n2)) 1)))))

(define (flip->pretty-slide #:title [title ""]
                            #:flip-in [flip-in #t] #:fade-out [fade-out #t]
                            #:distance [distance 0] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   #:title title
   (λ (n1 n2)
      (fade-pict
       (if fade-out n2 0)
       (scale
        (apply vc-append `(,distance ,@data))
        (max 0.001 n1) 1)
       (t "")))))

(define (start-pretty-slide #:title [title ""] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #f
   #:title title
   (apply animate-slide `(next ,@data))))

(define (end-pretty-slide #:title [title ""] . data)
  (play-n
   #:skip-first? #f
   #:skip-last? #t
   #:title title
   (animate-slide
    'alts
    `(,data ()))))

(define (shrink-transition-slide #:title [title ""]
                                 #:start-size [start-size 1] #:end-size [end-size 1]
                                 #:distance [distance 0] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   (λ (n1)
      (scale (apply vc-append `(,distance ,@data))
             (max (+ (* start-size (- 1 n1)) (* end-size n1)) 0.001)))))


(define (header-slide #:title [title ""] #:reversed [reversed #f]
                      #:append [append 'top] #:distance [distance 0]
                      #:fade-in [fade-in #t] #:fade-out [fade-out #t]
                      #:left [left ""] #:right [right ""]
                      #:header [header ""] . data)
  (play-n
   #:title title
   #:skip-first? #t
   #:skip-last? #t
   (λ (n1 n2 n3)
     (fade-pict
      (if fade-out n3 0)
      (fade-pict
          (if fade-in n1 1)
          (t "")
          (fade-around-pict
           (if reversed
               (- 1 n2)
               n2)
           header (λ (x)
                       (match append
                         ['top      (apply vc-append `(,distance ,x ,@data))]
                         ['top-l    (apply vl-append `(,distance ,x ,@data))]
                         ['bottom   (apply vc-append `(,distance ,@data ,x))]
                         ['bottom-l (apply vl-append `(,distance ,@data ,x))]
                         ['left     (apply hc-append `(,distance ,x ,@data))]
                         ['right    (apply hc-append `(,distance ,@data ,x))]
                         ['center-h (apply hc-append `(,distance ,left ,x ,right))]
                         ['center-v (apply vc-append `(,distance ,left ,x ,right))]
                         [else      (apply vc-append `(,distance ,x ,@data))]))))
      (t "")))))

(define (acronym-slide #:title [title ""]
                       #:fade-in [fade-in #t] #:fade-out [fade-out #t]
                       #:acronym [acronym ""] . data)
  (let ([height (if (pair? data) (pict-height (car data)) 0)])
    (play-n
     #:title title
     #:skip-first? #t
     #:skip-last? #t
     (λ (n1 n2 n3)
       (fade-pict
        (if fade-out n3 0)
        (fade-pict
         (if fade-in n1 1)
         (t "")
         (fade-pict
          n2
          acronym
          (apply vc-append `(,(- (* height n2) height) ,@data))))
        (t ""))))))

(define (transition-acronym-slide #:title [title ""]
                                  #:reversed [reversed #f]
                                  #:acronym [acronym ""] . data)
  (let ([height (if (pair? data) (pict-height (car data)) 0)])
    (play-n
     #:title title
     #:skip-first? #t
     #:skip-last? #t
     (λ (n1)
        (let [(fade (if reversed (- 1 n1) n1))]
          (fade-pict
           fade
           acronym
           (apply vc-append `(,(- (* height fade) height) ,@data))))))))

(define (insert-slide #:title [title ""] #:reversed [reversed #f]
                      #:left [left (t "")] #:right [right (t "")]
                      #:fade-in [fade-in #t] #:fade-out [fade-out #t]
                      #:append [append 'center-h] #:distance [distance 0]
                      #:insert [insert ""] . data)
  (play-n
   #:title title
   #:skip-first? #t
   #:skip-last? #t
   (λ (n1 n2 n3)
      (fade-pict
       (if fade-out n3 0)
       (fade-pict
        (if fade-in n1 1)
        (t "")
        ((λ (x)
            (match append
              ['top      (apply vc-append `(,distance ,x ,@data))]
              ['bottom   (apply vc-append `(,distance ,@data ,x))]
              ['left     (apply hc-append `(,distance ,x ,@data))]
              ['right    (apply hc-append `(,distance ,@data ,x))]
              ['center-h (apply hc-append `(,distance ,left ,x ,right))]
              ['center-v (apply vc-append `(,distance ,left ,x ,right))]
              [else      (apply vc-append `(,distance ,x ,@data))]))
         (if reversed
             (match append
               ['center-v (scale insert 1 (max 0.001 (- 1 n2)))]
               [else (scale insert (max 0.001 (- 1 n2)) 1)])
             (match append
               ['center-v (scale insert 1 (max 0.001 n2))]
               [else (scale insert (max 0.001 n2) 1)]))))
       (t "")))))

(define (transition-insert-slide #:title [title ""] #:reversed [reversed #f]
                                 #:left [left (t "")] #:right [right (t "")]
                                 #:append [append 'center-h] #:distance [distance 0]
                                 #:insert [insert ""] . data)
  (play-n
   #:title title
   #:skip-first? #t
   #:skip-last? #t
   (λ (n1)
        ((λ (x)
            (match append
              ['top      (apply vc-append `(,distance ,x ,@data))]
              ['bottom   (apply vc-append `(,distance ,@data ,x))]
              ['left     (apply hc-append `(,distance ,x ,@data))]
              ['right    (apply hc-append `(,distance ,@data ,x))]
              ['center-h (apply hc-append `(,distance ,left ,x ,right))]
              ['center-v (apply vc-append `(,distance ,left ,x ,right))]
              [else      (apply vc-append `(,distance ,x ,@data))]))
         (if reversed
             (match append
               ['center-v (scale insert 1 (max 0.001 (- 1 n1)))]
               [else (scale insert (max 0.001 (- 1 n1)) 1)])
             (match append
               ['center-v (scale insert 1 (max 0.001 n1))]
               [else (scale insert (max 0.001 n1) 1)]))))))

(define (transition-slide #:title [title ""] #:reversed [reversed #f]
                      #:append [append 'top] #:distance [distance 0]
                      #:left [left ""] #:right [right ""]
                      #:header [header ""] . data)
  (play-n
   #:title title
   #:skip-first? #t
   #:skip-last? #t
   (λ (n)
      (fade-around-pict
       (if reversed
           (- 1 n)
           n)
       header (λ (x)
                 (match append
                   ['top      (apply vc-append `(,distance ,x ,@data))]
                   ['top-l    (apply vl-append `(,distance ,x ,@data))]
                   ['bottom   (apply vc-append `(,distance ,@data ,x))]
                   ['bottom-l (apply vl-append `(,distance ,@data ,x))]
                   ['left     (apply hc-append `(,distance ,x ,@data))]
                   ['right    (apply hc-append `(,distance ,@data ,x))]
                   ['center-h (apply hc-append `(,distance ,left ,x ,right))]
                   ['center-v (apply vc-append `(,distance ,left ,x ,right))]
                   [else      (apply vc-append `(,distance ,x ,@data))]))))))

(define (X-slide #:title [title ""] #:distance [distance 0] . data)
  (play-n
   ;#:title title
   #:skip-first? #t
   (λ (n1)
      (cc-superimpose
       (apply vc-append `(,distance ,@data))
       (cellophane (colorize (uber-massive-text "X") "red") n1)))))

(define-syntax (picture-slide stx)
  (syntax-case stx ()
    [(k #:title title #:fade-in fade-in #:fade-out fade-out first-pic pic ...)
     ; =>
     #'(picture-slide* title fade-in fade-out first-pic pic ...)]

    [(k #:title title first-pic pic ...)
     ; =>
     #'(picture-slide* title #t #t first-pic pic ...)]

    [(k #:fade-in fade-in #:fade-out fade-out first-pic pic ...)
     ; =>
     #'(picture-slide* "" fade-in fade-out first-pic pic ...)]

    [(k first-pic pic ...)
     ; =>
     #'(picture-slide* "" #t #t first-pic pic ...)]))

(define-syntax (picture-slide* stx)
  (define (build-transitions pic id acc)
    (cond [(stx-null? pic) acc]
          [(stx-null? (stx-cdr pic))
           ; =>
           #`(fade-pict #,(stx-car id) #,acc
                        (scale #,(stx-car pic) #,(stx-car id)))]

          [else
           ; =>
           (build-transitions (stx-cdr pic) (stx-cdr id)
                              #`(fade-pict #,(stx-car id) #,acc
                                           (scale #,(stx-car pic) (+ #,(stx-car id) #,(stx-car (stx-cdr id))))))]))
  (syntax-case stx ()
    [(k title fade-in fade-out first-pic pic ...)
     ; =>
     (with-syntax ([(first-id) (generate-temporaries #'(first-pic))]
                   [(id ...) (generate-temporaries #'(pic ...))]
                   [(last-id) (generate-temporaries #'(1))])
       (with-syntax ([body (build-transitions #'(pic ...) #'(id ...)
                                              #`(cellophane (scale first-pic (+ 1 #,(stx-car #'(id ...))))
                                                            (if fade-in first-id 1)))])
         #'(play-n
            #:skip-first? #t
            #:skip-last? #t
            #:title title
            (λ (first-id id ... last-id)
              (cellophane body (if fade-out (- 1 last-id) 1))))))]))

(define-syntax (section stx)
  (syntax-case stx ()
    [(k #:title section-title slides ...)
     ; =>
     (with-syntax ([section-slide (datum->syntax #'k 'section-slide)]
                   [pretty-slide (datum->syntax #'k 'pretty-slide)]
                   [flip-slide (datum->syntax #'k 'flip-slide)]
                   [pretty->flip-slide (datum->syntax #'k 'pretty->flip-slide)]
                   [flip->pretty-slide (datum->syntax #'k 'flip->pretty-slide)]
                   [start-pretty-slide (datum->syntax #'k 'start-pretty-slide)]
                   [end-pretty-slide (datum->syntax #'k 'end-pretty-slide)]
                   [header-slide (datum->syntax #'k 'header-slide)]
                   [acronym-slide (datum->syntax #'k 'acronym-slide)]
                   [transition-acronym-slide (datum->syntax #'k 'transition-acronym-slide)]
                   [insert-slide (datum->syntax #'k 'insert-slide)]
                   [transition-insert-slide (datum->syntax #'k 'transition-insert-slide)]
                   [transition-slide (datum->syntax #'k 'transition-slide)]
                   [picture-slide (datum->syntax #'k 'picture-slide)])
       #'(let ([pretty-slide* pretty-slide]
               [flip-slide* flip-slide]
               [pretty->flip-slide* pretty->flip-slide]
               [flip->pretty-slide* flip->pretty-slide]
               [start-pretty-slide* start-pretty-slide]
               [end-pretty-slide* end-pretty-slide]
               [header-slide* header-slide]
               [acronym-slide* acronym-slide]
               [transition-acronym-slide* transition-acronym-slide]
               [insert-slide* insert-slide]
               [transition-insert-slide* transition-insert-slide]
               [transition-slide* transition-slide])

           (define (section-slide #:title [title #f] . data)
             (unless title (set! title section-title))
             (apply slide data #:title title))

           (define (pretty-slide #:title [title #f] . data)
             (unless title (set! title section-title))
             (apply pretty-slide* data #:title title))

           (define (flip-slide #:title [title #f]
                               #:flip-in [flip-in #t] #:flip-out [flip-out #t]
                               #:distance [distance 0] . data)
             (unless title (set! title section-title))
             (apply flip-slide* data #:title title #:flip-in flip-in #:flip-out flip-out
                    #:distance distance))

           (define (pretty->flip-slide #:title [title #f]
                                       #:fade-in [fade-in #t] #:flip-out [flip-out #t]
                                       #:distance [distance 0] . data)
             (unless title (set! title section-title))
             (apply pretty->flip-slide* data #:title title
                    #:fade-in fade-in #:flip-out flip-out
                    #:distance distance))

           (define (flip->pretty-slide #:title [title #f]
                                       #:flip-in [flip-in #t] #:fade-out [fade-out #t]
                                       #:distance [distance 0] . data)
             (unless title (set! title section-title))
             (apply flip->pretty-slide* data #:title title
                    #:flip-in flip-in #:fade-out fade-out
                    #:distance distance))

           (define (start-pretty-slide #:title [title #f] . data)
             (unless title (set! title section-title))
             (apply start-pretty-slide* data #:title title))

           (define (end-pretty-slide #:title [title #f] . data)
             (unless title (set! title section-title))
             (apply end-pretty-slide* data #:title title))

           (define (header-slide #:title [title #f] #:reversed [reversed #f]
                                 #:append [append 'top] #:distance [distance 0]
                                 #:fade-in [fade-in #t] #:fade-out [fade-out #t]
                                 #:left [left ""] #:right [right ""]
                                 #:header [header ""] . data)
             (unless title (set! title section-title))
             (apply header-slide* data #:title title #:reversed reversed #:append append
                    #:distance distance #:fade-in fade-in
                    #:fade-out fade-out #:left left #:right right
                    #:header header))

           (define (acronym-slide #:title [title #f]
                                  #:fade-in [fade-in #t] #:fade-out [fade-out #t]
                                  #:acronym [acronym ""] . data)
             (unless title (set! title section-title))
             (apply acronym-slide* data #:title title #:fade-in fade-in #:fade-out fade-out
                    #:acronym acronym))


           (define (transition-acronym-slide #:title [title #f]
                                             #:reversed [reversed #f]
                                             #:acronym [acronym ""] . data)
             (unless title (set! title section-title))
             (apply transition-acronym-slide* data #:title title #:reversed reversed
                    #:acronym acronym))

           (define (insert-slide #:title [title #f] #:reversed [reversed #f]
                                 #:left [left ""] #:right [right ""]
                                 #:fade-in [fade-in #t] #:fade-out [fade-out #t]
                                 #:append [append 'center-h] #:distance [distance 0]
                                 #:insert [insert ""] . data)
             (unless title (set! title section-title))
             (apply insert-slide* data #:title title #:reversed reversed
                    #:left left #:right right
                    #:fade-in fade-in #:fade-out fade-out
                    #:append append #:distance distance
                    #:insert insert))

           (define (transition-insert-slide #:title [title #f] #:reversed [reversed #f]
                                 #:left [left (t "")] #:right [right (t "")]
                                 #:append [append 'center-h] #:distance [distance 0]
                                 #:insert [insert ""] . data)
             (unless title (set! title section-title))
             (apply transition-insert-slide* data #:title title #:reversed reversed
                    #:left left #:right right #:append append #:distance distance
                    #:insert insert))

           (define (transition-slide #:title [title #f] #:reversed [reversed #f]
                                     #:append [append 'top] #:distance [distance 0]
                                     #:left [left ""] #:right [right ""]
                                     #:header [header ""] . data)
             (unless title (set! title section-title))
             (apply transition-slide* data #:title title
                    #:reversed reversed #:append append
                    #:left left #:right right
                    #:distance distance #:header header))

           (define-syntax (picture-slide stx)
             (syntax-case stx ()
               [(k #:title title #:fade-in fade-in #:fade-out fade-out first-pic pic (... ...))
                ; =>
                #'(picture-slide* title fade-in fade-out first-pic pic (... ...))]

               [(k #:title title first-pic pic (... ...))
                ; =>
                #'(picture-slide* title #t #t first-pic pic (... ...))]

               [(k #:fade-in fade-in #:fade-out fade-out first-pic pic (... ...))
                ; =>
                #'(picture-slide* section-title fade-in fade-out first-pic pic (... ...))]

               [(k first-pic pic (... ...))
                ; =>
                #'(picture-slide* section-title #t #t first-pic pic (... ...))]))

           slides ...))]))
