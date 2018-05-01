#lang racket/gui
(require racket/draw)
(require images/flomap) ; https://docs.racket-lang.org/images/flomap_title.html

; card struct
(define-struct card (color shape num shading))

; deck of all 81 cards
(define my-deck
  (for*/list ([color (in-list '("red" "purple" "green"))]
              [shape (in-list '("oval" "squiggle" "diamond"))]
              [num (in-list '("1" "2" "3"))]
              [shading (in-list '("solid" "striped" "outline"))])
    (card color shape num shading)))

; shuffled deck
(define shuffled-deck (shuffle my-deck))

; get a single card and scale it
(define (get-card card)
  (let ([img (string-append (card-color card) (card-shape card)
                            (card-num card) (card-shading card))])
    (flomap-scale (bitmap->flomap
                   (make-object bitmap%
                     (string-append "pngs\\" img ".png"))) 1/5)))

; append 3 cards to make a row
(define (row a b c)
  (flomap-ht-append (get-card a) (get-card b) (get-card c)))

; print 12 cards (4 rows of 3)
(define (print-4-rows deck)
  (flomap->bitmap
   (flomap-vl-append
    (row (list-ref deck 0) (list-ref deck 1) (list-ref deck 2))
    (row (list-ref deck 3) (list-ref deck 4) (list-ref deck 5))
    (row (list-ref deck 6) (list-ref deck 7) (list-ref deck 8))
    (row (list-ref deck 9) (list-ref deck 10) (list-ref deck 11)))))

; helper fxns for set?:
; check that 3 values are equal
(define (my-equal? a b c)
  (and (equal? a b) (equal? a c)))
 
; check that 3 values are not equal
(define (not-equal? a b c)
  (and (not (equal? a b))
       (not (equal? a c))
       (not (equal? b c))))

; determine if 3 cards make a set
; for each individual feature (color, shape, num, shading), check that the 3 values are all the same or all different
(define (set? a b c)
  (if (and (or (my-equal? (card-color a) (card-color b) (card-color c))
               (not-equal? (card-color a) (card-color b) (card-color c)))
           (or (my-equal? (card-shape a) (card-shape b) (card-shape c))
               (not-equal? (card-shape a) (card-shape b) (card-shape c)))
           (or (my-equal? (card-num a) (card-num b) (card-num c))
               (not-equal? (card-num a) (card-num b) (card-num c)))
           (or (my-equal? (card-shading a) (card-shading b) (card-shading c))
               (not-equal? (card-shading a) (card-shading b) (card-shading c))))
         (display "You found a set!") (display "That is not a valid set")))

; convert user input into cards and determine whether or not they are a set
(define (check-input str deck)
  (let* ([a (char->integer (string-ref str 0))] 
         [b (char->integer (string-ref str 2))] ; ignore spaces
         [c (char->integer (string-ref str 4))])
    (set? (list-ref deck (- a 49))
          (list-ref deck (- b 49))
          (list-ref deck (- c 49))))) ; adjust for zero indexing and ascii

(define (print-check card)
  (let ([img (string-append (card-color card) (card-shape card)
                            (card-num card) (card-shading card))])
    (flomap->bitmap (flomap-scale
                     (bitmap->flomap
                      (make-object bitmap%
                        (string-append "pngs\\" img ".png"))) 1/5))))

; play the game of SET:
(displayln "Welcome to the game of SET!")
(print-4-rows shuffled-deck)
(displayln "Indicate a set by entering the positions of the 3 cards (e.g. 1 = top-left)")
(define input (read-line))
(check-input input shuffled-deck)
