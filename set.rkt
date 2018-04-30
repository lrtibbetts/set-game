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

; print a single card
(define (print card)
  (let ([img (string-append (card-color card) (card-shape card)
                            (card-num card) (card-shading card))])
    (flomap->bitmap (flomap-scale (bitmap->flomap (make-object bitmap% (string-append "pngs\\" img ".png"))) 1/5))))

; print a row of 3 cards
(define (print-row a b c)
    (flomap->bitmap (flomap-ht-append (bitmap->flomap (print a))
                                      (bitmap->flomap (print b))
                                      (bitmap->flomap (print c)))))

; print 12 cards (4 rows of 3)
(define (print-4-rows deck)
  (flomap->bitmap (flomap-vl-append (bitmap->flomap (print-row (list-ref deck 0) (list-ref deck 1) (list-ref deck 2)))
                                    (bitmap->flomap (print-row (list-ref deck 3) (list-ref deck 4) (list-ref deck 5)))
                                    (bitmap->flomap (print-row (list-ref deck 6) (list-ref deck 7) (list-ref deck 8)))
                                    (bitmap->flomap (print-row (list-ref deck 9) (list-ref deck 10) (list-ref deck 11))))))

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
         (write "You found a set!") (write "That is not a valid set")))

; testing cards
(define card1 (card "red" "oval" "1" "solid"))
(define card2 (card "green" "squiggle" "3" "striped"))
(define card3 (card "purple" "diamond" "2" "outline"))

; allow the user to select three cards
; let the user know whether or not they found a set