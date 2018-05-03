#lang racket
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

; 4 rows
(define (rows deck)
  (flomap-vl-append
   (row (list-ref deck 0) (list-ref deck 1) (list-ref deck 2))
   (row (list-ref deck 3) (list-ref deck 4) (list-ref deck 5))
   (row (list-ref deck 6) (list-ref deck 7) (list-ref deck 8))
   (row (list-ref deck 9) (list-ref deck 10) (list-ref deck 11))))

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
; for each individual feature (color, shape, num, shading), 
; check that the 3 values are all the same or all different
(define (set? a b c)
  (if (and (or (my-equal? (card-color a) (card-color b) (card-color c))
               (not-equal? (card-color a) (card-color b) (card-color c)))
           (or (my-equal? (card-shape a) (card-shape b) (card-shape c))
               (not-equal? (card-shape a) (card-shape b) (card-shape c)))
           (or (my-equal? (card-num a) (card-num b) (card-num c))
               (not-equal? (card-num a) (card-num b) (card-num c)))
           (or (my-equal? (card-shading a) (card-shading b) (card-shading c))
               (not-equal? (card-shading a) (card-shading b) (card-shading c))))
         (displayln "You found a set!") (displayln "That is not a valid set")))

; convert user input into cards and determine whether or not they are a set
(define (check-input first-card second-card third-card deck)
  (let* ([a (string->number first-card)] 
         [b (string->number second-card)] ; ignore spaces
         [c (string->number third-card)])
    (set? (list-ref deck (- a 1)) ; subtract 1 to adjust for zero indexing 
          (list-ref deck (- b 1))
          (list-ref deck (- c 1)))))

; remove a SET from the deck after it's found
(define (remove-set deck a b c)
  (let* ([first-card (list-ref deck a)]
         [second-card (list-ref deck b)]
         [third-card (list-ref deck c)])
    (remove* (list first-card second-card third-card) deck)))

; play a game of SET
(define (play-game my-bool my-deck)
  (when (equal? my-bool #t)
    (println (flomap->bitmap (rows my-deck)))
    (displayln "Indicate a set by entering the positions of the 3 cards one at a time (e.g. 1 = top-left)")
    (define a (read-line))
    (define b (read-line))
    (define c (read-line))
    (check-input a b c my-deck)
    (displayln "Keep playing? (Y/N)")
    (define str (read-line))
    (if (and (equal? str "Y") (> (length my-deck) 6)) ; ensure that there will still be 3 cards
        (play-game #t (remove-set
                       my-deck
                       (- (string->number a) 1)
                       (- (string->number b) 1)
                       (- (string->number c) 1)))
        (play-game #f my-deck))))

; start game
(displayln "Welcome to the game of SET!")
(play-game #t shuffled-deck)
(displayln "Thanks for playing!")

; fxn to see if there are any more sets left
; fxn to add more cards if there are no sets
