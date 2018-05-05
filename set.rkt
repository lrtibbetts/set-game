#lang racket ; using the standard Racket language

;; CPSC 326 Final Project: the game of SET
;; Lucy Tibbetts
;; 5-4-2018

(require racket/draw)
(require images/flomap)
; flomap doc: https://docs.racket-lang.org/images/flomap_title.html

; card struct
; specify color, shape, num, and shading
(define-struct card (color shape num shading))

; deck of all 81 cards
(define my-deck
  ; for*/list generates a new list with all possible combinations
  ; of the items in the provided lists
  (for*/list ([color (in-list '("red" "purple" "green"))]
              [shape (in-list '("oval" "squiggle" "diamond"))]
              [num (in-list '("1" "2" "3"))]
              [shading (in-list '("solid" "striped" "outline"))])
    (card color shape num shading)))

; shuffled deck
(define shuffled-deck (shuffle my-deck))

; get a single card and scale it
; png images from: https://github.com/bluerider/set-game-multi/tree/master/set-cards
(define (get-card card)
  (let ([img (string-append (card-color card)
                            (card-shape card)
                            (card-num card)
                            (card-shading card))])
    ; convert bitmap to flomap in order to scale the image
    (flomap-scale (bitmap->flomap
                   (make-object bitmap%
                     (string-append "pngs\\" img ".png"))) 1/5)))

; append 3 cards horizontally to make a row
(define (row a b c)
  (flomap-ht-append (get-card a)
                    (get-card b)
                    (get-card c)))

; append rows vertically
; the number of rows to append depends on the size of the deck
(define (rows deck)
  (cond [(>= (length deck) 12) (flomap-vl-append
                                (row (list-ref deck 0)
                                     (list-ref deck 1)
                                     (list-ref deck 2))
                                (row (list-ref deck 3)
                                     (list-ref deck 4)
                                     (list-ref deck 5))
                                (row (list-ref deck 6)
                                     (list-ref deck 7)
                                     (list-ref deck 8))
                                (row (list-ref deck 9)
                                     (list-ref deck 10)
                                     (list-ref deck 11)))]
        [(= (length deck) 9) (flomap-vl-append
                              (row (list-ref deck 0)
                                   (list-ref deck 1)
                                   (list-ref deck 2))
                              (row (list-ref deck 3)
                                   (list-ref deck 4)
                                   (list-ref deck 5))
                              (row (list-ref deck 6)
                                   (list-ref deck 7)
                                   (list-ref deck 8)))]
        [(= (length deck) 6) (flomap-vl-append
                              (row (list-ref deck 0)
                                   (list-ref deck 1)
                                   (list-ref deck 2))
                              (row (list-ref deck 3)
                                   (list-ref deck 4)
                                   (list-ref deck 5)))]
        ; if there are only 3 cards left, no need to append
        [(= (length deck) 3) (row (list-ref deck 0)
                                  (list-ref deck 1)
                                  (list-ref deck 2))]))

; helper fxns for set?:

; check that 3 values are equal
(define (my-equal? a b c)
  (and (equal? a b)
       (equal? a c)))
 
; check that 3 values are not equal
(define (not-equal? a b c)
  (and (not (equal? a b))
       (not (equal? a c))
       (not (equal? b c))))

; determine if 3 cards make a set
; for each individual feature (color, shape, num, shading), 
; check that the 3 values are all the same or all different
(define (set? a b c)
  (and (or (my-equal? (card-color a)
                      (card-color b)
                      (card-color c))
           (not-equal? (card-color a)
                       (card-color b)
                       (card-color c)))
       (or (my-equal? (card-shape a)
                      (card-shape b)
                      (card-shape c))
           (not-equal? (card-shape a)
                       (card-shape b)
                       (card-shape c)))
       (or (my-equal? (card-num a)
                      (card-num b)
                      (card-num c))
           (not-equal? (card-num a)
                       (card-num b)
                       (card-num c)))
       (or (my-equal? (card-shading a)
                      (card-shading b)
                      (card-shading c))
           (not-equal? (card-shading a)
                       (card-shading b)
                       (card-shading c)))))
  
; convert user input into cards and determine whether or not they are a set
(define (check-input a b c deck)
  (set? (list-ref deck (- a 1)) ; subtract 1 to adjust for zero indexing 
        (list-ref deck (- b 1))
        (list-ref deck (- c 1))))

; remove a set from the deck after it's found
(define (remove-set deck a b c)
  ; use let* for multiple local bindings
  (let* ([first (list-ref deck (- a 1))]
         [second (list-ref deck (- b 1))]
         [third (list-ref deck (- c 1))])
    (remove* (list first second third) deck)))

; print a total of 15 cards (3 extra)
; function called if the user can't find any sets in the initial 12 cards
(define (print-extra deck)
  (println (flomap->bitmap (rows deck)))
  (println (flomap->bitmap (row (list-ref my-deck 12)
                                (list-ref my-deck 13)
                                (list-ref my-deck 14)))))

; play a game of SET
; my-bool: determines whether or not to continue playing
(define (play-game my-bool my-deck)
  (when (equal? my-bool #t)
    ; convert rows to bitmaps to print to the screen
    (println (flomap->bitmap (rows my-deck)))
    (displayln "Do you see any sets? (Y/N)")
    ; add more cards if the user can't find any sets
    ; only allow three extra cards to be added (odds of not finding a set out of 15 cards = 2500:1)
    (define str (read-line))
    (when (equal? str "N")
      (displayln "Adding three more cards (up to a total of 15)...")
      (print-extra my-deck))
    (displayln "Indicate a set by entering the positions of the 3 cards one at a time (e.g. 1 = top-left)")
    (define a (string->number (read-line)))
    (define b (string->number (read-line)))
    (define c (string->number (read-line)))
    ; check if the specified cards make a set
    ; let the user know whether or not they found a set
    (if (check-input a b c my-deck)
        (displayln "You found a set!")
        (displayln "That is not a valid set"))
    (displayln "Keep playing? (Y/N)")
    (define new-str (read-line))
    (if (and (equal? new-str "Y")
             (>= (length my-deck) 6))
        ; call play-game recursively with my-bool set to either #t or #f
        ; only remove valid sets from the deck
        (if (check-input a b c my-deck)
            (play-game #t (remove-set my-deck a b c))
            (play-game #t my-deck))
        (play-game #f my-deck))))

; start game
(displayln "Welcome to the game of SET!")
; utilize the shuffled deck of 81 cards
(play-game #t shuffled-deck)
(displayln "Thanks for playing!")