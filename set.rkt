#lang racket ; using the standard Racket lang.

;; CPSC 326 Final Project: the game of SET
;; Lucy Tibbetts

(require racket/draw)
(require images/flomap)
; flomap doc: https://docs.racket-lang.org/images/flomap_title.html

; card struct
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
(define (get-card card)
  (let ([img (string-append (card-color card)
                            (card-shape card)
                            (card-num card)
                            (card-shading card))])
    (flomap-scale (bitmap->flomap
                   (make-object bitmap%
                     (string-append "pngs\\" img ".png"))) 1/5)))

; append 3 cards to make a row
(define (row a b c)
  (flomap-ht-append (get-card a)
                    (get-card b)
                    (get-card c)))

; append rows depending on the size of the deck
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
  (if (and (or (my-equal? (card-color a)
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
                           (card-shading c))))
         (displayln "You found a set!")
         (displayln "That is not a valid set")))

; convert user input into cards and determine whether or not they are a set
(define (check-input a b c deck)
  (set? (list-ref deck (- a 1)) ; subtract 1 to adjust for zero indexing 
        (list-ref deck (- b 1))
        (list-ref deck (- c 1))))

; remove a SET from the deck after it's found
(define (remove-set deck a b c)
  (let* ([first (list-ref deck (- a 1))]
         [second (list-ref deck (- b 1))]
         [third (list-ref deck (- c 1))])
    (remove* (list first second third) deck)))

; play a game of SET
(define (play-game my-bool my-deck)
  (when (equal? my-bool #t)
    (println (flomap->bitmap (rows my-deck)))
    (displayln "Do you see any sets? (Y/N)")
    ; add more cards if there are no sets
    (define str (read-line))
    (when (equal? str "N")
      (displayln "3 more cards have been added")
      (println (flomap->bitmap (rows my-deck)))
      (println (flomap->bitmap (row (list-ref my-deck 12)
                                    (list-ref my-deck 13)
                                    (list-ref my-deck 14)))))
    (displayln "Indicate a set by entering the positions of the 3 cards one at a time (e.g. 1 = top-left)")
    (define a (string->number (read-line)))
    (define b (string->number (read-line)))
    (define c (string->number (read-line)))
    (check-input a b c my-deck)
    ; need to only remove valid sets!!
    (displayln "Keep playing? (Y/N)")
    (define new-str (read-line))
    (if (and (equal? new-str "Y")
             (>= (length my-deck) 6))
        ; ensure that there will still be 3 cards after 3 cards are removed
        (play-game #t (remove-set my-deck a b c))
        (play-game #f my-deck))))

; start game
(displayln "Welcome to the game of SET!")
(play-game #t shuffled-deck)
(displayln "Thanks for playing!")