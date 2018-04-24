#lang racket
(require 2htdp/image)

; card struct
; color: "red" "purple" "green"
; shapes: "oval" "squiggle" "diamond"
; nums: "1" "2" "3"
; shading: "solid" "striped" "outline"
(define-struct card (color shape num shading))

; example card
(define my-card (card "red" "oval" "1" "solid"))

; print out a card
(define (print card)
  (let ([img (string-append (card-color card) (card-shape card)
                            (card-num card) (card-shading card))])
    (scale 0.2 (bitmap/file (string-append "pngs\\" img ".png")))))

; deck of all 80 cards
; resource utilized to understand for*/list:
; https://beautifulracket.com/explainer/loops.html
(define my-deck
  (for*/list ([color (in-list '("red" "purple" "green"))]
              [shape (in-list '("oval" "squiggle" "diamond"))]
              [num (in-list '("1" "2" "3"))]
              [shading (in-list '("solid" "striped" "outline"))])
    (card color shape num shading)))

; function to print out all 80 cards
(define (print-deck deck)
  (map print (shuffle deck)))

; todo: function to play a game of set
; deal (print) 15 random cards at a time, keeping track of which cards have been displayed
; allow the user to select three cards
; check whether or not the three cards make a set
; if they do, remove the cards and tell the user
; if not, let the user know
; end the game after a certain amount of time or when there are no more cards


