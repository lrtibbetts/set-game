#lang racket
(require 2htdp/image)

; needed functions:
; set? is the given set valid?

; card struct
(define-struct card (color shape num shading))

; test card
(define test-card (card "red" "oval" "1" "solid"))

; define possible values for color, shape, num, and shading
; todo: use contracts
(define colors (list "red" "purple" "green"))
(define shapes (list "oval" "squiggle" "diamond"))
(define nums (list "1" "2" "3"))
(define shadings (list "solid" "striped" "outlined"))

; print out a card
(define (print card)
  (let ([img (string-append (card-color card) (card-shape card) (card-num card) (card-shading card))])
    (scale 0.25 (bitmap/file (string-append "pngs\\" img ".png")))))

; todo: function to print fifteen cards at a time