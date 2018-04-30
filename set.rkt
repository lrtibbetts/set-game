#lang racket/gui
(require racket/draw)
(require images/flomap) ; https://docs.racket-lang.org/images/flomap_title.html

; card struct
; color: "red" "purple" "green"
; shapes: "oval" "squiggle" "diamond"
; nums: "1" "2" "3"
; shading: "solid" "striped" "outline"
(define-struct card (color shape num shading))
(define my-card (card "red" "oval" "1" "solid"))

; deck of all 80 cards
; https://beautifulracket.com/explainer/loops.html
(define my-deck
  (for*/list ([color (in-list '("red" "purple" "green"))]
              [shape (in-list '("oval" "squiggle" "diamond"))]
              [num (in-list '("1" "2" "3"))]
              [shading (in-list '("solid" "striped" "outline"))])
    (card color shape num shading)))

; print a single card
(define (print card)
  (let ([img (string-append (card-color card) (card-shape card)
                            (card-num card) (card-shading card))])
    (flomap->bitmap (flomap-scale (bitmap->flomap (make-object bitmap% (string-append "pngs\\" img ".png"))) 1/5))))

; print out all 80 cards
(define (print-deck deck)
  (map print (shuffle deck)))

; print a row of 3 cards
(define (print-row card1 card2 card3)
    (flomap->bitmap (flomap-ht-append (bitmap->flomap (print card1))
                                      (bitmap->flomap (print card2))
                                      (bitmap->flomap (print card2)))))
  
; todo: print 12 cards at a time
; allow the user to select three cards
; check whether or not the three cards make a set
; if they do, remove the cards and tell the user
; if not, let the user know
; end the game after a certain amount of time or when there are no more cards