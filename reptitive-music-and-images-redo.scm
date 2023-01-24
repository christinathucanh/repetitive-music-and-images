; Project: Repetitive music and images
; This project uses list transformations to perform some interesting Cartesian repetitive decomposition and musical composition.
; Author: Anh Thuc (Christina) Vu
; Date: 2022-10-3 
; Acknowledgements: ...

(import image) 
(define ehrenstein
 (lambda (length n box-color circ-color outline-color)
 (let* (
    [x (/ length 2)]
    [y (pair x length)]
    [z (pair x 0)]
    [constant (/ x n)]
    [box-solid (square length "solid" box-color)]
    [outer-circle (circle x "solid" circ-color)]
    [outline (map (lambda (n) (circle (* (+ n 1) constant) "outline" outline-color))(range n))]
    [diamond (path length length (list z y y z z) "outline" outline-color)]) 
    (overlay diamond (apply overlay outline) outer-circle box-solid) 
 ))) 
; length: number?  side length of the square and circle
; n: number? number of circles that can be returned 
; box-color: color? the color of the box 
; circ-color: color? the color of the circles 
; outline-color: color? the color of the outline color
; Return the image of circles stacking up on the box, with a diamond on it. 
 (ehrenstein 200 5 "red" "yellow" "black") 
; return a single Ehrenstein illusion of length 200, 5 circles, a "red" box, "yellow" circles, and "black" outline.
 (define ehrenstein-2
 (ehrenstein 100 10 "aqua" "orange" "black")) 
ehrenstein-2 
; return a single Ehrenstein illusion of length 100, 10 circles, an "aqua" box, "orange" circles, and "black" outline.
 (define ehrenstein-3
 (ehrenstein 50 0 "white" "white" "green")) 
ehrenstein-3
; return  a single Ehrenstein illusion of length 50, no circles, a "white" box and circle, and "green" outline.
 (define grid
(lambda (m n img)
(apply beside (make-list n (apply above (make-list m img)))))) 
; m: number? number of rows
; n: number? number of columns 
; img: drawing? the image of the picture
; Return an image that is a grid of m rows and n columns of the provided image img. 

(define ehrenstein-4
(grid 3 3 (ehrenstein 100 10 "red" "yellow" "orange")))
ehrenstein-4 
; return a 3×3 grid of Ehrenstein illusions of length 100, 10 circles each, and a "red" box, "yellow" circle, and "orange" outline.
(define ehrenstein-5
(grid 3 2 (ehrenstein 50 5 "blue" "green" "white")))
ehrenstein-5 
; return a 3×2 grid of Ehrenstein illusions of length 50, 5 circles each, and a "blue" box, "green" circles, and "white" outline.
; PART 2 
(import music)
(define interval->note

  (lambda (n int)

    (cond

      [(or (equal? int "P1") (equal? int "d2")) n]

      [(or (equal? int "m2") (equal? int "A1")) (+ 1 n)]

      

      [(or(equal? int "M2") (equal? int "d3")) (+ 2 n)]

      

      [(or(equal? int "m3") (equal? int "A2")) (+ 3 n)]

      

      [(or(equal? int "M3") (equal? int "d4")) (+ 4 n)]

      

      [(or(equal? int "P4") (equal? int "A3")) (+ 5 n)]

      

      [(or(equal? int "d5") (equal? int "A4")) (+ 6 n)]

      

      [(or(equal? int "P5") (equal? int "d6")) (+ 7 n)]

      

      [(or(equal? int "A5") (equal? int "A5")) (+ 8 n)]

      

      [(or (equal? int "M6") (equal? int "d7")) (+ 9 n)]

     

      [(or(equal? int "m7") (equal? int "A6")) (+ 10 n)]

      

      [(or (equal? int "M7") (equal? int "d8")) (+ 11 n)] 

      [(or (equal? int "P8") (equal? int "A7")) (+ 12 n)] )))

; n: root of the notes  
; int: number? the MIDI numbers 


(define triad

  (lambda (root i1 i2 i3 dur) 
      (par

      (note (interval->note root i1) dur)

      (note (interval->note root i2) dur)

      (note (interval->note root i3) dur)))) 



(define chord 
(lambda (root int dur) 
 (apply (lambda (x y z) (triad root x y z dur)) int))) 
 (chord 60 (list "P1" "m3" "P5") qn) 

(define degree->offset 
 (lambda (degree)
   (cond 
    [(equal? (string-upcase degree) "I") 0] 
    [(equal? (string-upcase degree) "II") 2] 
    [(equal? (string-upcase degree) "III") 4]
    [(equal? (string-upcase degree) "IV") 5]
    [(equal? (string-upcase degree) "V") 7]
    [(equal? (string-upcase degree) "VI") 9]
    [(equal? (string-upcase degree) "VII") 11]
    [(equal? (string-upcase degree) "VIII") 12] 
   ))) 



(define progression 
(lambda (key chords durs)
(cond 
[(equal? (list-ref (map char-upper-case? (apply append (map string->list chords )))
0) #t )
(apply seq (map (lambda (x z) (chord x (list "P1" "M3" "P5" ) z)) (map (lambda (x) (+ x key)) (map degree->offset chords)) durs ))] 
[else 
(apply seq (map (lambda (x z) (chord x (list "P1" "m3" "P5") z )) (map (lambda (x) (+ x key)) (map degree->offset chords)) durs ))]))) 

(progression 60 (list "I" "V" "vi" "IV")(list hn hn hn hn)) 

(define I-V-vi-IV (progression 60 (list "I" "V" "vi" "IV")(list hn hn hn hn)) ) 

I-V-vi-IV 

(define I-IV-V-IV (progression 64 (list "I" "V" "vi" "IV")(list hn hn hn hn))) 

I-IV-V-IV 

(define ii-V-I (progression 60 (list "ii" "V" "I")(list hn hn hn))) 
ii-V-I  

