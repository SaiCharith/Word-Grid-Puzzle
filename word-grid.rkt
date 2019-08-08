#lang racket/gui
(provide (all-defined-out))
(require "words10000.txt")   ;imported dictionary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Definitions
(define word-list words-list) ;list of words in dictionary
(define words '()) ;words used in grid

(define Du-words '())  ;diagonal-up words-list
(define Dd-words '())  ;diagonal-down words-list
(define H-words '())   ;horizontal words-list
(define V-words '())   ;vertical words-list

(define h 0) ;no.of horizontal words
(define v 0) ;no.of vertical words                   
(define dd 0);no.of diagonal-up words
(define du 0);no.of diagonal-down words
(define nw (+ h v dd du)) ;total no. of words

(define word-details '()) ;vector of words present in the grid.
(define domain '())  ;list of random numbers to randomize given set of words.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2d-vector definitions

(define (make-2d-vector r c)
  (build-vector r 
                (lambda (x) (make-vector c #f))))
(define (2d-vector-ref vec r c)
  (vector-ref
   (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buttons and text-boxes 

(define frame (new frame%
                   [label "word-grid"]
                   [min-width 1300]
                   [min-height 800]
                   [stretchable-width #f]
                   [stretchable-height #f]))

(define rowpanel (new vertical-panel%
                      [parent frame]
                      [min-width 30]
                      [min-height 10]
                      [stretchable-width #f]
                      [stretchable-height #f]
                      [alignment '(center top)]
                      [horiz-margin 50]
                      [spacing 20]))

(define row1 (new horizontal-panel% 
                  [parent rowpanel]
                  [min-width 30]
                  [min-height 10]
                  [alignment '(left top)]))
(define row2 (new horizontal-panel% 
                  [parent rowpanel]
                  [min-width 30]
                  [min-height 10]
                  [alignment '(right top)]))

;; First type of game, where computer itself generates grid.

(define show-grid (new button%
                       [label "computer-genenarated-grid"]
                       [parent row1]
                       [min-width 50]
                       [min-height 10]
                       [vert-margin 5]
                       [horiz-margin 200]
                       [callback (lambda (button event)
                                   (set! select 1)
                                   (set! word-list words-list)
                                   (refresh)
                                   (play-grid 'solve-a-grid))]))

;; Second type, where user inputs a set of words to be hidden in grid.

(define my-grid (new button%
                     [label "make-own-grid"]
                     [parent row1]
                     [min-width 50]
                     [min-height 10]
                     [vert-margin 5]
                     [horiz-margin 200]
                     [callback (lambda (button event)
                                 (set! select 1)
                                 (refresh)
                                 (play-grid (send txt get-value)))]))

;; Difficulty level of game. (Change of size of grid and no. of words)

(define textt (new text-field%
                   [label "Level(1-8)"]
                   [parent row2]
                   [stretchable-width #f]
                   [stretchable-height #f]
                   [init-value "8"]
                   [horiz-margin 200]
                   [vert-margin 5]
                   [min-height 20]
                   [min-width 40]
                   ))

(define txt (new text-field%
                 [label "Enter words separated by commas"]
                 [parent row2]
                 [stretchable-width #f]
                 [stretchable-height #f]
                 [init-value ""]
                 [horiz-margin 200]
                 [vert-margin 5]
                 [min-height 20]
                 [min-width 300]
                 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define difficulty 8)                     ;1 to 8 levels
(define p (send textt get-value))         ;Getting difficulty level from canvas.
(set! difficulty (string->number p))      ;
(define n (* 2 (+ difficulty 2)))         ;grid size (nxn)
(define null-vector (make-2d-vector n n)) ; for refreshing grid
(define grid (make-2d-vector n n))        ;for storing letters in grid
(define word-check (make-2d-vector n n))  ;Starting and ending positions of a word stored in vector.
; it is easy to check whether player selects a correct word or not in grid.
(define r 0)                              ;no.of loops for particular combination
(define maxnoofloops 600)                 ;max val of r
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; improvised 2d-vector definitions according to the orientation of the word.
  
(define (gcord r c sym)
  (cond [(equal? sym 'H) (cons c r)]
        [(equal? sym 'V) (cons r c)]
        [(equal? sym 'D-d) (cons r c)]
        [(equal? sym 'D-u) (cons (- n 1 c) r)]))

(define (gvector-ref vec r c sym)
  (cond [(equal? sym 'H) (2d-vector-ref vec c r)]
        [(equal? sym 'V) (2d-vector-ref vec r c)]
        [(equal? sym 'D-d) (2d-vector-ref vec r c)]
        [(equal? sym 'D-u) (2d-vector-ref vec (- n 1 c) r)]))

(define (gvector-set! vec r c val sym)
  (cond[(equal? sym 'V) (2d-vector-set! vec r c val)]
       [(equal? sym 'H) (2d-vector-set! vec c r val)]
       [(equal? sym 'D-d) (2d-vector-set! vec r c val)]
       [(equal? sym 'D-u) (2d-vector-set! vec (- n 1 c) r val)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GRID-GENERATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "grid-set" takes in a "wlist" and "sym",and places the words in the grid
;;  according to the orientation, that sym represents.

;; ABSTRACTION: Here "sym" is added to the arguements,so that the same
;; function can be used to place words in any orientation.
;; Eg : setting "sym" as V' makes the function to place all the words in "wlist"
;; in Vertical orientation.
  
(define (grid-set wlist sym)
  (define check-t (make-2d-vector n n)) ;;a small thing introduced to ensure words do not overlap.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; "helper" function takes a word and places in the grid.
   ;; MEMOISATION: Here "memo" is added to store the positions
   ;; of the word,that have already been checked to place in the grid.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                  
  (define (helper grid word Sno memo max count)
    (set! r (+ 1 r))
    (if (or (> r maxnoofloops) (= max count))
        (re-run)
        (let* [(l (length word))
               (x (random (- n l -1)))
               (y (if (or (equal? sym 'D-u)
                          (equal? sym 'D-d))
                      (random (- n l -1)) (random n)))]
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (define (satisfy)  ;;checks if the current position can be occupied by the word
     
            (define (iter word i)
              (let((var (if (or (equal? sym 'D-d)
                                (equal? sym 'D-u)) i 0)))
                (if (< i l)
                    (let ((p (gvector-ref grid (+ x i) (+ var y) sym)))
                      (if (not p) (iter (cdr word) (+ i 1))
                          (if (equal? (car word) p)
                              (if (not (gvector-ref check-t (+ x i) (+ var y) sym))
                                  (iter (cdr word) (+ i 1)) #f)
                              #f))) #t)))
            (iter word 0))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
          (define (place-word) ;;places the word in position
            (define i 0)
            (define (iter word)
              (let((var (if (or (equal? sym 'D-u)
                                (equal? sym 'D-d)) i 0)))
                (cond[(< i l) (begin(cond [(or (= i 0) (= i (- l 1)))
                                           (let ((a (gvector-ref word-check (+ x i) (+ y var) sym)))
                                             (cond [(list? a)
                                                    (gvector-set! word-check (+ x i)
                                                                  (+ y var) (cons Sno a) sym)]
                                                   [(number? a)
                                                    (gvector-set! word-check (+ x i)
                                                                  (+ y var) (list Sno a) sym)]
                                                   [#t (gvector-set! word-check (+ x i)
                                                                     (+ y var) Sno sym)]))])
                                    (gvector-set! check-t (+ x i) (+ y var) #t sym)
                                    (gvector-set! grid (+ x i) (+ y var)
                                                  (car word) sym)
                                    (set! i (+ i 1))
                                    (iter (cdr word)))])))
            (let*[
                  (var (if (or (equal? sym 'D-d)
                               (equal? sym 'D-u)) (- l 1) 0))
                  (P1 (gcord x y sym))
                  (P2 (gcord (+ x l -1) (+ var y) sym))
                  ]
              (begin
                (2d-vector-set! word-details Sno 1 P1)                
                (2d-vector-set! memo x y #t)
                (set! count (+ 1 count))
                (2d-vector-set! word-details Sno 2 P2)
                (iter word))))
          (if (and (not (2d-vector-ref memo x y)) (satisfy))
              (place-word) (begin (2d-vector-set! memo x y #t)
                                  (helper grid word Sno memo max count))))))
;;;foldr being used as an iterator.
  (foldr (lambda (t p)
           (cond [(not (null? domain)) 
                  (let*[(a (car domain))
                        (l (length t))
                        (x (- n l -1))
                        (y (if (or (equal? sym 'D-u)
                                   (equal? sym 'D-d))
                               (- n l -1) n))
      
                        (memo (make-2d-vector x y))
                        ]
                    (begin (set! domain (cdr domain))
                           (2d-vector-set! word-details a 0 (list->string t))
                           (helper grid t a memo (- (* x y) 1) 0)))]))
         'nothing wlist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define (word-selector)
  (let* [(a (car words))
         (l (length a))
         ]
    (if (or (> l n) (>= 2 l)) (begin (set! words (cdr words))
                                     (word-selector))
        (begin (set! words (cdr words)) a))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grid is reset,whenever the program recognises a break-down,or long loops.
;; reseting the grid better ensures its completion,than let-going the long-lasting loops.

(define (re-run)
  (set! r 0)
  (set! words (shuffle word-list))
  (set! grid (make-2d-vector n n))
  (set! word-check (make-2d-vector n n))
  (set! words (shuffle word-list))
  (set! Du-words '())
  (set! Dd-words '())
  (set! H-words '())
  (set! V-words '())
  (set! word-details (make-2d-vector (+ h v du dd) 3))
  (set! domain (shuffle (range (+ h v du dd))))
  ;;
  ;; Here "foldr" is slightly unconventionally being used as a "for" loop.
  ;;
  (foldr (lambda (t p) (let((a (word-selector)))
                         (set! H-words (cons a H-words))))
         'nothing (range h))
  (foldr (lambda (t p) (let((a (word-selector)))
                         (set! V-words (cons a V-words))))
         'nothing (range v))
  (foldr (lambda (t p) (let((a (word-selector)))
                         (set! Du-words (cons a Du-words))))
         'nothing (range du))
  (foldr (lambda (t p) (let((a (word-selector)))
                         (set! Dd-words (cons a Dd-words))))
         'nothing (range dd))
  (grid-set Du-words 'D-u)
  (grid-set Dd-words 'D-d)  
  (grid-set H-words 'H)
  (grid-set V-words 'V))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Filling the gaps in the grid with random letters.
(define (final-grid grid)
  (foldr (lambda (t p) ;Foldr is being used as an iterator.
           (foldr (lambda (w x)
                    (let ((a (2d-vector-ref grid t w)))
                      (cond [(not a)
                             (2d-vector-set! grid t w
                                             (make-string 1 (integer->char
                                                             (+ 65 (random 26)))))]
                            [#t (2d-vector-set! grid t w
                                                (make-string 1 (integer->char
                                                                (- (char->integer a) 32)
                                                                )))]
                            )))
                  'nothing (range n))) 'nothing (range n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Canvas definitions

(struct bx (x1 y1 x2 y2) #:transparent) ;end coordinates of a word.
(define list-of-actions '())            ;all permanent drawing actions
(define grid-x 150)                     ;starting position of 1st letter.
(define grid-y 50)                      
(define grid1-x 140)                    ;starting position of grid.
(define grid1-y 45)
(define word-x 900)                     ;starting position of displayed words
(define word-y 50)                                    ; (not in grid)
(define box-width 30)                   ;distance b/w adjacent letters in grid.
(define word-len 170)                   ;assumed max. length of word (to display 
(define word-height 30)                 ;   side-by-side) , (not in grid).
(define letter-width 10)              ;used for striking off discovered words from grid.
(define radius 20)                    ;radius of circle used to highlight letters in grid.
(define delta 14)                     ;used for length of box for striking off.
(define delta1 3)                     ;used as a correction in highliting letter selected.
(define click-state 0)                ;click state of a left click
(define first-click (cons 0 0))       ;Noting coordinates of odd left click
(define select 0)             ;For showing canvas only after one of the 2 modes of game are clicked.
(define cheat "")                     ;Notes type of cheat used.
(define size (- n 1))                 ; Size of grid, starting from 0.
(define nwords (quotient nw 2))       ; no.of words displayed in a column (not in grid).

;;;;;;;;;;;;;;;; Pens and drawing brushes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define black-pen (make-object pen% "black" 1 'solid))
(define blue-pen (make-object pen% "blue" 1 'solid))
(define yellow-pen (make-object pen% "yellow" 1 'solid))
(define yellow-brush (make-object brush% "Yellow" 'solid))
(define green-brush (make-object brush% "Green" 'solid))
(define red-brush (make-object brush% "Red" 'solid))
(define red-pen (make-object pen% "red" 2 'solid))
(define green-pen (make-object pen% "green" 1 'solid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A higher order function for drawing purposes.
(define (iterator i condn f change)
  (cond[(condn i) (f i)
                  (iterator (change i) condn f change)]))

;; Displays letters in the grid on canvas.
(define (disp-grid)
  (define len (- (vector-length grid) 1))
  (define (condn i) (>= i 0))
  (define (change i) (- i 1))
  (define (f i)
    (define (f1 j)
      (send bm-dc draw-text (2d-vector-ref grid j i)
            (+ grid-x (* box-width i))
            (+ grid-y (* box-width j))))                          
    (iterator len condn f1 change))
  (iterator len condn f change))

;;Displaying box around letters in canvas.
(define (disp-box)
  (define (condn i) (>= i 0))
  (define (change t) (- t 1)) 
  (define (f i)
    (send bm-dc set-pen black-pen)
    (send bm-dc set-brush red-brush)
    (send bm-dc draw-line (+ grid1-x (* 30 i)) grid1-y (+ grid1-x (* 30 i)) (+ (* n 30) grid1-y))
    (send bm-dc draw-line grid1-x (+ grid1-y (* 30 i)) (+ (* n 30) grid1-x) (+ grid1-y (* 30 i))))
  (iterator  n condn f change))

;;Gets common number from l1 and l2.
;;l1 and l2 can be numbers, #f, list of numbers (2 coordinates of intersected words).
(define (get-common l1 l2)
  (cond[(not l1) #f]
       [(not l2) #f]
       [(number? l1) (if(equal? l1 l2) l1
                        (if(list? l2)
                           (if(equal? (length l2) (length (remove l1 l2))) #f l1) #f))]
       [(number? l2) (if(equal? (length l1) (length (remove l2 l1))) #f l2)]
       [(null? l1) #f]
       [#t (if (get-common (car l1) l2) (car l1)
               (get-common (cdr l1) l2))]))

;;Checks whether the selected bx (struct) is a hidden word or not and strikes off if it is.
(define (present? b)
  (let* ([b1 (2d-vector-ref word-check (bx-y1 b) (bx-x1 b))]
         [b2 (2d-vector-ref word-check (bx-y2 b) (bx-x2 b))]
         [check-same (not (equal? (cons (bx-x1 b) (bx-y1 b)) (cons (bx-x2 b) (bx-y2 b))))]
         [common (get-common b1 b2)])
    (cond[ (and check-same common)
           (let* ([qi (quotient common nwords)]
                  [rem (remainder common nwords)]
                  [len (string-length (2d-vector-ref word-details common 0))]
                  [x1 (- (+ word-x (* word-len qi)) delta)]
                  [y1  (+ word-y (* word-height rem) 2)]
                  [x2 (+ 0 (* letter-width len) delta delta)]
                  [y2 y1])
             (set! list-of-actions
                   (cons (lambda()
                           (send bm-dc set-pen red-pen)
                           (send bm-dc set-brush yellow-brush)
                           (send bm-dc draw-rectangle x1 y1 x2 23)) list-of-actions))
             )])(and check-same common)))

;;Highlights the selected letters in the grid.
(define (draw-box b pn brush)
  (define (condn t) (>= t 0))
  (define (change t) (- t 0.3))
  (define i1 (bx-x1 b))
  (define j1 (bx-y1 b))
  (define i2 (bx-x2 b))
  (define j2 (bx-y2 b))
  (define (f t)
    (send bm-dc set-pen pn)
    (send bm-dc set-brush brush)
    (send bm-dc draw-ellipse  (+ (- grid-x delta1) (* box-width i1) (* t (- i2 i1)))
          (+ (+ grid-y delta1) (* box-width j1) (* t (- j2 j1))) radius radius))
  (lambda() (if(or (= i1 i2) (= j1 j2) (= (abs (- i1 i2)) (abs (- j1 j2))))
               (iterator 30 condn f change) 'ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Canvas
(define face-bitmap (make-bitmap 1500 1500))
(define bm-dc (new bitmap-dc% [bitmap face-bitmap]))

(define my-canvas%
  (class canvas%
    (define/override (on-event event)  ;Overriding mouse events on the grid.
      (define cx (send event get-x))
      (define cy (send event get-y))
      (define i (quotient (- cx grid-x) box-width))
      (define j (quotient (- cy grid-y) box-width))
      (cond[(and (= select 1) (<= i size) (<= j size) (>= i 0) (>= j 0))
            (send bm-dc clear)
            (if (send event button-down? 'left)
                (begin
                  (set! click-state (modulo (+ 1 click-state) 2))
                  (if(= 1 click-state)
                     (set! first-click (cons i j))
                     (begin
                       (send bm-dc clear)
                       (let ([ln (bx (car first-click) (cdr first-click)  i j)])
                         (cond[(present? ln  )
                               (set! list-of-actions (cons (draw-box ln green-pen green-brush)
                                                           list-of-actions))])))))
                (cond[(= 1 click-state)
                      (begin
                        ((draw-box (bx (car first-click) (cdr first-click)  i j) yellow-pen yellow-brush)))]
                     ))
            (map (lambda(t) (t)) list-of-actions)
            (paint bm-dc)
            (send t on-paint)]))
    
    (define/override (on-char event)   ;Overriding key-events only to implement cheat.
      (define pressed (send event get-key-code))
      (cond  [(equal? pressed #\backspace) (set! cheat "")] ;refreshes the cheat
             [(char? pressed)
              (set! cheat (string-append cheat (make-string 1 pressed)))])
      (cond[(equal? cheat "solve") (solve)]       ;shows complete solution if (equal? cheat "solve")
           [(equal? cheat "erase")  ; erases all the solved part till then if (equal? cheat "erase")
            (set! list-of-actions '())
            (send bm-dc clear)
            (paint bm-dc)
            (send t on-paint)]))
    (super-new)))

(define t (new my-canvas% [parent frame]
               [paint-callback (lambda(canvas dc) (send dc draw-bitmap face-bitmap 0 0)
                                 )]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Displaying entire canvas used (grid, words displayed).
(define (paint dc)
  (send bm-dc set-font (make-font #:size 15 #:family 'swiss #:weight 'bold))  
  (send bm-dc set-text-foreground  "blue")
  (disp-grid)
  (disp-box)
  (send bm-dc set-text-foreground  "black")
  (define pl word-details)
  (iterator 0
            (lambda(t) (< t (vector-length word-details)))
            (lambda(t) (send bm-dc draw-text (2d-vector-ref word-details t 0)
                             (+ word-x (* word-len (quotient t nwords)))
                             (+ word-y (* word-height (remainder t nwords)))))
            (lambda(t) (+ t 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Shows complete solution.
(define (solve)
  (define (condn i) (>= i 0))
  (define (change i) (- i 1))
  (define (f i)
    (let ([b (bx (cdr (2d-vector-ref word-details i 1))
                 (car (2d-vector-ref word-details i 1))
                 (cdr (2d-vector-ref word-details i 2))
                 (car (2d-vector-ref word-details i 2)))])
      (set! list-of-actions (cons (draw-box b green-pen green-brush) list-of-actions))
      (present? b)))
  (iterator (- (vector-length word-details) 1) condn f change)
  (map (lambda(t) (t)) list-of-actions)
  (paint bm-dc)
  (send t on-paint) (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Parsing the words given by user.
(define (word-parse l s)
  (cond[(equal? "" s) (if(null? l) '()
                         (if(and (> (length l) 2) (<= (length l) n))
                            (list (reverse l)) '()))]
       [#t (let* ([c (string-ref s 0)]
                  [ac (char->integer c)]
                  [rem (substring s 1)])
             (if(and (<= ac 122)  (>= ac 97)) (word-parse (cons c l) rem)
                (if(and (> (length l) 2) (<= (length l) n)) (cons (reverse l)
                                                                  (word-parse '()  rem))
                   (word-parse '()  rem) )))]))

;;Differentiates b/w 2 modes of input.
(define (play-grid s)
  (if(equal? s 'solve-a-grid) 'ok
     (let* ([e-list  (word-parse '() s)]
            [l (length e-list)]
            [l1 (quotient l 6)]
            [r1 (remainder l 6)])
       ;(display e-list)
       (set! word-list e-list)
       (if(> l 40) 'ok
          (begin (set! dd l1)
                 (set! du l1)
                 (set! h (* 2 l1))
                 (set! v (+ (* 2 l1) r1))))))
  (re-run)
  (final-grid grid)
  (send bm-dc clear)
  (paint bm-dc)
  (send t on-paint))

;Refreshing everything 
(define (refresh)
  (set! list-of-actions '())
  (set! word-check null-vector)
  (set! grid null-vector)
  (set! word-details '())
  (set! p (send textt get-value))
  (set! difficulty (string->number p))
  (set! n (* 2 (+ 2 difficulty)))
  (set! size (- n 1))
  (set! h (+ 1 (quotient n 2))) 
  (set! v h)
  (set! dd (- (quotient n 2) 3))
  (set! du dd)
  (set! nw (+ h v dd du))
  (set! nwords (quotient nw 2)))

;;Finally displaying canvas.
(send frame show #t)