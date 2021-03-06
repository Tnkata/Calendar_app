#lang racket

;;; Project 0 Tic-tac-toe with Racket
;;; 
;;; Please immediately read README.md

(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;;

(define (bdHelp lst)
  (cond [(empty? lst) #t]
        [(equal? (car lst) 'X) (bdHelp (cdr lst))]
        [(equal? (car lst) 'O) (bdHelp (cdr lst))]
        [(equal? (car lst) 'E) (bdHelp (cdr lst))]
        [else #f]))


; Check whether a list is a valid board
(define (board? lst)
  (cond [(empty? lst) #f]
        [(not(integer?(sqrt(length lst)))) #f]
        [(equal? (bdHelp lst) #f) #f]
        [(> (count (lambda (x) (equal? x 'X)) lst) (+ 1 (count (lambda (x) (equal? x 'O)) lst))) #f]
        [(and (equal? (car lst) 'X) (not(equal? (car lst) 'E))) #t]
        [else #t]))
      
       

;;; From the board, calculate who is making a move this turn
(define (next-player board)
  (cond [(empty? board) '()]
        [(< (count (lambda (x) (equal? x 'X)) board) (count (lambda (x) (equal? x 'O)) board)) 'X]
        [(equal? (count (lambda (x) (equal? x 'X)) board) (count (lambda (x) (equal? x 'O)) board)) 'X]
        [else 'O]))

;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)
(define (valid-move? board row col player)
  (cond [(empty? board) #f]
        [(and (< row (sqrt (length board))) (< col (sqrt(length board))) (equal? (next-player board) player) (equal? (list-ref board (+ (* row (sqrt(length board))) col)) 'E)) #t]
        [else #f]))
  

;;; To make a move, replace the position at row col to player ('X or 'O)
(define (make-move board row col player)
  (cond [(equal? col '0) (list-set board row player)]
        [(equal? col '2) (list-set board (+ 4 col row) player)]
        [(equal? col '1) (list-set board (+ 2 col row) player)]
        [else board]))
      

;;; To determine whether there is a winner?
(define (xCount lst)
  (if (empty? lst)
      0
      (if (equal? (car lst) 'X)
          (+ 1 (xCount (cdr lst)))
          (xCount (cdr lst)))))

(define (oCount lst)
  (if (empty? lst)
      0
      (if (equal? (car lst) 'O)
          (+ 1 (oCount (cdr lst)))
          (oCount (cdr lst)))))

(define (winner? board)
  (cond [(empty? board) #f]
        [(and (equal? (sqrt(length board)) (xCount board)) (equal? (car board) 'X)) 'X]
        [(equal? (sqrt(length board)) (oCount board)) 'O]
        [else #f]))

;;; The board is the list containing E O X 
;;; Player will always be 'O
;;; returns a pair of x and y
(define (calculate-next-move board player)
  'todo)

(board? '(E E E X E X E E))
(next-player '(X E E E X E E E E))
(winner? '(O O O O X O X X E))