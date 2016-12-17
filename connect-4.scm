(#%require racket/vector)

(define player-1 'X)

(define player-2 'O)

(define num-rows 6)

(define num-cols 7)

(define init-vector-rows
  (lambda (rows count)
    (cond
      ((= count num-rows) rows)
      (else (vector-set! rows count (make-vector num-cols '()))
            (init-vector-rows rows (+ count 1)))))) 

(define init-board-matrix
  ;; Vector contains columns vectors
  ; #(#(() () () () () () ())
  ; #(() () () () () () ())
  ; #(() () () () () () ())
  ; #(() () () () () () ())
  ; #(() () () () () () ())
  ; #(() () () () () () ()))
  (init-vector-rows (make-vector num-rows) 0))

(define board init-board-matrix)

(define drop-chip
  (lambda (board col player)
    (define check-lowest-row
      (lambda (row)
        (cond
          ((< row 0) #f)
          ((null? (vector-ref (vector-ref board row) col))
           (vector-set! (vector-ref board row) col player) #t)
          (else (and (check-lowest-row (- row 1)) #t)))))
    (check-lowest-row (- num-rows 1))))
    ;(print-board board)))
    ;(cond ((win? board 'X) (display "Player X Wins"))
          ;((win? board 'O) (display "Player O Wins")))
          ;(newline)))

(define print-board
  (lambda (board)
    (define iter
      (lambda (row)
        (cond
          ((= row num-rows) (display "0 1 2 3 4 5 6"))
          (else
           (vector-map (lambda (col) (cond
                                       ((null? col) (display " |"))
                                       (else (display col) (display "|"))))
                       (vector-ref board row))
           (newline)
           (iter (+ row 1))))))
    (iter 0)
    (newline)))

(define get-col
  (lambda (board col)
    (let ((requested-col (make-vector num-rows))
          (fill-col 0))
      (vector-map
       (lambda (row)
         (vector-set! requested-col fill-col (vector-ref row col))
         (set! fill-col (+ fill-col 1)))
       board)
      requested-col)))

(define get-row
  (lambda (board row)
    (vector-ref board row)))

(define get-slot
  (lambda (board row col)
    (vector-ref (get-row board row) col)))

(define count-max-continuous-player
  (lambda (board vec player-to-check)
    (let ((max-count 0)
          (count 0))
      (vector-map
       (lambda (player) (if (equal? player player-to-check) ; player symbol are equal
                            (cond
                              ((> (+ count 1) max-count)
                                (set! count (+ count 1))
                                (set! max-count count))
                              (else (set! count (+ count 1))))
                            (set! count 0)))
       vec)
      max-count)))


(define win?
  (lambda (board player)
    (cond ((column-win? board player) #t)
          ((row-win? board player) #t)
          ((diagonal-win? board player) #t)
          (else #f))))


(define column-win?
  (lambda (board player)
    (define counter
      (lambda (count)
        (cond ((< count 0) #f)
              ((>= (count-max-continuous-player board (get-col board count) player) 4) #t)
              (else (counter (- count 1))))))
    (counter (- num-cols 1))))

(define row-win?
  (lambda (board player)
    (define counter
      (lambda (count)
        (cond ((< count 0) #f)
              ((>= (count-max-continuous-player board (get-row board count) player) 4) #t)
              (else (counter (- count 1))))))
    (counter (- num-rows 1))))

(define diagonal-win?
  (lambda (board player)
    (let ((right-to-left (vector-ref (get-diagonal board) 0))
          (left-to-right (vector-ref (get-diagonal board) 1)))
      (define counter
        (lambda (count)
          (cond ((< count 0) #f)
                ((>= (count-max-continuous-player board (vector-ref left-to-right count) player) 4) #t)
                ((>= (count-max-continuous-player board (vector-ref right-to-left count) player) 4) #t)
                (else (counter (- count 1))))))
      (counter (- (vector-length left-to-right) 1)))))

(define get-diagonal
  (lambda (board)
    (vector (get-left-to-right-diagonals board)
            (get-right-to-left-diagonals board))))


(define get-right-to-left-diagonals
  (lambda (board)
    (let ((start-row 0)
          (start-col (- num-cols 4))
          (v (vector)))
      (define diagonal-iteration
        (lambda (row col)
          (cond ((< col 0) (vector))
                ((>= row num-rows) (vector))
                (else (vector-append v (vector (get-slot board row col)) (diagonal-iteration (+ row 1) (- col 1) ))))))
      (define iter
        (lambda (row col)
          (cond ((>= col num-cols) (iter (+ row 1) (- col 1)))
                ((> row (- num-rows 4)) (vector))
                (else (vector-append (vector (diagonal-iteration row col)) (iter row (+ col 1)))))))
      (iter start-row start-col))))

(define get-left-to-right-diagonals
  (lambda (board)
    (let ((start-row 0)
          (start-col (- num-cols 4))
          (v (vector)))
      (define diagonal-iteration
        (lambda (row col)
          (cond ((>= col num-cols) (vector))
                ((>= row num-rows) (vector))
                (else (vector-append v (vector (get-slot board row col)) (diagonal-iteration (+ row 1) (+ col 1) ))))))
      (define iter
        (lambda (row col)
          (cond ((< col 0) (iter (+ row 1) 0))
                ((> row (- num-rows 4)) (vector))
                (else (vector-append (vector (diagonal-iteration row col)) (iter row (- col 1)))))))
      (iter start-row start-col))))



;; Check Row Win
;(drop-chip board 0 'O)
;(drop-chip board 1 'O)
;(drop-chip board 2 'O)
;(drop-chip board 3 'O)
;(print-board board)
;(win? board 'X)
;(win? board 'O)

;; Check Column Win
;(drop-chip board 6 'X)
;(drop-chip board 6 'X)
;(drop-chip board 6 'X)
;(drop-chip board 6 'X)
;(print-board board)
;(win? board 'X)
;(win? board 'O)

(drop-chip board 0 'X)
(drop-chip board 1 'O)
(drop-chip board 1 'X)
(drop-chip board 2 'O)
(drop-chip board 2 'O)
(drop-chip board 2 'X)
(drop-chip board 3 'O)
(drop-chip board 3 'X)
(drop-chip board 3 'O)
(drop-chip board 3 'X)
(print-board board)
(win? board 'X)
(win? board 'O)


;(drop-chip board 1 'X)
;(drop-chip board 1 'O)
;(drop-chip board 1 'O)
;(drop-chip board 1 'O)
;(drop-chip board 1 'X)
;(drop-chip board 1 'X)
;(print-board board)
;(count-max-continuous-player board (get-col board 1) 'X)
;(count-max-continuous-player board (get-col board 1) 'O)
