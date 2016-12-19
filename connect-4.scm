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

(define valid-move?
  (lambda (board col)
    (define check-lowest-row
      (lambda (row)
        (cond
          ((< row 0) #f)
          ((null? (vector-ref (vector-ref board row) col)) #t)
          (else (check-lowest-row (- row 1))))))
    (check-lowest-row (- num-rows 1))))

(define drop-chip
  (lambda (board col player)
    (define check-lowest-row
      (lambda (row)
        (cond
          ((< row 0) #f)
          ((null? (get-slot board row col))
           (vector-set! (vector-ref board row) col player) #t)
          (else (and (check-lowest-row (- row 1)) #t)))))
    (check-lowest-row (- num-rows 1))))

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


(define win-n?
  (lambda (board player n)
    (cond ((column-win-n? board player n) #t)
          ((row-win-n? board player n) #t)
          ((diagonal-win-n? board player n) #t)
          (else #f))))


(define column-win-n?
  (lambda (board player n)
    (define counter
      (lambda (count)
        (cond ((< count 0) #f)
              ((>= (count-max-continuous-player board (get-col board count) player) n) #t)
              (else (counter (- count 1))))))
    (counter (- num-cols 1))))

(define row-win-n?
  (lambda (board player n)
    (define counter
      (lambda (count)
        (cond ((< count 0) #f)
              ((>= (count-max-continuous-player board (get-row board count) player) n) #t)
              (else (counter (- count 1))))))
    (counter (- num-rows 1))))

(define diagonal-win-n?
  (lambda (board player n)
    (let ((right-to-left (vector-ref (get-diagonal board) 0))
          (left-to-right (vector-ref (get-diagonal board) 1)))
      (define counter
        (lambda (count)
          (cond ((< count 0) #f)
                ((>= (count-max-continuous-player board (vector-ref left-to-right count) player) n) #t)
                ((>= (count-max-continuous-player board (vector-ref right-to-left count) player) n) #t)
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


(define INF 10000000000000000000000000000000000000000000000000000000000000000000)

(define is-max?
  (lambda (player)
    (equal? player ai-player)))

(define opponent
  (lambda (player)
    (cond
      ((equal? player player-1) player-2)
      (else player-1))))

(define candidate-board car)
(define candidate-move cadr)

(define make-interval
  (lambda (min max)
    (cond
      ((= min max) '())
      (else (cons min (make-interval (+ min 1) max))))))

(define clone-board
  (lambda (board)
    (let ((clone (make-vector num-rows)))
      (map
       (lambda (row) (vector-set! clone row (vector-copy (vector-ref board row))))
       (make-interval 0 num-rows))
      clone)))

(define all-possible-moves
  (lambda (board player)
    (define col-iter
      (lambda (col)
        (cond
          ((= col num-cols) '())
          ((valid-move? board col) (cons col (col-iter (+ col 1))))
          (else (col-iter (+ col 1))))))
    (map (lambda (col)
           (let ((board-copy (clone-board board)))
             (drop-chip board-copy col player)
             (list board-copy col))) ; builds (board col)
         (col-iter 0))))

(define complete-board?
  (lambda (board)
    (define false-exists?
      (lambda (lst)
        (cond
          ((null? lst) #f)
          ((car lst) (false-exists? (cdr lst)))
          (else #t))))

    (false-exists? (map (lambda (col) (valid-move? board col)) (make-interval 0 num-cols)))))


(define score
  (lambda (board player)
    (cond
      ((win-n? board player 4) 10000)
      ((win-n? board player 3) 1000)
      ((win-n? board player 2) 300)
      (else 0))))

(define best-move '())

(define minimax
  (lambda (depth board player)
    ; returns: int (column of best move)
    (cond
      ((= depth 0) (if (is-max? player) 1000 -1000))
      ((complete-board? board) (score board player))
      ((is-max? player) (let ((max-score (- INF)))
                          (map
                           (lambda (candidate-board-move)
                             (let ((candidate-score (minimax (- depth 1)
                                                             (candidate-board candidate-board-move)
                                                             (opponent player))))
                               (cond
                                 ((> candidate-score max-score)
                                  (set! max-score candidate-score)
                                  (set! best-move (candidate-move candidate-board-move))))))
                           (all-possible-moves board player))
                          max-score))
      (else (let ((min-score INF))
              (map
               (lambda (candidate-board-move)
                 (let ((candidate-score (minimax (- depth 1)
                                                 (candidate-board candidate-board-move)
                                                  (opponent player))))
                   (cond
                     ((< candidate-score min-score)
                      (set! min-score candidate-score)
                      (set! best-move (candidate-move candidate-board-move))))))
               (all-possible-moves board player))
              min-score)))))

(define count-continuous
  (lambda (board vec player-to-check n)
    (let ((n-count 0)
          (counter 0))
      (vector-map
       (lambda (player) (if (equal? player player-to-check) ; player symbol are equal
                            (cond
                              ((= (+ counter 1) n)
                               (set! counter 0)
                               (set! n-count (+ n-count 1)))
                              (else (set! counter (+ counter 1))))
                            (set! counter 0)))
       vec)
      n-count)))

(define count-continuous-col
  (lambda (board player n)
    (define counter
      (lambda (count total)
        (cond ((< count 0) total)
              (else (let ((col-count (count-continuous board (get-col board count) player n)))
                      (counter (- count 1) (+ total col-count)))))))
    (counter (- num-cols 1) 0)))

(define count-continuous-row
  (lambda (board player n)
    (define counter
      (lambda (count total)
        (cond ((< count 0) total)
              (else (let ((row-count (count-continuous board (get-row board count) player n)))
                      (counter (- count 1) (+ total row-count)))))))
    (counter (- num-rows 1) 0)))

(define count-continuous-diagonal
  (lambda (board player n)
    (let ((right-to-left (vector-ref (get-diagonal board) 0))
          (left-to-right (vector-ref (get-diagonal board) 1)))
      (define counter
        (lambda (count total)
          (cond ((< count 0) total)
                (else (let ((right-to-left-count (count-continuous board (vector-ref right-to-left count) player n))
                            (left-to-right-count (count-continuous board (vector-ref left-to-right count) player n)))
                        (counter (- count 1) (+ total right-to-left-count left-to-right-count)))))))
      (counter (- (vector-length left-to-right) 1) 0))))

(define total-count-continuous-n
  (lambda (board player n)
    (let ((column-count (count-continuous-col board player n))
          (row-count (count-continuous-row board player n))
          (diagonal-count (count-continuous-diagonal board player n)))
      (+ column-count row-count diagonal-count))))



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

(define board init-board-matrix)


(display "SIMULATING MINIMAX\n")
(print-board board)
(define ai-player player-1)
(define max-depth 5)
;(minimax max-depth board player-1)

;(drop-chip board 1 'X)
;(drop-chip board 1 'O)
;(drop-chip board 1 'O)
;(drop-chip board 1 'O)
;(drop-chip board 1 'X)
;(drop-chip board 1 'X)
;(print-board board)
;(count-max-continuous-player board (get-col board 1) 'X)
;(count-max-continuous-player board (get-col board 1) 'O)
