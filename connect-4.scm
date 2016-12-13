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
  (lambda (col player)
    (define check-lowest-row
      (lambda (row)
        (cond
          ((< row 0) (display "invalid move") (newline))
          ((null? (vector-ref (vector-ref board row) col))
           (vector-set! (vector-ref board row) col player))
          (else (check-lowest-row (- row 1))))))
    (check-lowest-row (- num-rows 1))))

(define print-board
  (lambda ()
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
    (iter 0)))

(define get-col
  (lambda (col)
    (let ((requested-col (make-vector num-rows))
          (fill-col 0))
      (vector-map
       (lambda (row)
         (newline)
         (vector-set! requested-col fill-col (vector-ref row col))
         (set! fill-col (+ fill-col 1)))
       board)
      requested-col)))
