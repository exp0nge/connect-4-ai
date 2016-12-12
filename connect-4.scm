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
  ;; Each row-list contains that level's columns
  ; ((() () () () () () ())
  ; (() () () () () () ())
  ; (() () () () () () ())
  ; (() () () () () () ())
  ; (() () () () () () ())
  ; (() () () () () () ()))
  (init-vector-rows (make-vector num-rows) 0))

(define board init-board-matrix)



