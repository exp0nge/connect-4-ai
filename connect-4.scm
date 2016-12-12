(define player-1 'X)

(define player-2 'O)

(define num-rows 6)

(define num-cols 7)

(define generate-list
  (lambda (size)
    (cond
      ((= size 0) (quote ()))
      (else (cons (quote ()) (generate-list (- size 1)))))))

(define init-board-matrix
  ;; Each row-list contains that level's columns
  ; ((() () () () () () ())
  ; (() () () () () () ())
  ; (() () () () () () ())
  ; (() () () () () () ())
  ; (() () () () () () ())
  ; (() () () () () () ()))
  (map
   (lambda (row) (append row (generate-list num-cols)))
   (generate-list num-rows)))






