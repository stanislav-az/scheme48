(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n)))
(factorial 100000)
