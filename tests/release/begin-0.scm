(let ((n (quote 1)))
  (begin (set! n (+ n (quote 1)))
         (set! n (* (quote 2) n))
         (set! n (* n n n)) n))
