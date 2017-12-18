(define a 42)
(if ((lambda (x) #f) 2)
  (a 2 3 4 5)
  (begin
    (let ([a (lambda (x) x)])
      (define b 52)
      ((a b) 0))))
