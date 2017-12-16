(begin
    (define x 2)
    (begin
      (define
        (f a b c)
        (+ 1 2)
        (begin
          (define x 5)
          (define (t a b c . z) c)
          (t 1 2 5))
        (+ a b c x)))
    (f 1 3 5))
