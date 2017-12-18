(define a 24)

(define id (lambda (x) x))

(letrec*
  ([c 42])
  (id a)
  (letrec
    ([b (lambda (x y) (y x))])
      (b d a)))
