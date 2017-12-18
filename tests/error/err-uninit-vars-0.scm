(define a 24)

(define id (lambda (x) x))

(letrec*
  ([c 42])
  (id b))
