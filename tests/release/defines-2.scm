(define (f a b c . d) d)

(f 1 2 3 4 5 6 7 8)

(define (r a b . x) (cons 'a x))

(r 1 2 3 4 5)

((lambda (a b . c) c) 1 2 3 4 5 6)
