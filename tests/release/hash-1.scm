(define h (hash 1 2 3 4 5 6))
(define n (hash-remove h 3))
(+ (hash-ref h 1) (if (hash-has-key? n 3)
                    (hash-ref n 3)
                    (hash-ref h 3)))
