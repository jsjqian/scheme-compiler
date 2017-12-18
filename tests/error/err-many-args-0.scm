((lambda (x z w y) (let ([q (if (= z x)
                                y
                                w)])
                      (q 54 23))) 12 23 (lambda (x) x) (lambda (x y) y))
