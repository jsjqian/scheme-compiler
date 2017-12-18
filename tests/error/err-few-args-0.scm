((lambda (x z w y) (let ([q (if (= z x)
                                w
                                y)])
                      (q 54 23))) 12 23 (lambda (x z) x) (lambda (x y z) y))
