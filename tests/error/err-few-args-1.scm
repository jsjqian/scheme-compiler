((lambda (x z w y)
   (let ([q (if (= z x)
                w
                y)])
      (q w 2 3 4 5 6 7))) 12 23 (lambda (v w x y z) x) (lambda (t u v w x y z)
                                            (t y)))
