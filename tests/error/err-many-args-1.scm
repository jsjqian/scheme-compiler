((lambda (x z w y) 
   (let ([q (if (= z x)
                w
                y)])
     (q y 23))) 12 23 (lambda (x) x) (lambda (x y) (x y y y y y)))
