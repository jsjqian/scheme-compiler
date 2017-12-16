(let ([a '6])
  (cond ['#f (begin (set! a (+ a '3)) a)]
        ['#f (begin (set! a (+ a '4)) a)]
        ['#t (begin (set! a (+ a '5)) a)]))
