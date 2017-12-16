
(let ([a '4])
      (let ([p (delay
                  (begin
                  (set! a '3)
                  a))])
  (+ (force p)
     (begin
      (set! a '5)
      (force p)))))
