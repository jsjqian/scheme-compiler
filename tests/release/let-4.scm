(let fac ([n '10])
    (if (= '0 n)
        '1
        (* n (fac (- n '1)))))
