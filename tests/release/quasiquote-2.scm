(begin
  (define a (quote 4))
  (define b (quote 8))
  (define (f y x) (quasiquote ((unquote x) unquote y)))
  (quasiquote ((unquote (+ 1 2)) (unquote (f a b)) (unquote 7))))
