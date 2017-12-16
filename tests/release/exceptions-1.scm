

(guard (x [else x])
  (+ '1 (guard (x ['#f ((lambda (y) x) '8)]) (+ '2 (raise '3)))))

