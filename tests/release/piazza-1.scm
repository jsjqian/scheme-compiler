(guard
  (errval 
    [(equal? errval "nope.avi") (print "We just got nope'd\n") "just didnt like you"]
    [(= errval 0) "uhhh its zero i guess"]
    [(null? errval) "well thats unhelpful"]
    [else "who knows."])
  (print "particularly useless print statement\n")
  (raise "nope.avi")
  (raise "This should not be run!"))
