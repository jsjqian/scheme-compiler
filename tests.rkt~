#lang racket

;; Testing apparatus for final

(require "scm-llvm.rkt")
(require "utils.rkt")


(define ((make-test path) exp ext)
  (lambda ()
    (define t0 (test-scm-llvm scm-llvm exp))
    t0))

(define ((make-error-test path) exp ext)
  (lambda ()
    (define t0 (test-error scm-llvm exp))
    t0))

(define (tests-list dir)
  (map
   (lambda (path)
     (string->path
      (string-append "tests/" dir "/"
                     (path->string path))))
   (filter (lambda (path)
             (define p (path->string path))
             (member (last (string-split p ".")) '("scm")))
           (directory-list (string-append "tests/" dir "/")))))


(define ((path->test type) p)
  (define filename (last (string-split (path->string p) "/")))
  (if (equal? type 'error)
      `(,(string-append (last (string-split (string-join (drop-right (string-split (path->string p) ".") 1) ".") "/")))
        ,type
        ,((make-error-test p)
          (with-input-from-file p read-begin #:mode 'text)
          (last (string-split (path->string p) "."))))
      `(,(string-append (last (string-split (string-join (drop-right (string-split (path->string p) ".") 1) ".") "/")))
        ,type
        ,((make-test p)
          (with-input-from-file p read-begin #:mode 'text)
          (last (string-split (path->string p) "."))))))

(define public-tests `(,@(map (path->test 'public) (tests-list "public"))))

(define release-tests `(,@(map (path->test 'release) (tests-list "release"))))

(define secret-tests `(,@(map (path->test 'secret) (tests-list "secret"))))

(define error-tests `(,@(map (path->test 'error) (tests-list "error"))))

(define tests
  `(,@public-tests
    ,@release-tests
    ,@secret-tests
    ,@error-tests))

(define (run-test/internal is-repl . args)
  (define (correct-count test-list)
    (foldl (lambda (testcase count)
             (match testcase
               [(list test-name _ exec)
                (define exec-result
                  (with-handlers ([exn:fail? identity])
                    (exec)))
                (if (eq? exec-result #t)
                    (begin
                      ;; (display "Test ")
                      ;; (display test-name)
                      ;; (display " passed.")
                      ;; (newline)
                      (+ count 1))
                    (begin
                      (display "Test ")
                      (display test-name)
                      (display " failed!")
                      (newline)
                      count))]))
           0
           test-list))
  ;; Run all tests, a specific test, a class of tests, or print the available tests
  (match args
    [(list "all")
     (display "Score on all available tests: ")
     (display (/ (round (/ (* 10000 (correct-count tests)) (length tests))) 100.0))
     (display "%")
     (newline)]

    [(list "public")
     (display "Score on available public tests: ")
     (display (/ (round (/ (* 10000 (correct-count public-tests)) (length public-tests))) 100.0))
     (display "%")
     (newline)]
    
    [(list "release")
     (display "Score on available release tests: ")
     (display (/ (round (/ (* 10000 (correct-count release-tests)) (length release-tests))) 100.0))
     (display "%")
     (newline)]
    
    [(list "secret")
     (display "Score on available secret tests: ")
     (display (/ (round (/ (* 10000 (correct-count secret-tests)) (length secret-tests))) 100.0))
     (display "%")
     (newline)]
    
    [(list "error")
     (display "Score on available error tests: ")
     (display (/ (round (/ (* 10000 (correct-count error-tests)) (length error-tests))) 100.0))
     (display "%")
     (newline)]

    [(list "mk-test-props")
     (define groupped-tests
       ;; key: group (symbol)
       ;; value: reversed list of testcases
       (foldl
        (lambda (testcase h)
          (match testcase
            [(list _ grp _)
             (define cur-group
               (hash-ref h grp '()))
             (hash-set h grp (cons testcase cur-group))]))
        (hash)
        tests))
     (for-each
      displayln
      '("build.language=c"
        "build.make.file=Makefile"
        "test.exec=timeout -s KILL 55s /usr/local/bin/racket ./tests.rkt &"))
     (for-each
      (lambda (kv)
        (match kv
          [(cons grp ts)
           (define testnames
             (reverse (map car ts)))
           (printf
            "test.cases.~a=~a~n"
            grp
            (string-join
             testnames
             ","))]))
      (hash->list
       groupped-tests))]

    [(list test-name)
     #:when (assoc test-name tests)
     (match (assoc test-name tests)
       [(list _ _ exec)
        (define exec-result
          (with-handlers ([exn:fail? identity])
            (exec)))
        (define passed (eq? exec-result #t))
        (displayln (if passed "Test passed!" "Test failed!"))
        (unless is-repl
          (exit (if (eq? exec-result #t) 0 1)))])]
    [else
     (display "Available tests: ")
     (newline)
     (display
      (string-join
       (map car tests)
       ", "))
     (newline)]))

(define run-test
  (curry run-test/internal #t))

(apply
 run-test/internal
 (cons
  #f
  (vector->list (current-command-line-arguments))))



