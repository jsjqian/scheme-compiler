#lang racket

(provide scm-llvm)

(require "utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 2 (taken from Thomas Gilray)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (t-desugar e) 
  (match e
    [`(letrec ([,xs ,es] ...) ,e0)
     (define ts (map gensym xs))
     `(let ,(map (lambda (x e) `(,x '())) xs es)
        (let ,(map (lambda (x e) `(,x ,(t-desugar e))) ts es)
          ,(t-desugar
            `(begin
               ,@(map (lambda (x t) `(set! ,x ,t)) xs ts)
               ,e0))))]

    [`(letrec* ([,xs ,es] ...) ,e0)
     `(let ,(map (lambda (x e) `(,x '())) xs es)
        ,(t-desugar
          `(begin
             ,@(map (lambda (x e) `(set! ,x ,e)) xs es)
             ,e0)))]

    [`(lambda ,(? symbol? x) ,e0)
     `(lambda ,x ,(t-desugar e0))]
    [`(lambda (,(? symbol? xs) ...) ,e0)
     `(lambda ,xs ,(t-desugar e0))]
    [`(lambda ,xs ,e0)
     (define t (gensym 'args))
     (define (wrap e xs)
       (if (symbol? xs)
           `(let ([,xs ,t]) ,e)
           `(let ([,(car xs) (car ,t)] [,t (cdr ,t)])
              ,(wrap e (cdr xs)))))
     (t-desugar
      `(lambda ,t
         ,(wrap e0 xs)))]

    [`(let ([,xs ,es] ...) ,e0)
     `(let ,(map (lambda (x e) `(,x ,(t-desugar e))) xs es)
        ,(t-desugar e0))]
    [`(let ,lp ([,xs ,es] ...) ,e0)
     (t-desugar
      `(letrec ([,lp (lambda ,xs ,e0)])
         (,lp . ,es)))]

    [`(let* () ,e0)
     (t-desugar e0)]
    [`(let* ([,x ,e0] . ,rest) ,e1)
     (t-desugar
      `(let ([,x ,e0]) (let* ,rest ,e1)))]

    [`(delay ,e0)
     (t-desugar
      `(list '%promise
             (lambda () ,e0)
             (make-vector '2 '#f)))]
         
    [`(force ,e0)
     (define t (gensym 'promise))
     (t-desugar
      `(let* ([,t ,e0]
              [vec (third ,t)])
         (if (vector-ref vec '0)
             (vector-ref vec '1)
             (let ([pval ((second ,t))])
               (begin (vector-set! (third ,t) '0 '#t)
                      (vector-set! (third ,t) '1 pval)
                      pval)))))]
         
    [`(promise? ,e0)
     (define t (gensym 'promise))
     (t-desugar
      `(let ([,t ,e0])
         (and (cons? ,t) (eq? '%promise (car ,t)))))]

    [`(let/cc ,x ,e0)
     (t-desugar
      `(call/cc (lambda (,x) ,e0)))]

    [`(call/cc ,e0)
     `(call/cc
       ,(t-desugar
         `(lambda (k) ;save k's stack
            (,e0 (let ([k-stack %wind-stack])
                   (lambda (x) ; first do whatever winding is needed
                     (begin (%do-wind k-stack) (k x))))))))]

    [`(dynamic-wind ,e0 ,e1 ,e2)
     (t-desugar `(%dynamic-wind ,e0 ,e1 ,e2))]

    [`(guard (,x) ,e0)
     (t-desugar e0)]
    [`(guard (,x ,clauses ...) ,e0)
     (t-desugar
      `(let* ([%old-handler %raise-handler]
              [%kont (call/cc (lambda (u) (u u)))])
         (cond
           [(cons? %kont)
            (let ([,x (car %kont)])
              (cond . ,clauses))]
           [else
            (dynamic-wind
             (lambda () (set! %raise-handler (lambda (x) (%kont (cons x '())))))
             (lambda () ,e0)
             (lambda () (set! %raise-handler %old-handler)))])))]

    [`(raise ,e0)
     (t-desugar
      `(%raise-handler ,e0))]
         
    [`(and) '(quote #t)]
    [`(and ,e0) (t-desugar e0)]
    [`(and ,e0 ,es ...)
     (t-desugar
      `(if ,e0 (and . ,es) '#f))]

    [`(or) '(quote #f)]
    [`(or ,e0) (t-desugar e0)]
    [`(or ,e0 ,es ...)
     (define t (gensym 'or))
     (t-desugar
      `(let ([,t ,e0])
         (if ,t ,t (or . ,es))))]

    [`(cond) `(prim void)]
    [`(cond [else ,e0])
     (t-desugar e0)]
    [`(cond [,e0] . ,rest)
     (define t (gensym 'cond))
     (t-desugar
      `(let ([,t ,e0])
         (if ,t ,t (cond . ,rest))))]
    [`(cond [,e0 ,e1] . ,rest)
     (t-desugar
      `(if ,e0 ,e1 (cond . ,rest)))]

    [`(case ,e0 ,clauses ...)
     #:when (not (symbol? e0))
     (define t (gensym 'case))
     (t-desugar
      `(let ([,t ,e0])
         (case ,t . ,clauses)))]
    [`(case ,x) `(prim void)]
    [`(case ,x [else ,e0])
     (t-desugar e0)]
    [`(case ,x [(,ds ...) ,e0] . ,rest)
     (t-desugar
      `(if (memv ,x (quote ,ds))
           ,e0
           (case ,x . ,rest)))]

    [`(set! ,x ,e0)
     `(set! ,x ,(t-desugar e0))]
 
    [`(begin ,e0) (t-desugar e0)]
    [`(begin ,e0 . ,rest)
     (t-desugar
      `(let ([_t ,e0])
         (begin . ,rest)))]

    [`(if ,es ...)
     `(if . ,(map t-desugar es))]

    [`(when ,e0 ,e1)
     (t-desugar `(if ,e0 ,e1 (void)))]

    [`(unless ,e0 ,e1)
     (t-desugar `(if (not,e0) ,e1 (void)))]

    ['promise? (t-desugar `(lambda (x) (promise? x)))]
    [(? prim? op) `(lambda args (apply-prim ,op args))]
    [(? symbol? x) x]

    [`(quote ,(? datum? d)) `(quote ,d)]

    [`(apply ,e0 ,e1)
     `(apply ,(t-desugar e0) ,(t-desugar e1))]

    [`(,(? prim? op) ,es ...)
     `(prim ,op . ,(map t-desugar es))]

    [`(,es ...)
     (map t-desugar es)]))


(define (desugar e)
  (define (wrap-with-lib e)
    `(let* ([%wind-stack '()]
            [common-tail (lambda (x y)
                           (let ((lx (length x))
                                 (ly (length y)))
                             (let loop ([x (if (> lx ly) (drop x (- lx ly)) x)]
                                        [y (if (> ly lx) (drop y (- ly lx)) y)])
                               (if (eq? x y)
                                   x
                                   (loop (cdr x) (cdr y))))))]
            [%do-wind (lambda (new)
                        (unless (eq? new %wind-stack) 
                          (let ([tail (common-tail new %wind-stack)])
                            (begin
                              (let f ((l %wind-stack))
                                (unless (eq? l tail)
                                  (begin
                                    (set! %wind-stack (cdr l))
                                    ((cdr (car l)))
                                    (f (cdr l)))))
                              (let f ([l new])
                                (unless (eq? l tail)
                                  (begin
                                    (f (cdr l))
                                    ((car (car l)))
                                    (set! %wind-stack l))))))))]
            [%dynamic-wind (lambda  (pre body post)
                             (begin
                               (pre)
                               (set! %wind-stack (cons (cons pre post) %wind-stack))
                               (let ([v (body)])
                                 (begin
                                   (set! %wind-stack (cdr %wind-stack))
                                   (post)
                                   v))))]
            [%raise-handler '()])
       ,e))
  ; wrap e in special functions or definitions we need
  ; and then pass it into a helper function t-desugar that 
  ; performs the case-by-case translation recursively
  (t-desugar (wrap-with-lib e)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The output of assignment 2:
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (set! x e)
;     | (call/cc e)
;     | x
;     | (quote dat)


(define (assignment-convert e)
  ; Suggestion: compute a set of mutated variables and save here.
  ; I.e., a set of all x where there exists a (set! x ...).
  (define mutated-vars (set->list (set)))
  ; A strategy like this can help you to avoid boxing variables that don't need to be boxed
  ; and slowing down compiled programs as a result.
  (define (search-vars e)
    (match e
      [`(let ([,xs ,es] ...) ,e1)
       (map search-vars (cons e1 es))]
      [`(lambda (,x ...) ,e)
       (search-vars e)]
      [`(lambda ,x ,e)
       (search-vars e)]
      [`(apply ,e0 ,e1)
       (map search-vars (list e0 e1))]
      [`(prim ,op ,es ...)
       (map search-vars es)]
      [`(apply-prim ,op e)
       (search-vars e)]
      [`(if ,e0, e1, e2)
       (search-vars (list e0 e1 e2))]
      [`(call/cc ,e)
       (search-vars e)]
      [`(set! ,x ,e0)
       (set! mutated-vars (set-add mutated-vars x))
       (search-vars e0)]
      [`(quote ,(? datum? d))
       d]
      [(? symbol? x)
       x]
      [`(,es ...)
       (map search-vars es)]))
  
  (define (box-let x e)
    `[,x (prim make-vector '1 ,(boxify e))])
  
  (define (box-lambda x)
    `[,x (prim make-vector '1 ,x)])
  
  (define (box-check xs)
    (foldl (lambda (x a)
             (if (set-member? mutated-vars x)
                 #t
                 (or #f a))) #f xs))
  
  (define (boxify e)
    (match e
      ; box mutated vars at initialization,
      ; e.g., (let ([x '()]) ...) -> (let ([x (prim make-vector '1 '())]) ...)
      ; What happens to (lambda (x) ...) if x is mutated?
           
      ; .. all all other forms in the language ...
      [`(let ([,xs ,es] ...) ,e1)
       `(let ,(map (lambda (x e)
                     (if (set-member? mutated-vars x)
                         (box-let x e)
                         `(,x ,(boxify e))))
                   xs es) ,(boxify e1))]

      [`(lambda (,x ...) ,e)
       `(lambda ,x
          ,(if
            (box-check x)
            `(let
                 (,@(foldl
                     (lambda (x a)
                       (if (set-member? mutated-vars x)
                           (cons (box-lambda x) a)
                           a)) `() x)) ,(boxify e))
            (boxify e)))]

      [`(lambda ,x ,e)
       `(lambda ,x
          ,(if (box-check (list x))
               `(let (,(box-lambda x))
                  ,(boxify e))
               (boxify e)))]

      [`(apply ,e0 ,e1)
       `(apply ,(boxify e0) ,(boxify e1))]

      [`(prim ,op ,es ...)
       `(prim ,op ,@(map boxify es))]

      [`(apply-prim ,op e)
       `(apply-prim ,op ,(boxify e))]

      [`(if ,e0, e1, e2)
       `(if ,(boxify e0) ,(boxify e1) ,(boxify e2))]

      [`(call/cc ,e)
       `(call/cc ,(boxify e))]
      
      [`(set! ,x ,e0)
       `(prim vector-set! ,x '0 ,(boxify e0))]

      [`(quote ,(? datum? d))
       `(quote ,d)]
      
      [(? symbol? x)
       (if (set-member? mutated-vars x)
           `(prim vector-ref ,x '0)
           x)]

      [`(,es ...)
       (map boxify es)]))
  (define e1 (alphatize e))
  (search-vars e1)
  (boxify e1))


; assignment-convert => 

;;; set! is removed and replaced with vector-set!
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (call/cc e)
;     | x
;     | (quote dat)

; alphatize both takes and produces this language as well

(define (alphatize e)
  ; Defining curried rename function is convenient for mapping over lists of es
  (define ((rename env) e)
    (match e
      ; Rename all variables
      [`(let ([,xs ,es] ...) ,e1)
       (define xs+ (map gensym xs))
       (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
       `(let ,(map (lambda (x e)
                     `(,x ,((rename env) e)))
                   xs+ es)
          ,((rename env+) e1))]
      
      [`(lambda (,xs ...) ,e0)
       (define xs+ (map gensym xs))
       (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
       `(lambda ,xs+ ,((rename env+) e0))]

      [`(lambda ,x ,e)
       (define x+ (gensym x))
       `(lambda ,x+
          ,((rename (hash-set env x x+)) e))]

      [`(apply ,e0 ,e1)
       `(apply ,((rename env) e0) ,((rename env) e1))]

      [`(prim ,op ,es ...)
       `(prim ,op ,@(map (rename env) es))]

      [`(apply-prim ,op ,e)
       `(apply-prim ,op ,((rename env) e))]

      [`(if ,e0 ,e1 ,e2)
       `(if ,((rename env) e0) ,((rename env) e1) ,((rename env) e2))]

      [`(call/cc ,e)
       `(call/cc ,((rename env) e))]

      [(? symbol? x)
       (hash-ref env x)]

      [`(quote ,(? datum? d))
       `(quote ,d)]

      [`(set! ,x ,e0)
       `(set! ,((rename env) x) ,((rename env) e0))]

      [`(,es ...)
       (map (rename env) es)]
      ; etc ...
      ))
  ((rename (hash)) e))


; Converts to ANF; adapted from Flanagan et al.
(define (anf-convert e)
  (propagate-xy (normalize e (lambda (x) x))))



(define (normalize e k)
  (define (normalize-ae e k)
    (normalize e (lambda (anf)
                   (match anf
                     [`(lambda ,xs ,e0)
                      (k `(lambda ,xs ,e0))]
                     [`',dat (k `',dat)]
                     [(? symbol? x) (k x)]
                     [else
                      (let ([x (gensym 'a)])
                        `(let ([,x ,anf]) ,(k x)))]))))
  (define (normalize-aes es k)
    (if (null? es)
        (k '())
        (normalize-ae (car es)
                      (lambda (x) (normalize-aes (cdr es)
                                                 (lambda (xs) (k `(,x . ,xs))))))))
  (match e
    [`',dat (k `',dat)]
    [(? symbol? x) (k x)]
    [`(lambda ,xs ,e0)
     (k `(lambda ,xs ,(anf-convert e0)))]
    [`(let () ,e0)
     (normalize e0 k)]
    [`(let ([,x ,rhs] . ,rest) ,e0)
     (k `(let ([,x ,(anf-convert rhs)])
           ,(anf-convert
             `(let ,rest ,e0))))]
    [`(if ,ec ,et ,ef)
     (normalize-ae ec
                   (lambda (xc)
                     (k `(if ,xc
                             ,(anf-convert et)
                             ,(anf-convert ef)))))]
    [`(prim ,op ,es ...)
     (normalize-aes es
                    (lambda (xs)
                      (k `(prim ,op . ,xs))))]
    [`(apply-prim ,op ,e0)
     (normalize-ae e0
                   (lambda (x)
                     (k `(apply-prim ,op ,x))))]
    [`(call/cc ,e0)
     (normalize-ae e0
                   (lambda (x)
                     (k `(call/cc ,x))))]
    [`(apply ,es ...)
     (normalize-aes es
                    (lambda (xs)
                      (k `(apply . ,xs))))]
    [`(,es ...)
     (normalize-aes es k)]))


(define (propagate-xy e)
  (define ((prop env) e)
    (match e
      [`',dat `',dat]
      [(? symbol? x)
       (hash-ref env x (lambda () x))]
      [`(let ([,x ,(? symbol? y)]) ,e0)
       ((prop (hash-set env x (hash-ref env y (lambda () y)))) e0)]
      [`(let ([,x ,e0]) ,e1)
       `(let ([,x ,((prop env) e0)]) ,((prop env) e1))]
      [`(lambda ,xs ,e0)
       `(lambda ,xs ,((prop env) e0))]
      [`(if ,es ...)
       `(if . ,(map (prop env) es))]
      [`(prim ,op ,es ...)
       `(prim ,op . ,(map (prop env) es))]
      [`(apply-prim ,op ,e0)
       `(apply-prim ,op ,((prop env) e0))]
      [`(call/cc ,e0)
       `(call/cc ,((prop env) e0))]
      [`(apply ,es ...)
       `(apply . ,(map (prop env) es))]
      [`(,es ...)
       (map (prop env) es)]))
  ((prop (hash)) e))


; anf-convert =>

; e ::= (let ([x e]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (prim op ae ...)
;     | (apply-prim op ae)
;     | (if ae e e)
;     | (call/cc ae)
;     | ae
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)


(define (cps-convert e)
  (define (T-ae ae)
    (match ae
      [`(lambda (,xs ...) ,e0)
       (define cx (gensym 'cont))
       `(lambda (,cx ,@xs)
          ,(T e0 cx))]
      [`(lambda ,x ,e0)
       (define cx (gensym 'cont))
       (define x+ (gensym x))
       `(lambda ,x+
          (let ([,cx (prim car ,x+)])
            (let ([,x (prim cdr ,x+)]) 
              ,(T e0 cx))))]
      [(? symbol? x)
       x]
      [`',dat
       `',dat]))
  (define (T e cae)
    (match e
      ; return (call continuation)
      [(? symbol? x)
       `(,cae '0 ,x)]
      [`',dat
       `(,cae '0 ',dat)]
      [`(lambda . ,rest)
       `(,cae '0 ,(T-ae e))]
      ; prim ops
      [`(prim ,op ,aes ...)
       (define retx (gensym 'retprim))
       (T `(let ([,retx (prim ,op ,@aes)]) ,retx) cae)]
      [`(apply-prim ,op ,ae)
       (define retx (gensym 'retprim))
       (T `(let ([,retx (apply-prim ,op ,ae)]) ,retx) cae)]
      [`(let ([,x (apply-prim ,op ,ae)]) ,e0)
       `(let ([,x (apply-prim ,op ,(T-ae ae))])
          ,(T e0 cae))]
      [`(let ([,x (prim ,op ,aes ...)]) ,e0)
       `(let ([,x (prim ,op ,@(map T-ae aes))])
          ,(T e0 cae))]
      [`(let ([,x (lambda ,xs ,elam)]) ,e0)
       `(let ([,x ,(T-ae `(lambda ,xs ,elam))])
          ,(T e0 cae))]
      [`(let ([,x ',dat]) ,e0)
       `(let ([,x ',dat])
          ,(T e0 cae))]
      ; let -> continuation
      [`(let ([,x ,rhs]) ,e0)
       (define _x (gensym '_))
       (T rhs `(lambda (,_x ,x)
                 ,(T e0 cae)))]
      ; walk if, desugar call/cc, apply function 
      [`(if ,ae ,e0 ,e1)
       `(if ,(T-ae ae) ,(T e0 cae) ,(T e1 cae))]
      [`(call/cc ,ae)
       `(,(T-ae ae) ,cae ,cae)]
      [`(apply ,ae0 ,ae1)
       (define xlst (gensym 'cps-lst))
       `(let ([,xlst (prim cons ,cae ,(T-ae ae1))])
          (apply ,(T-ae ae0) ,xlst))]
      [`(,fae ,args ...)
       `(,(T-ae fae) ,cae ,@(map T-ae args))]))
  (T e '(lambda (_0 x) (let ([_1 (prim halt x)]) (_1 _1)))))


; cps-convert => 

; e ::= (let ([x (apply-prim op ae)]) e)
;     | (let ([x (prim op ae ...)]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (if ae e e)
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Pass that removes lambdas and datums as atomic and forces them to be let-bound
;   ...also performs a few small optimizations
(define (simplify-ae e)
  (define (wrap-aes aes wrap)
    (match-define (cons xs wrap+)
      (foldr (lambda (ae xs+wrap)
               (define gx (gensym 'arg))
               (if (symbol? ae)
                   (cons (cons ae (car xs+wrap))
                         (cdr xs+wrap))
                   (cons (cons gx (car xs+wrap))
                         (lambda (e)
                           (match ae
                             [`(lambda ,xs ,body) 
                              `(let ([,gx (lambda ,xs ,(simplify-ae body))])
                                 ,((cdr xs+wrap) e))]
                             [`',dat
                              `(let ([,gx ',dat])
                                 ,((cdr xs+wrap) e))])))))
             (cons '() wrap)
             aes))
    (wrap+ xs))
  (match e
    [`(let ([,x (lambda ,xs ,elam)]) ,e0)
     `(let ([,x (lambda ,xs ,(simplify-ae elam))]) ,(simplify-ae e0))]

    [`(let ([,x ',dat]) ,e0)
     `(let ([,x ',dat]) ,(simplify-ae e0))]

    [`(let ([,x (prim ,op ,aes ...)]) ,e0)
     (wrap-aes aes (lambda (xs) `(let ([,x (prim ,op ,@xs)]) ,(simplify-ae e0))))]
    [`(let ([,x (apply-prim ,op ,aes ...)]) ,e0)
     (wrap-aes aes (lambda (xs) `(let ([,x (apply-prim ,op ,@xs)]) ,(simplify-ae e0))))]

    [`(if (lambda . ,_) ,et ,ef)
     (simplify-ae et)]
    [`(if '#f ,et ,ef)
     (simplify-ae ef)]
    [`(if ',dat ,et ,ef)
     (simplify-ae et)]
    [`(if ,(? symbol? x) ,et ,ef)
     `(if ,x ,(simplify-ae et) ,(simplify-ae ef))]

    [`(apply ,ae0 ,ae1)
     (wrap-aes (list ae0) (lambda (x) `(apply ,@x ,ae1)))]
         
    [`(,aes ...)
     (wrap-aes aes (lambda (xs) xs))]))


; Helper to remove vararg lambdas/callsites
(define (remove-varargs e)
  ;(display "Matched: ")
  ;(display e)
  (match e
    [`(let ([,x ',dat]) ,e0)
     `(let ([,x ',dat]) ,(remove-varargs e0))]
    [`(let ([,x (prim ,op ,xs ...)]) ,e0)
     `(let ([,x (prim ,op ,@xs)]) ,(remove-varargs e0))]
    [`(let ([,x (apply-prim ,op ,y)]) ,e0)
     `(let ([,x (apply-prim ,op ,y)]) ,(remove-varargs e0))]
    [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
     ; turns (xs ...) into x and immediately into (x)
     ; by adding the needed car/cdr calls and let bindings
     (define gx (gensym 'rvp))
     (define gx+e
       (foldr (lambda (x gx+e)
                (define gx (gensym 'rvp))
                (cons gx
                      `(let ([,x (prim car ,gx)])
                         (let ([,(car gx+e) (prim cdr ,gx)])
                           ,(cdr gx+e)))))
              (cons (gensym 'na) (remove-varargs body))
              xs))
     `(let ([,x (lambda (,(car gx+e)) ,(cdr gx+e))])
        ,(remove-varargs e0))]
    [`(let ([,x (lambda ,y ,body)]) ,e0)
     `(let ([,x (lambda (,y) ,(remove-varargs body))])
        ,(remove-varargs e0))]
    [`(if ,x ,e0 ,e1)
     `(if ,x ,(remove-varargs e0) ,(remove-varargs e1))]
    [`(apply ,f ,args)
     `(,f ,args)] 
    [`(,f ,xs ...)
     ; case not written
     (define (cons-list xs sym)
       (match xs
         [`(,h)
          (define x+ (gensym 'x))
          `(let ([,x+ (prim cons ,h ,sym)])
             (,f ,x+))]
         [`(,h . ,t)
          (define x+ (gensym 'x))
          `(let ([,x+ (prim cons ,h ,sym)])
             ,(cons-list t x+))]))
     (define sym+ (gensym 'sym))
     `(let ([,sym+ '()])
        ,(cons-list (reverse xs) sym+))]))


; call simplify-ae on input to closure convert, then remove vararg callsites/lambdas
(define (closure-convert cps)
  (define scps (simplify-ae cps))
  (define no-varargs-cps (remove-varargs scps))
  ; case not written; see our livecoding from class
  ; Exp x List[Proc] -> Exp x Set[Var] x List[Proc]
  (define (bottom-up e procs)
    (match e
      [`(let ([,x ',dat]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
         (bottom-up e0 procs))
       `((let ([,x ',dat]) ,e0+)
         ,(set-remove free+ x)
         ,procs+)]
      [`(let ([,x (prim ,op ,xs ...)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
         (bottom-up e0 procs))
       `((let ([,x (prim ,op ,@xs)]) ,e0+)
         ,(set-remove (set-union free+ (list->set xs)) x)
         ,procs+)]
      [`(let ([,x (apply-prim ,op ,xs ...)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
         (bottom-up e0 procs))
       `((let ([,x (apply-prim ,op ,@xs)]) ,e0+)
         ,(set-remove (set-union free+ (list->set xs)) x)
         ,procs+)]
      [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
       (match-define `(,e0+ ,free0+ ,procs0+)
         (bottom-up e0 procs))
       (match-define `(,body+ ,freelam+ ,procs1+)
         (bottom-up body procs0+))
       (define env-vars (foldl (lambda (x fr) (set-remove fr x))
                               freelam+
                               xs))
       (define ordered-env-vars (set->list env-vars))
       (define lamx (gensym 'lam))
       (define envx (gensym 'env))
       (define body++ (cdr (foldl (lambda (x count+body)
                                    (match-define (cons cnt bdy) count+body)
                                    (cons (+ 1 cnt)
                                          `(let ([,x (env-ref ,envx ,cnt)])
                                             ,bdy)))
                                  (cons 1 body+)
                                  ordered-env-vars)))
       `((let ([,x (make-closure ,lamx ,@ordered-env-vars)]) ,e0+)
         ,(set-remove (set-union free0+ env-vars) x)
         ((proc (,lamx ,envx ,@xs) ,body++) . ,procs1+))]
      [`(if ,(? symbol? x) ,e0 ,e1)
       (match-define `(,e0+ ,free0+ ,procs0+)
         (bottom-up e0 procs))
       (match-define `(,e1+ ,free1+ ,procs1+)
         (bottom-up e1 procs0+))
       `((if ,x ,e0+ ,e1+)
         ,(set-union free1+ free0+ (set x))
         ,procs1+)]
      [`(,(? symbol? xs) ...)
       `((clo-app ,@xs)
         ,(list->set xs)
         ,procs)]))
  (match-define `(,main-body ,free ,procs) (bottom-up no-varargs-cps '()))
  `((proc (main) ,main-body) . ,procs))


; Walk procedures and emit llvm code as a string
; (string-append "  %r0 = opcode i64 %a, %b \n"
;                "  %r1 = ... \n")
(define (proc->llvm proc)
  (define global-strings '())
  (define function-args (make-hash))
  (define (llvm-convert proc)
    (match-define `(proc (,xs ...) ,e) proc)
    (hash-set! function-args (car xs) (sub1 (sub1 (length xs))))
    (define (llvm-exp e)
      (match e
        [`(let ([,x0 (apply-prim ,op ,x1)]) ,e0)
         (~a "  %" (c-name x0) " = call i64 @" (prim-applyname op) "(i64 %" x1 ")\n" (llvm-exp e0))]
        
        [`(let ([,x (prim ,op ,xs0 ...)]) ,e0)
         (~a "  %" (c-name x) " = call i64 @" (prim-name op) "("
             (if (empty? xs0)
                 ""
                 (foldl (lambda (x a) (~a a ", i64 %" (c-name x))) (~a "i64 %" (c-name (car xs0))) (cdr xs0)))
             ")\n"
             (llvm-exp e0))]
        
        [`(let ([,x (make-closure ,xs0 ...)]) ,e0)
         (define cloptr (gensym 'cloptr))
         (define s1 (~a "  %" cloptr " = call i64* @alloc(i64 " (* (length xs0) 8) ")\n"))
         (define num-vars (build-list (sub1 (length xs0)) (lambda (x) (+ x 1))))
         (define s2s (foldl (lambda (x n a)
                              (define eptr (gensym 'eptr))
                              (define s2p1 (~a "  %" eptr " = getelementptr inbounds i64, i64* %" cloptr ", i64 " n "\n"))
                              (define s2p2 (~a "  store i64 %" (c-name x) ", i64* %" eptr "\n"))
                              (~a a s2p1 s2p2))
                            ""
                            (cdr xs0)
                            num-vars))
         (define feptr (gensym 'eptr))
         (define s3p1 (~a "  %" feptr " = getelementptr inbounds i64, i64* %" cloptr ", i64 0\n"))
         (define f (gensym 'f))
         (define s3p2 (~a "  %" f " = ptrtoint void(i64" (foldl (lambda (n a)
                                                                  (~a ",i64" a))
                                                                ")* @"
                                                                (build-list (hash-ref function-args (car xs0)) values))
                          (car xs0) " to i64\n"))
         (define s3p3 (~a "  store i64 %" f ", i64* %" feptr "\n"))
         (define s3p4 (~a "  %" (c-name x) " = ptrtoint i64* %" cloptr " to i64\n"))
         (~a s1 s2s s3p1 s3p2 s3p3 s3p4 (llvm-exp e0))]
        [`(let ([,x0 (env-ref ,x1 ,nat)]) ,e0)
         (define envptr1 (gensym 'envptr))
         (define envptr2 (gensym 'envptr))
         (define s1 (~a "  %" envptr1 " = inttoptr i64 %" (c-name (car (cdr xs))) " to i64*"))
         (define s2 (~a "  %" envptr2 " = getelementptr inbounds i64, i64* %" envptr1 ", i64 " nat))  
         (define s3 (~a "  %" (c-name x0) " = load i64, i64* %" envptr2 ", align 8"))
         (~a s1 s2 s3 (llvm-exp e0) #:separator "\n")]
        
        [`(let ([,x (quote ,dat)]) ,e0)
         (define function (match dat
                            [(? integer?)
                             (~a "call i64 @const_init_int(i64 " dat ")")]
                            [(? string?)
                             (define str (gensym '.str.))
                             (set! global-strings (cons (list str dat) global-strings))
                             (~a "call i64 @const_init_string(i8* getelementptr inbounds (["
                                 (add1 (string-length dat)) " x i8], ["
                                 (add1 (string-length dat)) " x i8]* @"
                                 str ", i32 0, i32 0))")]
                            [(? boolean?)
                             (if dat
                                 "call i64 @const_init_true()"
                                 "call i64 @const_init_false()")]
                            [(? symbol?)
                             (define str (gensym '.str.))
                             (define dats (symbol->string dat))
                             (set! global-strings (cons (list str dats) global-strings))
                             (~a "call i64 @const_init_symbol(i8* getelementptr inbounds (["
                                 (add1 (string-length dats)) " x i8], ["
                                 (add1 (string-length dats)) " x i8]* @"
                                 str ", i32 0, i32 0))")]
                            [(? null?)
                             "call i64 @const_init_null()"]))
         (~a "  %" (c-name x) " = " function "\n" (llvm-exp e0))]
        
        [`(clo-app ,xs0 ...)
         (define cloptr (gensym 'cloptr))
         (define s1 (~a "  %" cloptr " = inttoptr i64 %" (c-name (car xs0)) " to i64*"))
         (define i0ptr (gensym 'i0ptr))
         (define s2 (~a "  %" i0ptr " = getelementptr inbounds i64, i64* %" cloptr ", i64 0"))
         (define f (gensym 'f))
         (define s3 (~a "  %" f " = load i64, i64* %" i0ptr ", align 8"))
         (define fptr (gensym 'fptr))
         (define num-vars (build-list (sub1 (length xs0)) (lambda (x) (+ x 1))))
         (define i64s (foldl (lambda (n a) (~a ",i64" a)) ")*" num-vars))
         (define s4 (~a "  %" fptr " = inttoptr i64 %" f " to void (i64" i64s))
         (define s5 (~a "  musttail call fastcc void %" fptr "("
                        (foldl (lambda (x a) (~a a ", i64 %" (c-name x))) (~a "i64 %" (c-name (car xs0))) (cdr xs0))
                        ")"))
         (~a s1 s2 s3 s4 s5 "  ret void\n" #:separator "\n")]
        
        [`(if ,x ,e0 ,e1)
         (define cmp (gensym 'cmp))
         (define global-false (gensym 'false))
         (define then (gensym 'then))
         (define else (gensym 'else))
         (define s0 (~a "  %" global-false " = call i64 @const_init_false()"))
         (define s1 (~a "  %" cmp " = icmp ne i64 %" (c-name x) ", %" global-false))
         (define s2 (~a "  br i1 %" cmp ", label %" then ", label %" else))
         (define s3 (~a then ":"))
         (define s4 (llvm-exp e0))
         (define s5 (~a else ":"))
         (define s6 (llvm-exp e1))
         (~a s0 s1 s2 s3 s4 s5 s6 "\n" #:separator "\n")]))
    (define function (llvm-exp e))                              
    (~a (match xs
          [`(main)
           "define void @proc_main() {\n"]
          [else
           (~a "define void @" (car xs) "("
               (foldl (lambda (x a) (~a a ", i64 %" x)) (~a "i64 %" (car (cdr xs))) (cdr (cdr xs)))
               ") {\n")])
        function
        "}\n"))
  (define functions (foldr (lambda (proc acc)
                             (~a acc (llvm-convert proc) "\n"))
                           (~a "define i32 @main() {\n" "  call fastcc void @proc_main()\n" "  ret i32 0\n" "}\n\n")
                           proc))
  (define globals (foldl (lambda (s a)
                           (~a a
                               "@"
                               (car s)
                               " = global ["
                               (+ (string-length (car (cdr s))) 1)
                               " x i8] c\"" (car (cdr s)) "\\00\", align 8\n"))
                         ""
                         global-strings))
  (~a globals functions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (top-level e)
  (define (pairs xs es) (map (lambda (x e0) `(,x ,(tl e0))) xs es))
  (define (case-clause cl) (map (lambda (x)
                                  (match x
                                    [`(else ,es ...)
                                     `(else ,(tl `(begin ,@es)))]
                                    [`(,(? datum? dat) ,es ...)
                                     `(,dat ,(tl `(begin ,@es)))])) cl))
  (define (cond-clause cl) (map (lambda (x)
                                  (match x
                                    [`(else ,es ...)
                                     `(else ,(tl `(begin ,@es)))]
                                    [`(,es ...)
                                     (if (empty? (cdr es))
                                         `(,(tl (car es)))
                                         `(,(tl (car es)) ,(tl `(begin ,@(cdr es)))))])) cl))
  (define (tl e)
    (match e
      [`(letrec* ([,xs ,es] ...) ,e0s ...)
       `(letrec* ,(pairs xs es) ,(tl `(begin ,@e0s)))]
      [`(letrec ([,xs ,es] ...) ,e0s ...)
       `(letrec ,(pairs xs es) ,(tl `(begin ,@e0s)))]
      [`(let* ([,xs ,es] ...) ,e0s ...)
       `(let* ,(pairs xs es) ,(tl `(begin ,@e0s)))]
      [`(let ([,xs ,es] ...) ,e0s ...)
       `(let ,(pairs xs es) ,(tl `(begin ,@e0s)))]
      [`(let ,x ([,xs ,es] ...) ,e0s ...)
       `(let ,x ,(pairs xs es) ,(tl `(begin ,@e0s)))]
      #;[`(lambda (,xs . ,l) ,es ...)
         `(lambda (,xs . ,l) ,(tl `(begin ,@es)))]
      [`(lambda ,x ,es ...)
       `(lambda ,x ,(tl `(begin ,@es)))]
      [`(lambda (,xs ... [,x0s ,e0s] ...) ,e1s ...)
       'var-lam]
      [`(dynamic-wind ,e0 ,e1 ,e2)
       `(dynamic-wind ,(tl e0) ,(tl e1) ,(tl e2))]
      [`(guard (,x ,clauses ...) ,e0s ...)
       `(guard (,x ,@(cond-clause clauses)) ,(tl `(begin ,@e0s)))]
      [`(raise ,e0)
       `(raise ,(tl e0))]
      [`(delay ,e0)
       `(delay ,(tl e0))]
      [`(force ,e0)
       `(force ,(tl e0))]
      [`(and ,es ...)
       `(and ,@(map tl es))]
      [`(or ,es ...)
       `(or ,@(map tl es))]
      [`(match ,e ,clauses ...)
       'match]
      [`(cond ,clauses ...)
       `(cond ,@(cond-clause clauses))]
      [`(case ,e0 ,clauses ...)
       `(case ,(tl e0) ,@(case-clause clauses))]
      [`(if ,e0 ,e1 ,e2)
       `(if ,(tl e0) ,(tl e1) ,(tl e2))]
      [`(when ,e0 ,es ...)
       `(when ,(tl e0) ,@(map tl es))]
      [`(unless ,e0 ,es ...)
       `(unless ,(tl e0) ,@(map tl es))]
      [`(set! ,x ,e0)
       `(set! ,x ,(tl e0))]
      [`(begin ,es ...)
       (define (find-defs es)
         (foldl
          (lambda (e0 a)
            (match e0
              #;[`(define (,f ,xs ...) ,e0s ...)
                 (list (cons `(,f (lambda ,xs ,(tl `(begin ,@e0s)))) (car a)) (car (cdr a)))]
              [`(define (,f ,xs ... . ,l) ,e0s ...)
               (list (cons `(,f (lambda (,@xs . ,l) ,(tl `(begin ,@e0s)))) (car a)) (car (cdr a)))]
              [`(define ,x ,e0)
               (list (cons `(,x ,(tl e0)) (car a)) (car (cdr a)))]
              [`(set! ,x ,e0)
               (list (cons `(,(gensym 'set-sym) (set! ,x ,(tl e0))) (car a)) (car (cdr a)))]
              [`(begin ,e0s ...)
               (define res (find-defs e0s))
               (list (append (car res) (car a)) (append (car (cdr res)) (car (cdr a))))]
              [else
               (list (car a) (cons (tl e0) (car (cdr a))))]))
          (list (list) (list))
          es))
       (define defs-es (find-defs es))
       `(begin (letrec* ,(reverse (car defs-es)) ,(if (empty? (car (cdr defs-es)))
                                                      `(void)
                                                      `(begin ,@(reverse (car (cdr defs-es)))))))]
      [`(call/cc ,e0)
       `(call/cc ,(tl e0))]
      [`(apply ,e0 ,e1)
       `(apply ,(tl e0) ,(tl e1))]
      [(? prim? op)
       op]
      [`(quasiquote ,es)
       (define (quasi es)
         (match es
           [(list 'unquote uq)
            (tl uq)]
           [`(,h . ,t)
            `(cons ,(quasi h) ,(quasi t))]
           [else
            `',es]))
       (quasi es)]
      [`(quote ,(? datum? dat))
       `(quote ,dat)]
      [(? symbol? x)
       x]
      [`(,es ...)
       (map tl es)]
      [(? datum? dat)
       `(quote ,dat)]))
  (tl e))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scm-llvm e)
  (define step 1)
  (define use-timer #f)
  (define time (current-inexact-milliseconds))
  (define (timer)
    (define now (current-inexact-milliseconds))
    (if use-timer
        (begin
          (print (~a "Step " step " completed in " (- now time)))
          (newline)
          (set! step (+ 1 step))
          (set! time now))
        (void)))

  (define top-level-e (top-level e))
  (timer) ;1
  ;(scheme-exp? top-level-e)
  ;(eval-scheme top-level-e)
  (define desugar-e (desugar top-level-e))
  (timer) ;2
  ;(ir-exp? desugar-e)
  ;(eval-ir desugar-e)
  (define simplify-ir-e (simplify-ir desugar-e))
  (timer) ;3
  (define assignment-convert-e (assignment-convert simplify-ir-e))
  (timer) ;4
  (define alphatize-e (alphatize assignment-convert-e))
  (timer) ;5
  ;(alphatized-exp? alphatize-e)
  ;(eval-ir alphatize-e)
  (define anf-e (anf-convert alphatize-e))
  (timer) ;6
  ;(anf-exp? anf-e)
  (define cps-e (cps-convert anf-e))
  (timer) ;7
  ;(cps-exp? cps-e)
  (define closure-e (closure-convert cps-e))
  (timer) ;8
  ;(proc-exp? closure-e)
  (define llvm (proc->llvm closure-e))
  (timer) ;9
  llvm)
