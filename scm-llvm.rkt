#lang racket

(provide scm-llvm)

(require "utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 2 (Taken from Thomas Gilray)
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
     (define check (gensym 'check))
     `(let ([,check ,(t-desugar `(procedure? ,e0))])
        (if ,check
            (apply ,(t-desugar e0) ,(t-desugar e1))
            (prim halt '"Error: Non-function value is applied.")))]
    
    [`(/ ,es ...)
     (define check (gensym 'check))
     `(let ([,check ,(t-desugar `(foldl (lambda (x a)
                                          (or (= x '0) a))
                                        '#f
                                        (cdr (list ,@es))))])
        (if ,check
            (prim halt (quote ,(~a "Error: Division by zero. Expression was '" e "'")))
            (prim / . ,(map t-desugar es))))]

    [`(,(? prim? op) ,es ...)
     `(prim ,op . ,(map t-desugar es))]

    [`(,e0 ,es ...)
     (define check (gensym 'check))
     `(let ([,check ,(t-desugar `(procedure? ,e0))])
        (if ,check
            ,(map t-desugar (cons e0 es))
            (prim halt (quote ,(~a "Error: Non-function value is applied. Expression was '" e "'")))))]

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
;; Assignment 3 (Partially taken from Thomas Gilray)
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
       (hash-ref env x `(prim halt (quote ,(~a "Error: Use of not-yet-initialized variable: " x))))]

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
;; Assignment 4 (Partially taken from Thomas Gilray)
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


(define optimize-varargs #f)

; Helper to remove vararg lambdas/callsites
(define (successors exp store) 
  (define (lookup x) (hash-ref store x set)) 
  (match exp
    [`(let ([,x (quote ,dat)]) ,e0)
     (cons (set e0) store)]
    [`(let ([,x (lambda ,xs ,elam)]) ,e0)
     (cons (set e0) (store-extend store x `(lambda ,xs ,elam)))]
    [`(let ([,x (prim vector ,as ...)]) ,e0)
     (cons (set e0) (store-extend store x (set-union (set x) (foldl set-union (set) (map lookup as)))))]
    [`(let ([,x (apply-prim vector ,args)]) ,e0)
     (cons (set e0) (store-extend store x (set-union (set x) (lookup args))))]
    [`(let ([,x (prim make-vector ,a ,b)]) ,e0)
     (cons (set e0) (store-extend store x (set-union (set x) (lookup a) (lookup b))))]
    [`(let ([,x (apply-prim make-vector ,args)]) ,e0)
     (cons (set e0) (store-extend store x (set-union (set x) (lookup args))))]
    [`(let ([,x (prim vector-set! ,a ,b ,c)]) ,e0)
     (cons (set e0) (foldl (lambda (a store) (store-extend store a (lookup c)))
                           store
                           (filter symbol? (set->list (lookup a)))))]
    [`(let ([,x (apply-prim vector-set! ,a)]) ,e0)
     (cons (set e0) (foldl (lambda (a store) (store-extend store a (lookup a)))
                           store
                           (filter symbol? (set->list (lookup a)))))]
    [`(let ([,x (prim vector-ref ,a ,bs ...)]) ,e0)
     (cons (set e0) (store-extend store x (foldl (lambda (a vs)
                                                   (set-union vs (lookup a)))
                                                 (set)
                                                 (filter symbol? (set->list (lookup a))))))]
    [`(let ([,x (apply-prim vector-ref ,args)]) ,e0)
     (cons (set e0) (store-extend store x (foldl (lambda (a vs)
                                                   (set-union vs (lookup a)))
                                                 (set)
                                                 (filter symbol? (set->list (lookup args))))))]
    [`(let ([,x (prim ,op ,as ...)]) ,e0)
     (cons (set e0) (store-extend store x (foldl set-union (set) (map lookup as))))]
    [`(let ([,x (apply-prim ,op ,a)]) ,e0)
     (cons (set e0) (store-extend store x (lookup a)))]
    [`(if ,ae ,e0 ,e1)
     (cons (set e0 e1) store)]
    [`(apply ,f ,a)
     (define vs (lookup a))
     (foldl (lambda (lam succs+store)
              (match-define (cons succs store) succs+store)
              (match lam
                [`(lambda (,xs ...) ,e0)
                 (cons (set-add succs e0)
                       (foldl (lambda (x store)
                                (store-extend store x vs))
                              store
                              xs))]
                [`(lambda ,(? symbol? x) ,e0)
                 (cons (set-add succs e0)
                       (store-extend store x vs))]
                [else succs+store]))
            (cons (set) store)
            (set->list (lookup f)))]
    [`(,f ,as ...) 
     (foldl (lambda (lam succs+store)
              (match-define (cons succs store) succs+store)
              (match lam
                [`(lambda (,xs ...) ,e0)
                 #:when (= (length xs) (length as))
                 (cons (set-add succs e0)
                       (foldl (lambda (x a store)
                                (store-extend store x (lookup a)))
                              store
                              xs
                              as))]
                [`(lambda ,(? symbol? x) ,e0)
                 (cons (set-add succs e0)
                       (foldl (lambda (a store)
                                (store-extend store x (lookup a)))
                              store
                              as))]
                [else succs+store]))
            (cons (set) store)
            (set->list (lookup f)))]))


; utility functions
(define (graph-extend graph e0 succs)
  (foldl (lambda (succ graph)
           (graph-extend graph succ (set)))
         (hash-set graph e0 (set-union succs (hash-ref graph e0 set)))
         (set->list succs)))
(define (store-extend store x val)
  (hash-set store x (set-union (store-ref store x) (if (set? val) val (set val)))))
(define (store-ref store x)
  (hash-ref store x set))


; Run 0-CFA analysis (times out and returns (cons #f #f) if too expensive)
(define (0-cfa exp)
  (define start-ms (current-milliseconds))
  (let loop ([graph (hash exp (set))]
             [store (hash)])
    (match-define (cons graph+ store+)
      (foldl (lambda (e0 graph+store)
               (match-define (cons graph store) graph+store)
               (match-define (cons succs store+)
                 (successors e0 store))
               (cons (graph-extend graph e0 succs)
                     store+))
             (cons graph store)
             (hash-keys graph)))
    (if (and (equal? graph graph+)
             (equal? store store+))
        (cons graph+ store+)
        (if (> (- (current-milliseconds) start-ms) 8000)
            (begin #;(pretty-print '0-cfa-timed-out)
                   (cons #f #f))
            (loop graph+ store+)))))



; computes a mapping from callsites or lambdas to the corresponding lambdas or callsites (respectively)
(define (callsites-lambdas cfg store)
  (define calls-to-lambdas
    (foldl (lambda (e mp)
             (match e
               [`(apply ,f ,args)
                (hash-set mp e (list->set (filter (not/c symbol?) (set->list (hash-ref store f set)))))]
               [`(,(? (and/c symbol? (not/c reserved?)) f) ,(? symbol? xs) ...)
                (hash-set mp e (list->set (filter (not/c symbol?) (set->list (hash-ref store f set)))))]
               [else mp]))
           (hash)
           (hash-keys cfg)))
  ; union calls-to-lambdas with the reverse of itself (lambdas-to-callsites)
  (foldl (lambda (call mp)
           (foldl (lambda (lam mp)
                    (hash-set mp lam (set-add (hash-ref mp lam set) call)))
                  mp
                  (set->list (hash-ref mp call set))))
         calls-to-lambdas
         (hash-keys calls-to-lambdas)))


(define (shallow e [n 2])
  (match e
    [`(,es ...) #:when (<= n 0) '...]
    [`(,es ...) (map (lambda (e) (shallow e (- n 1))) es)]
    [else e]))


; Computes a set of lambdas/callsites to be converted to use variable arguments
; (using the output of callsites-lambdas)
(define (compute-vararg-set clm)
  (define (iterate-set st)
    (foldl (lambda (call/lam st)
             (match call/lam
               [(or `(,(? (and/c symbol? (not/c reserved?)) _) ,(? symbol? xs) ...)
                    `(lambda (,xs ...) ,_))
                (define lst (set->list (hash-ref clm call/lam set)))
                (if (< 1 (set-count (foldl set-union
                                           (set (length xs))
                                           (map (match-lambda
                                                  [(? ((curry set-member?) st)) (set 0 1)]
                                                  [`(lambda (,ys ...) ,_) (set (length ys))]
                                                  [`(lambda ,y ,_) (set 0 1)]
                                                  [`(apply ,f ,y) (set 0 1)]
                                                  [`(,f ,ys ...) (set (length ys))])
                                                lst))))
                    (set-add st call/lam)
                    st)]
               [(or `(apply ,_ ,_)
                    `(lambda ,_ ,_))
                (set-union st (list->set (filter (match-lambda
                                                   [`(lambda ,(? symbol? x) ,body) #f]
                                                   [`(apply ,f ,x) #f]
                                                   [else #t])
                                                 (set->list (hash-ref clm call/lam set)))))]))
           st
           (hash-keys clm)))
  (define (fixpoint st)
    (define st+ (iterate-set st))
    (if (equal? st st+)
        st
        (fixpoint st+)))
  (fixpoint (set)))


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
     (define num 0)
     (define gx+e
       (foldr (lambda (x gx+e)
                (define gx (gensym 'rvp))
                (define check (gensym 'check))
                (define empty (gensym 'empty))
                (define abort (gensym 'abort))
                (define fail (gensym 'fail))
                (define msg (gensym 'msg))
                (define many (gensym 'many))
                (define rest (gensym 'rest))
                (set! num (add1 num))
                (cons gx
                      #;`(let ([,rest (prim cdr ,gx)])
                           (let ([,check (prim null? ,rest)])
                             (if ,check
                                 ,(cdr gx+e)
                                 ,(remove-varargs
                                   `(let ([,msg (quote ,(~a "Error: Too many arguments given."))])
                                      (let ([,abort (prim halt ,msg)])
                                        (,abort)))))))
                      `(let ([,check (prim null? ,gx)])  
                         (if ,check
                             ,(remove-varargs
                               `(let ([,msg (quote ,(~a "Error: Too few arguments given, missing: " num))])
                                  (let ([,abort (prim halt ,msg)])
                                    (,abort))))
                             ,(if (= num 1)
                                  `(let ([,rest (prim cdr ,gx)])
                                     (let ([,empty (prim null? ,rest)])
                                       (if ,empty
                                           (let ([,x (prim car ,gx)])  
                                             (let ([,(car gx+e) (prim cdr ,gx)])
                                               ,(cdr gx+e)))
                                           ,(remove-varargs
                                             `(let ([,many (quote ,(~a "Error: Too many arguments given."))])
                                                (let ([,fail (prim halt ,many)])
                                                  (,fail)))))))
                                  `(let ([,x (prim car ,gx)])  
                                     (let ([,(car gx+e) (prim cdr ,gx)])
                                       ,(cdr gx+e))))))))
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
         [`()
          `(,f ,sym)]
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


(define (T-bottom-up e procs)
  (match e
    [`(let ([,x ',dat]) ,e0)
     (match-define `(,freevars ,e0+ ,procs+) (T-bottom-up e0 procs))
     (define dx (gensym 'd))
     (list (set-remove freevars x)
           `(let ([,x ',dat]) ,e0+)
           procs+)]
    [`(let ([,x (prim ,op ,xs ...)]) ,e0)
     (match-define `(,freevars ,e0+ ,procs+) (T-bottom-up e0 procs))
     (list (set-remove (set-union (list->set xs) freevars) x)
           `(let ([,x (prim ,op ,@xs)]) ,e0+)
           procs+)]
    [`(let ([,x (apply-prim ,op ,y)]) ,e0)
     (match-define `(,freevars ,e0+ ,procs+) (T-bottom-up e0 procs))
     (list (set-remove (set-add freevars y) x)
           `(let ([,x (apply-prim ,op ,y)]) ,e0+)
           procs+)]
    [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
     (match-define `(,freevars ,e0+ ,procs0+) (T-bottom-up e0 procs))
     (match-define `(,freelambda ,body+ ,procs1+) (T-bottom-up body procs0+))
     (define fx (gensym 'lam))
     (define envx (gensym 'env))
     (define envvars (foldl (lambda (x fr) (set-remove fr x))
                            freelambda
                            xs))
     (define envlist (set->list envvars))
     (define body++ (cdr (foldl (lambda (x count+bdy)
                                  (cons (+ 1 (car count+bdy))
                                        `(let ([,x (env-ref ,envx ,(car count+bdy))])
                                           ,(cdr count+bdy))))
                                (cons 1 body+)
                                envlist)))
     (list (set-remove (set-union envvars freevars) x)
           `(let ([,x (make-closure ,fx ,@envlist)]) ,e0+)
           `((proc (,fx ,envx ,@xs) ,body++) . ,procs1+))]
    [`(if ,x ,e0 ,e1)
     (match-define `(,freevars0 ,e0+ ,procs0+) (T-bottom-up e0 procs))
     (match-define `(,freevars1 ,e1+ ,procs1+) (T-bottom-up e1 procs0+))
     (list (set-add (set-union freevars0 freevars1) x)
           `(if ,x ,e0+ ,e1+)
           procs1+)]
    [`(,f ,xs ...)
     (list (list->set `(,f ,@xs)) 
           `(clo-app ,f ,@xs)
           procs)]))


; call simplify-ae on input to closure convert, then cfa-0, then walk ast
(define (closure-convert cps)
  (define scps (simplify-ae cps))
  (match-define (cons cfg store) (0-cfa scps))
  (if (and optimize-varargs cfg store)
      (let () 
        (define call-lam-map (callsites-lambdas cfg store))
        (define vararg-set (compute-vararg-set call-lam-map))
        ;(pretty-print `(vararg-set-size ,(set-count vararg-set)))
        (define no-varargs-cps (remove-varargs scps vararg-set))
        (match-define `(,freevars ,main-body ,procs) (T-bottom-up no-varargs-cps '()))
        `((proc (main) ,main-body) . ,procs))
      (let () 
        (define no-varargs-cps (remove-varargs scps))
        (match-define `(,freevars ,main-body ,procs) (T-bottom-up no-varargs-cps '()))
        `((proc (main) ,main-body) . ,procs))))

; Walk procedures and emit llvm code as a string
; (string-append "  %r0 = opcode i64 %a, %b \n"
;                "  %r1 = ... \n")
(define (proc->llvm procs)
  (define globals "")
  (define (s-> s) (c-name s))

  (define (comment-line . strs)
    (match-define `(,strs+ ... ,cmt) strs)
    (define line (apply string-append strs+))
    (define extline (string-append line (let loop ([s ""]
                                                   [i (string-length line)])
                                          (if (>= i 85) s (loop (string-append " " s) (+ i 1))))))
    (string-append extline "; " cmt "\n"))

  (define (e->llvm e)
    (match e
      [`(let ([,x '#t]) ,e0)
       (string-append
        (comment-line
         "  %" (s-> x) " = call i64 @const_init_true()"
         "quoted #t")
        (e->llvm e0))]
      [`(let ([,x '#f]) ,e0)
       (string-append
        (comment-line
         "  %" (s-> x) " = call i64 @const_init_false()"
         "quoted #f")
        (e->llvm e0))]
      [`(let ([,x '()]) ,e0)
       (string-append
        (comment-line
         "  %" (s-> x) " = add i64 0, 0"
         "quoted ()")
        (e->llvm e0))]
      [`(let ([,x ',(? integer? dat)]) ,e0)
       (string-append
        (comment-line
         "  %" (s-> x) " = call i64 @const_init_int(i64 " (number->string dat) ")"
         "quoted int")
        (e->llvm e0))]
      [`(let ([,x ',(? string? dat)]) ,e0)
       (define dx (gensym 'str))
       (define lenstr (string-append "[" (number->string (+ 1 (string-length dat))) " x i8]"))
       (set! globals
             (string-append globals
                            "@" (s-> dx) " = private unnamed_addr constant "
                            lenstr " c\"" dat "\\00\", align 8\n"))
       (string-append
        (comment-line
         "  %" (s-> x) " = call i64 @const_init_string(i8* getelementptr inbounds (" lenstr ", " lenstr "* @" (s-> dx) ", i32 0, i32 0))"
         "quoted string")
        (e->llvm e0))]
      [`(let ([,x ',(? symbol? dat)]) ,e0)
       (define dx (gensym 'sym))
       (define lenstr (string-append "[" (number->string (+ 1 (string-length (symbol->string dat)))) " x i8]"))
       (set! globals
             (string-append globals
                            "@" (s-> dx) " = private unnamed_addr constant "
                            lenstr " c\"" (symbol->string dat) "\\00\", align 8\n"))
       (string-append
        (comment-line
         "  %" (s-> x) " = call i64 @const_init_symbol(i8* getelementptr inbounds (" lenstr ", " lenstr "* @" (s-> dx) ", i32 0, i32 0))"
         "quoted string")
        (e->llvm e0))]
      [`(let ([,x (make-closure ,lamx ,xs ...)]) ,e0)
       (define cptr (gensym 'cloptr))
       (define fptrptr (gensym 'eptr))
       (define eptrs (map (lambda (x) (gensym 'eptr)) xs))
       (define n (match (car (filter (match-lambda [`(proc (,f . ,_) . ,_) (eq? f lamx)]) procs))
                   [`(proc (,lamx ,ys ...) . ,_) (length ys)]))
       (define f (gensym 'f))
       (apply string-append
              `(,(comment-line
                  "  %" (s-> cptr) " = call i64* @alloc(i64 " (number->string (* (+ (length xs) 1) 8)) ")"
                  "malloc")
                ,@(map (lambda (iptr n)
                         (comment-line
                          "  %" (s-> iptr) " = getelementptr inbounds i64, i64* %" (s-> cptr) ", i64 " (number->string n)
                          (string-append "&" (s-> iptr) "[" (number->string n) "]")))
                       eptrs
                       (cdr (range (+ 1 (length xs)))))
                ,@(map (lambda (x iptr)
                         (comment-line
                          "  store i64 %" (s-> x) ", i64* %" (s-> iptr)
                          (string-append "*" (s-> iptr) " = %" (s-> x))))
                       xs
                       eptrs)
                ,(comment-line
                  "  %" (s-> fptrptr) " = getelementptr inbounds i64, i64* %" (s-> cptr) ", i64 0"
                  (string-append "&" (s-> cptr) "[0]"))
                ,(comment-line 
                  "  %" (s-> f) " = ptrtoint void(i64"
                  (foldr string-append "" (map (lambda (_) ",i64")
                                               (range (- n 1))))
                  ")* @" (s-> lamx) " to i64"
                  "fptr cast; i64(...)* -> i64")
                ,(comment-line
                  "  store i64 %" (s-> f) ", i64* %" (s-> fptrptr)
                  "store fptr")
                ,(comment-line
                  "  %" (s-> x) " = ptrtoint i64* %" (s-> cptr) " to i64"
                  "closure cast; i64* -> i64")
                ,(e->llvm e0)))]
      [`(let ([,x (env-ref ,y ,n)]) ,e0)
       (define yptr (gensym 'envptr))
       (define iptr (gensym 'envptr))
       (apply string-append
              `(,(comment-line
                  "  %" (s-> yptr) " = inttoptr i64 %" (s-> y) " to i64*"
                  "closure/env cast; i64 -> i64*")
                ,(comment-line
                  "  %" (s-> iptr) " = getelementptr inbounds i64, i64* %" (s-> yptr) ", i64 " (number->string n)
                  (string-append "&" (s-> yptr) "[" (number->string n) "]"))
                ,(comment-line
                  "  %" (s-> x) " = load i64, i64* %" (s-> iptr) ", align 8"
                  (string-append "load; *" (s-> iptr)))
                ,(e->llvm e0)))]
      [`(let ([,x (prim ,op ,ys ...)]) ,e0)
       (string-append
        (comment-line
         "  %" (s-> x) " = call i64 @" (prim-name op) "("
         (if (null? ys)
             ""
             (string-append
              "i64 %" (s-> (car ys))
              (foldl (lambda (y acc) (string-append acc ", i64 %" (s-> y))) "" (cdr ys))))
         ")"
         (string-append "call " (prim-name op)))
        (e->llvm e0))]
      [`(let ([,x (apply-prim ,op ,y)]) ,e0)
       (string-append
        (comment-line
         "  %" (s-> x) " = call i64 @" (prim-applyname op) "(i64 %" (s-> y) ")"
         (string-append "call " (prim-applyname op)))
        (e->llvm e0))]
      [`(if ,x ,e0 ,e1)
       (define cmp (gensym 'cmp))
       (define tlab (gensym 'then))
       (define flab (gensym 'else))
       (string-append
        (comment-line "  %" (s-> cmp) " = icmp eq i64 %" (s-> x) ", 15"
                      "false?")
        (comment-line "  br i1 %" (s-> cmp) ", label %" (s-> flab) ", label %" (s-> tlab)
                      "if")
        "\n" (s-> tlab) ":\n"
        (e->llvm e0)
        "\n" (s-> flab) ":\n"
        (e->llvm e1))]
      [`(clo-app ,fx ,xs ...)
       (define cloptr (gensym 'cloptr))
       (define i0ptr (gensym 'i0ptr))
       (define fptr (gensym 'fptr))
       (define f (gensym 'f))
       (apply string-append
              `(,(comment-line
                  "  %" (s-> cloptr) " = inttoptr i64 %" (s-> fx) " to i64*"
                  "closure/env cast; i64 -> i64*")
                ,(comment-line
                  "  %" (s-> i0ptr) " = getelementptr inbounds i64, i64* %" (s-> cloptr) ", i64 0"
                  (string-append "&" (s-> cloptr) "[0]"))
                ,(comment-line
                  "  %" (s-> f) " = load i64, i64* %" (s-> i0ptr) ", align 8"
                  (string-append "load; *" (s-> i0ptr)))
                ,(comment-line
                  "  %" (s-> fptr) " = inttoptr i64 %" (s-> f) " to void (i64"
                  (foldl string-append "" (map (lambda (_) ",i64") (range (length xs))))
                  ")*"
                  "cast fptr; i64 -> void(...)*")
                ,(comment-line
                  "  musttail call fastcc void %" (s-> fptr)
                  "(i64 %" (s-> fx)
                  (foldl (lambda (x acc) (string-append acc ", i64 %" (s-> x))) "" xs)
                  ")"
                  "tail call")
                "  ret void\n"))]))

  (define (proc->llvm proc)
    (match proc
      [`(proc (main) ,e)
       (string-append
        "define void @proc_main() {\n"
        (e->llvm e)
        "}\n"
        "\n\n"
        "define i32 @main() {\n"
        "  call fastcc void @proc_main()\n"
        "  ret i32 0\n"
        "}\n\n")]
      [`(proc (,lamx ,x0 ,xs ...) ,e0)
       (string-append
        "define void @" (symbol->string lamx)
        "("
        (foldl (lambda (x args) (string-append args ", i64 %" (s-> x)))
               (string-append "i64 %" (s-> x0))
               xs)
        ") {\n"
        ;;"call void @print_u64(i64 " (number->string (random 100000 999999)) ")\n"  ; useful for debugging
        (e->llvm e0)
        "}\n")]))
  
  (define llvm-procs
    (apply string-append
           (map (lambda (s) (string-append s "\n\n"))
                (map proc->llvm procs))))
  (string-append llvm-procs
                 "\n\n\n"
                 globals))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (top-level e)
  ;(display e)
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
