;; PL Project - Fall 2018
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus (e1 e2) #:transparent) ;; minus two expressions
(struct mult (e1 e2) #:transparent) ;; multipy two expressions
(struct div (e1 e2) #:transparent) ;; divide two expressions
(struct neg (e) #:transparent) ;; negative a expression

(struct bool (e) #:transparent) ;; a boolean constants, e,g., (bool #t)
(struct andalso (e1 e2) #:transparent) ;; andalso two expressions
(struct orelse (e1 e2) #:transparent) ;; or divide two expressions

(struct cnd (e1 e2 e3) #:transparent) ;; condition
(struct iseq (e1 e2) #:transparent) ;; comparison two expressions
(struct ifnzero (e1 e2 e3) #:transparent) ;; condition
(struct ifleq (e1 e2 e3 e4) #:transparent) ;; condition

(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application

(struct with (s e1 e2) #:transparent)

(struct apair (e1 e2) #:transparent)
(struct 1st (e) #:transparent)
(struct 2nd (e) #:transparent)

(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent)

;; Problem 1

(define (racketlist->numexlist xs)
  (cond [(null? xs) (munit)]
        [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs)))]
        [#t (error ("it's not a racket list"))]
  )
)
(define (numexlist->racketlist xs)
  (cond [(munit? xs) null]
        [(apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))]
        [#t (error ("it's not a numex list"))]
  )
)

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)])
  (cond [(equal? str (car (car env))) (cdr (car env))]
        [else (envlookup (cdr env) str)]
  )
)

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(num? e)
         (cond [(integer? (num-int e)) e]
               [else (error "NUMEX num applied to non racket integer")])]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        [(minus? e)
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1)
                       (num-int v2)))
               (error "NUMEX minus applied to non-numbers")))]
        [(mult? e)
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1)
                       (num-int v2)))
               (error "NUMEX multiply applied to non-numbers")))]
        [(div? e)
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (/ (num-int v1)
                       (num-int v2)))
               (error "NUMEX divide applied to non-numbers")))]
        [(neg? e)
         (let ([v (eval-under-env (neg-e e) env)])
           (if (or (num? v)
                   (bool? v))
               (if (num? v)
                   (num (- 0 (num-int v)))
                   (if (equal? (bool-e v) #t) (bool #f) (bool #t)))
               (error "NUMEX negation applied to non-number or non-boolean")))]
        [(bool? e)
         (cond [(boolean? (bool-e e)) e]
               [else (error "NUMEX bool applied to non racket boolean")])]
        [(andalso? e)
         (let ([v1 (eval-under-env (andalso-e1 e) env)]
               [v2 (eval-under-env (andalso-e2 e) env)])
           (if (and (bool? v1)
                    (bool? v2))
               (bool (and (bool-e v1)
                              (bool-e v2)))
               (error "NUMEX andalso applied to non-booleans")))]
        [(orelse? e)
         (let ([v1 (eval-under-env (orelse-e1 e) env)]
               [v2 (eval-under-env (orelse-e2 e) env)])
           (if (and (bool? v1)
                    (bool? v2))
               (bool (or (bool-e v1)
                         (bool-e v2)))
               (error "NUMEX orelse applied to non-booleans")))]
        [(cnd? e)
         (let ([v1 (eval-under-env (cnd-e1 e) env)])
           (if (bool? v1)
               (if (equal? (bool-e v1) #t)
                              (eval-under-env (cnd-e2 e) env)
                              (eval-under-env (cnd-e3 e) env))
               (error "NUMEX cnd applied to non-boolean")))]
        [(iseq? e)
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
               (if (equal? v1 v2)
                   (bool #t)
                   (bool #f)))]
        [(ifnzero? e)
         (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
           (if (num? v1)
               (if (equal? (num-int v1) 0)
                   (eval-under-env (ifnzero-e3 e) env)
                   (eval-under-env (ifnzero-e2 e) env))
               (error "NUMEX isnzero applied to non-number")))]
        [(ifleq? e)
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (if (<= (num-int v1) (num-int v2))
                   (eval-under-env (ifleq-e3 e) env)
                   (eval-under-env (ifleq-e4 e) env))
               (error "NUMEX ifleq applied to non-number")))]
         [(lam? e)
         (if (and (or (string? (lam-nameopt e))
                      (null? (lam-nameopt e)))
                  (string? (lam-formal e)))
             (closure env e)
             (error "NUMEX lam name and parameter name must be string"))]
        [(apply? e)
         (let ([v (eval-under-env (apply-actual e) env)]
               [clsr (eval-under-env (apply-funexp e) env)])
           (if (closure? clsr)
               (let ([clsrFun (closure-f clsr)])
                 (if (null? (lam-nameopt clsrFun))
                     (eval-under-env (lam-body clsrFun) (cons (cons (lam-formal clsrFun) v) (closure-env clsr)))
                     (eval-under-env (lam-body clsrFun) (cons (cons (lam-nameopt clsrFun) clsr) (cons (cons (lam-formal clsrFun) v) (closure-env clsr))))))
               (error "NUMEX apply applied to non-lam" e)))]
        [(with? e)
         (let ([v1 (eval-under-env (with-e1 e) env)])
           (if (string? (with-s e))
               (eval-under-env (with-e2 e) (cons (cons (with-s e) v1) env))
               (error "NUMEX with applied to non-number or the name of the variable is not a string")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
                (apair v1 v2))]
        [(1st? e)
         (let ([v (eval-under-env (1st-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "NUMEX 1st applied to non-apair" e)))]
        [(2nd? e)
         (let ([v (eval-under-env (2nd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "NUMEX 2nd applied to non-apair")))]
        [(munit? e) e]
        [(ismunit? e)
         (let ([v (eval-under-env (ismunit-e e) env)])
           (if (munit? v)
               (bool #t)
               (bool #f)))]
        [(closure? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (with* bs e2) "CHANGE")

(define (ifneq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define numex-filter "CHANGE")

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
