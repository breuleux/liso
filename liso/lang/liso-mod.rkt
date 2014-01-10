
#lang s-exp "./hack.rkt"

(provide

 ;; (for-syntax
 ;;  (rename-out
 ;;   ;; (liso-syntax-case syntax-case)
 ;;   (liso-syntax-rules stx-rules)))

 (except-out
  (all-from-out "./hack.rkt")
  let           ;; Becomes (let (list (= x y) ...) ...)
  let*          ;; Becomes (let* (list (= x y) ...) ...)
  letrec        ;; Becomes (letrec (list (= x y) ...) ...)
  parameterize  ;; Becomes (parameterize (list (= x y) ...) ...)
  lambda        ;; Becomes (lambda (list x ...) ...)
  ;; define     ;; unchanged
  if            ;; Becomes (if test (then ...) (else ...))
  ;; cond       ;; unchanged
  case          ;; Becomes (case ((list c ...) ...) ...)
  for           ;; Becomes (for ((list (<- x y)) ...) ...)
  set!          ;; Becomes (:= var val)

  ;; define-syntax-rule
  ;; syntax-case
  ;; syntax-rules
  ;; =          ;; becomes define
  ->
  )

 (rename-out
  (liso-let let)
  (liso-let* let*)
  (liso-letrec letrec)
  (liso-parameterize parameterize)
  (liso-lambda ->)
  (liso-if if)
  (liso-case case)
  (liso-for for)
  ;; (liso-define-syntax-rule define-syntax-rule)
  ;; (liso-syntax-rules syntax-rules)
  (set! :=)
  (begin block)
  (if <then>_<else>)
  (if2 <if>_<else>)
  (quote |..|)
  (quasiquote |.|)
  (unquote |^|)

  (expt **)
  (cons ::)
  (or \|\|)
  (and &&)
  (string-append ++)

  (apply racket-apply)
  )

 void
 void?
 <>
 /=
 $

 )

(define void (cond))

(define (void? x) (eq? x void))

(define-syntax-rule (if2 a test b) (if test a b))

(define (/= a b)
  (not (== a b)))

(define <>
  (case-lambda
   ((obj) (pretty-print obj))
   ((port obj) (pretty-print obj port))))


(define-syntax liso-let
  (syntax-rules (list =)
    ((_ (list (= id value) ...) stmt ...)
     (let ((id value) ...) stmt ...))
    ((_ (name (= id value) ...) stmt ...)
     (let name ((id value) ...) stmt ...))
    ((_ name stmt ...)
     (let name () stmt ...))))

(define-syntax liso-let*
  (syntax-rules (list =)
    ((_ (list (= id value) ...) stmt ...)
     (let* ((id value) ...) stmt ...))))

(define-syntax liso-letrec
  (syntax-rules (list =)
    ((_ (list (= id value) ...) stmt ...)
     (letrec ((id value) ...) stmt ...))))

(define-syntax liso-parameterize
  (syntax-rules (list =)
    ((_ (list (= id value) ...) stmt ...)
     (parameterize ((id value) ...) stmt ...))))

(define-syntax liso-lambda
  (syntax-rules (list)
    ((_ (list id ... (* rest _)) stmt ...)
     (lambda (id ... . rest) stmt ...))
    ((_ (list id ...) stmt ...)
     (lambda (id ...) stmt ...))
    ((_ id stmt ...)
     (lambda id stmt ...))))

(define-syntax liso-if
  (syntax-rules (then else)
    ((_ test (then stmt1 ...) (else stmt2 ...))
     (if test (let () stmt1 ...) (let () stmt2 ...)))
    ((_ test stmt ...)
     (if test (let () stmt ...) #f))))

(define-syntax liso-case
  (syntax-rules (list else)
    ((_ x ((list c ...) stmt) ...)
     (case x ((c ...) stmt) ...))
    ((_ x ((list c ...) stmt) ... (else estmt))
     (case x ((c ...) stmt) ... (else estmt)))))

(define-syntax liso-for
  (syntax-rules (list <-)
    ((_ (list (<- v l) ...) stmt ...)
     (for ((v l) ...) stmt ...))))

(define-syntax $
  (syntax-rules (begin list =)
    ((_ expr (begin (= b v) ...))
     (let* ((b v) ...) expr))
    ((_ expr (list (= b v) ...))
     (let* ((b v) ...) expr))
    ((_ expr (= b v) ...)
     (let* ((b v) ...) expr))
    ))


(define-syntax liso-syntax-case
  (syntax-rules (list begin)
    ((_ e (list l ...) clause ...)
     (syntax-case e (list begin l ...) clause ...))))

(define-syntax liso-syntax-rules
  (syntax-rules (list)
    ((_ (list l ...) clause ...)
     (syntax-rules (list begin l ...) clause ...))
    ((_ ...)
     (error "ugh"))))

(define-syntax liso-define-syntax-rule
  (syntax-rules (list begin)
    ((_ expr stmt ...)
     (define-syntax
       (syntax-rules (list begin)
         expr (begin stmt ...))))))

