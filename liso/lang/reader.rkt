
#lang racket

(require srfi/1
         data/queue
         data/gvector
         syntax/strip-context)

(provide
 (rename-out
  (liso-read read)
  (liso-read-syntax read-syntax)))


(define liso-readtable
  (make-readtable
   (current-readtable)

   #\{ 'terminating-macro 
   (case-lambda
    ((ch port)
     (parse port 0))
    ((ch port src line col pos)
     (parse port 0)))))

(define void (when #f #f))
(struct token (name value) #:transparent)

(define (tokens-OPEN brack)
  (list (token 'ID void)
        (token 'OPEN brack)))

(define (tokens-CLOSE brack)
  (list (token 'CLOSE brack)
        (token 'ID void)))


(define $opchar "[+\\-*/~^<>=%$&|?!]")
(define $symchar "[^ \n\r\t()\\[\\]{}.:;,\"'`#]")
(define $opboundary "(?=[ \n\r\t()\\[\\]{}.:;,\"'`#]|$)")


(define-syntax lexotron
  (syntax-rules ()
    ((_ input) (error "No rules apply." (peek-char input)))
    ((_ input (rxp policy fn) rest ...)
     (let ((tmp (regexp-try-match rxp input)))
       (if tmp
           (cons 'policy (and fn (apply fn (map bytes->string/utf-8 tmp))))
           (lexotron input rest ...))))))

(define (gluere . args)
  (pregexp (string-append "^(?:" (apply string-append args) ")")))

(define $tickop (gluere "`(" $symchar "*)`"))
(define $colonop (gluere ":(?:\\.|:|" $opchar ")*"))
(define $dotop (gluere "\\.(?:\\.|:|" $opchar ")*"))
(define $op (gluere $opchar "(?:" $symchar "*" $opchar ")?" $opboundary))
(define $id (gluere "(?:\\d+\\.\\d+)|" $symchar "+"))
(define $keyword (gluere "@(" $symchar "+)"))

(define (liso-lexer in depth)
  (match-define (cons policy result)
    (lexotron in

      ($keyword many
          (lambda (_ m) (tokens-OPEN (list 'KW (string->symbol m)))))
      ($colonop one
          (lambda (m)
            (if (equal? m ":")
                (token 'CLOSE (string->symbol m))
                (token 'OP (string->symbol m)))))

      ;; White space
      (#rx"^[ \t]+" ignore #f)
      (#rx"^(\n[ \t]*)+\\\\" ignore #f)

      ;; ;; :
      ;; ($colonop one
      ;;  (lambda (m) (token 'OP (string->symbol m))))

      ;; . and ^
      ($dotop many
       (lambda (m) (list (token 'ID void)
                         (token 'PFX (string->symbol m)))))
      (#rx"^\\^" many
       (lambda (m) (list (token 'ID void)
                         (token 'PFX (string->symbol m)))))

      ;; operator
      ($op one
       (lambda (m)
         (token 'OP (string->symbol m))))

      ;; identifier
      ($id one
       (lambda (m)
         (token 'ID (read (open-input-string m)))))

      ;; brackets
      (#px"^[\\[\\{]" push
          (lambda (m) (tokens-OPEN (string->symbol m))))
      (#px"^[\\]\\}]" pop
          (lambda (m) (tokens-CLOSE (string->symbol m))))

      ;; Indent
      (#rx"^(\n[ \t]*)+" one
          (lambda (_ m)
            (token 'INDENT (- (string-length m) 1))))

      ;; ,
      (#rx"^,+" one (lambda (m) (token 'OP '|,|)))

      ;; "string"
      (#rx"^\"(?:\\\\\"|[^\"])*\"" one
          (lambda (m)
            (token 'ID (read (open-input-string m)))))

      ;; 'char'
      (#rx"^'\\\\''" one (lambda (m) (token 'ID #\')))
      (#rx"^'([^']*)'" one
          (lambda (_ m)
            (token 'ID (read (open-input-string (string-append "#\\" m))))))

      ;; `operator`
      ($tickop one
       (lambda (_ m)
         (token 'OP (read (open-input-string m)))))

      ;; #"symbol"
      (#rx"^#(\"(?:\\\\\"|[^\"])*\")" one
          (lambda (_ m)
            (token 'ID (string->symbol
                        (read (open-input-string m))))))

      ;; #stuff...
      (#rx"^(?=#)" one
          (lambda (m)
            (token 'ID (read in))))

      ;; comments
      (#rx"^;;[^\n]+" ignore #f)

      ;; s-expressions
      (#rx"^(?=\\()" one
          (lambda (_)
            (token 'ID
                   (parameterize ((current-readtable liso-readtable))
                                 (read in)))))

      ;; eof
      (#rx"^$" done (lambda (m) (stream (token 'CLOSE '|}|)
                                        (token 'ID void))))))

  (case policy
    ((one) (stream-cons result (liso-lexer in depth)))
    ((ignore) (liso-lexer in depth))
    ((push) (stream-append result (liso-lexer in (and depth (+ depth 1)))))
    ((pop)
     (if (and depth (<= depth 0))
         result
         (stream-append result (liso-lexer in (and depth (- depth 1))))))
    ((many) (stream-append result (liso-lexer in depth)))
    ((done) result)
    (else (error "Unknown policy" policy))))


(define (liso-raw port (depth #f))
  (stream-cons
   (token 'ID void)
   (stream-cons
    (token 'OPEN '|{|)
    (liso-lexer port depth))))

(define $linebreak '|,|)
(define $indent-open '|{|)
(define $indent-close '|}|)

(define (liso-indent tokens (cont (lambda (_) (error "Unbalanced brackets."))))
  (define curr #f)
  (define stack (make-queue))
  (define (process tokens)
    (if (stream-empty? tokens)
        (stream)
        (let ((tok (stream-first tokens))
              (rest (stream-rest tokens)))
          ;; (pretty-print tok)
          (case (token-name tok)
            ((INDENT)
             (define inserts
               (let ((new-indent (token-value tok)))
                 (cond
                  ((not curr)
                   (set! curr new-indent)
                   (list (token 'OP $linebreak)))
                  (#t
                   (cond
                    ((> new-indent curr)
                     (enqueue-front! stack curr)
                     (set! curr new-indent)
                     (tokens-OPEN $indent-open))
                    ((= new-indent curr)
                     (list (token 'OP $linebreak)))
                    (#t
                     (define rval (list (token 'OP $linebreak)))
                     (let loop ()
                       (when (and (< new-indent curr)
                                  (not (queue-empty? stack)))
                             (set! curr (dequeue! stack))
                             (set! rval (append
                                         (tokens-CLOSE $indent-close)
                                         rval))
                             (loop)))
                     (when (and (< new-indent curr)
                                (queue-empty? stack))
                           (set! curr 'inconsistent))
                     rval))))))
             (stream-append inserts (process rest)))
            ((OPEN)
             (if (eq? curr 'inconsistent)
                 (error "Inconsistent indent")
                 (stream-cons tok
                              (liso-indent rest process))))
            ((CLOSE)
             (stream-append
              (foldr append '()
                     (map (lambda (_) (tokens-CLOSE $indent-close))
                          (queue->list stack)))
              (list tok)
              (cont rest)))
            (else
             (if (eq? curr 'inconsistent)
                 (error "Inconsistent indent")
                 (stream-cons tok
                              (process rest))))))))
  (process tokens))


(define (liso-alternate tokens (last-id? #f))
  (if (stream-empty? tokens)
      (if last-id?
          (stream)
          (stream void))
      (let* ((tok (stream-first tokens))
             (rest (stream-rest tokens))
             (id? (eq? (token-name tok) 'ID)))
        (stream-append
         (cond
          ((and id? last-id?) '(| |))
          ((and (not id?) (not last-id?))
           (list void))
          (else '()))
         (stream-cons
          (token-value tok)
          (liso-alternate rest id?))))))


(define-syntax-rule (chk expr expr2)
  (with-handlers ((exn? (lambda (e) expr2)))
    expr))

(define absent (gensym 'absent))

(define (oparse next order finalize)
  (define between (next))
  (call/cc (lambda (return)
    (define right-op (chk (next) (return (finalize between))))
    (define left-op absent)
    (define stack (make-queue))
    (define current absent)

    (let loop ()
      (define ord
        (cond
         ((eq? left-op absent)
          (if (eq? right-op absent)
              (return between)
              'right))
         ((eq? right-op absent) 'left)
         (#t (order left-op right-op between))))
      (case ord
        ((left)
         (gvector-add! current between)
         (set! between (finalize current))
         (let ((front (dequeue! stack)))
           (set! left-op (car front))
           (set! current (cdr front))))
        ((right)
         (enqueue-front! stack (cons left-op current))
         (set! left-op right-op)
         (set! current (gvector (gvector right-op) between))
         (set! between (next))
         (set! right-op (chk (next) absent)))
        ((aggr)
         (gvector-add! (gvector-ref current 0) right-op)
         (gvector-add! current between)
         (set! left-op right-op)
         (set! between (next))
         (set! right-op (chk (next) absent)))
        (else
         (error "Operators cannot be mixed in the order given" left-op right-op)))
      (loop)))))




(define order-table
  ;; ((compat-left compat-right prio-left prio-right) operators ...)
  ;; When comparing op1 to op2,
  ;; if compat-left(op1) & compat-right(op2) == 0: error
  ;; if prio-left(op1) > prio-right(op2): op1 wins
  ;; if prio-left(op1) < prio-right(op2): op2 wins
  ;; if prio-left(op1) == prio-right(op2): op1 and op2 are aggregated
  ;; When comparing two operators that have void between them,
  ;; the second operator given is (PFX op).

  `(;; brackets
    ((255 255 0 10000) |{| |(| |[| (PFX |{|) (PFX |[|) (PFX |(|))
    ((255 255 10000 0) |}| |)| |]| (PFX |}|) (PFX |]|) (PFX |)|))

    ;; organization and assignment
    ((255 255 3 3) |,| (PFX |,|))
    ((255 255 5 6) => (PFX =>))
    ((255 4   10 11) ->)  ;; 4 prevents arith/custom operators in lhs
    ((255 255 10 11) = :=)

    ;; KW
    ((255 255 1 10000)
     ,(match-lambda
       ((list 'KW x) #t)
       ((list 'PFX (list 'KW x)) #t)
       (_ #f)))
    ((255 255 4 1) : (PFX :))

    ;; PFX
    ((255 255 0 9999) ,(lambda (x) (list? x)))

    ;; <...>
    ((255 255 20 20)
     ,(lambda (x)
        ;; x == <{...}>, e.g. <if>, <union>, etc.
        (let* ((s (symbol->string x))
               (l (string-length s)))
          (and (eq? (string-ref s 0) #\<)
               (eq? (string-ref s (- l 1)) #\>)))))

    ;; logical
    ((1 1 100 100) \|\|)
    ((1 1 101 101) &&)
    ((1 1 102 102) !)

    ;; comparison
    ((1 1 150 150) < > <= >= == /=)

    ;; arith
    ((1 1 201 200) + -)
    ((1 1 301 300) * / // %)
    ((1 1 400 401) **)

    ;; dot and juxtaposition
    ((255 255 1001 1000) | |)
    ((255 255 2000 10000)
     ^
     ,(lambda (x)
        ;; x == .{...}, e.g. ., .+, .&*&, etc.
        (let* ((s (symbol->string x)))
          (eq? (string-ref s 0) #\.))))

    ;; custom
    ((2 2 20 20) ,(lambda (x) #t))

    ))

(define ord-hash (make-hash))
(define ord-fns '())

(for-each
 (lambda (entry)
   (let ((policy (car entry)))
     (for-each
      (lambda (operator)
        (if (procedure? operator)
            (set! ord-fns (cons (cons operator policy) ord-fns))
            (hash-set! ord-hash operator policy)))
      (cdr entry))))
 order-table)
(set! ord-fns (reverse ord-fns))


(define (get-priority op sel1 sel2)
  (let* ((hash-policy (hash-ref ord-hash op #f))
         (policy (if hash-policy
                     hash-policy
                     (let loop ((fns ord-fns))
                       (let ((fn (car fns)))
                         (if ((car fn) op)
                             (cdr fn)
                             (loop (cdr fns))))))))
    (list (sel1 policy) (sel2 policy))))

(define (order o1 o2 between)
  (match-let (((list c1 p1) (get-priority o1 car caddr))
              ((list c2 p2) (get-priority o2 cadr cadddr)))
    (cond
     ((zero? (bitwise-and c1 c2)) 'none)
     ((> p1 p2)
      (if (eq? between void)
          (order o1 (list 'PFX o2) #f)
          'left))
     ((< p1 p2) 'right)
     (else 'aggr))))

(define (make-finalizer fn)
  (lambda (f)
    (let ((exprs (gvector->list f))) 
      (apply fn
             (gvector->list (car exprs))
             (cdr exprs)))))

(define (collapse-start li)
  (if (<= (length li) 1)
      li
      (if (eq? (car li) (cadr li))
          (collapse-start (cdr li))
          li)))

(define (split-lead expr)
  (match expr
    ((list 'apply f arg)
     (let* ((results (split-lead f))
            (lead (car results))
            (args (cdr results)))
       (cons lead
             (if (eq? args absent)
                 arg
                 `(apply ,args ,arg)))))
    ((list f fargs ...)
     (let* ((results (split-lead f))
            (lead (car results))
            (args (cdr results)))
       (cons lead
             (if (eq? args absent)
                 `(list ,@fargs)
                 `(,args ,@fargs)))))
    (_ (cons expr absent))))


(define finalize
  (make-finalizer
   (lambda (ops . args)
     (match ops

            ('(|{| |}|)
             (let ((arg (cadr args)))
               (match arg
                      ((list '#%seq args ...)
                       `(begin ,@args))
                      ((== void eq?)
                       `(begin))
                      (_ arg))))

            ('(|[| |]|)
             (let ((arg (cadr args)))
               (match arg
                      ((list '#%seq args ...)
                       `(list ,@args))
                      ((== void eq?)
                       `(list))
                      (_ `(list ,arg)))))

            (`((KW ,f) :)
             (let ((arg (match (cadr args)
                          ((list '#%seq args ...)
                           `(begin ,@args))
                          ((== void eq?)
                           #f)
                          (x x)))
                   (body (match (caddr args)
                          ((list 'begin args ...)
                           args)
                          ((== void eq?)
                           `())
                          (x (list x)))))
               (if arg
                   `(,f ,arg ,@body)
                   `(,f ,@body))))

            ;; ('(|[| |]|)
            ;;  (let ((arg (cadr args)))
            ;;    (match arg
            ;;           ((list '#%seq args ...)
            ;;            `(sqbr ,@args))
            ;;           ((== void eq?)
            ;;            `(sqbr))
            ;;           (_ `(sqbr ,arg)))))

            ('(| |)
             (let ((f (car args))
                   (arg (cadr args)))
               (match arg
                      ((list 'list args ...)
                       `(,f ,@args))
                      (_
                       `(apply ,f ,arg)))))

            ('(=>)
             args)

            ('(|.|)
             (list 'quasiquote (cadr args)))

            ('(|..|)
             (list 'quote (cadr args)))

            ('(|.:|)
             (let* ((arg (cadr args)))
               (match arg
                 ((list begin rest ...)
                  (list 'quasiquote rest))
                 ((list begin rest ...)
                  (list 'quasiquote arg)))))

            ;; ('(:)
            ;;  (let* ((lhs (car args))
            ;;         (body (cadr args))
            ;;         (results (split-lead lhs))
            ;;         (lead (car results))
            ;;         (arg (cdr results)))
            ;;    (define rest
            ;;      (match body
            ;;        ((list 'begin stmts ...)
            ;;         stmts)
            ;;        (_
            ;;         (list body))))
            ;;    (if (eq? lead void)
            ;;        (if (eq? arg absent)
            ;;            rest
            ;;            (cons arg rest))
            ;;        (if (eq? arg absent)
            ;;            (cons lead rest)
            ;;            (cons lead (cons arg rest))))))

            ((list '|,| ...)
             (let ((args (filter (lambda (arg) (not (eq? arg void))) args)))
               (if (= (length args) 1)
                   (car args)
                   `(#%seq ,@args))))

            ((list single)
             (if (eq? (car args) void)
                 (if (eq? (cadr args) void)
                     single
                     (cons single (cdr args)))
                 (cons single args)))

            (_
             (let* ((ops (reverse (collapse-start (reverse ops))))
                    (s (string->symbol
                        (string-join (map symbol->string ops) "_"))))
               (cons s args)))))))

(define (make-next stream)
  (define current stream)
  (lambda ()
    (let ((rval (stream-first current)))
      (set! current (stream-rest current))
      rval)))

(define (parse in (depth #f))
  (let ((tokens (liso-alternate (liso-indent (liso-raw in depth)))))
    (oparse (make-next tokens)
            order
            finalize)))

(define (liso-read in)
  (syntax->datum (read-syntax #f in)))

;; This line seems to prevent racket from being required twice (once
;; here and once by the module returned by read-syntax). I am not
;; entirely sure what is going on, but hey, it works, I guess.
(module liso-mod "liso-mod.rkt")

(define (liso-read-syntax src in)
  (define ptree (parse in))
  ;; (pretty-print ptree)
  ;; (display "======\n")
  (define rval
    (with-syntax
        ((code ptree)
         (path 'liso/lang/liso-mod))
      (strip-context
       #'(module _ path code))))
  rval)
