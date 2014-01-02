
#lang racket

(require srfi/1
         data/queue
         data/gvector
         parser-tools/lex
         (prefix-in rx parser-tools/lex-sre)
         parser-tools/yacc
         syntax/strip-context)

(provide
 (rename-out
  (liso-read read)
  (liso-read-syntax read-syntax)))


(define-tokens tk (ID OP PFX SFX OPEN CLOSE INDENT))


(define $boundary
  (apply set (string->list " \n\r\t()[]{}.:;,\"'")))

(define-lex-abbrevs
  (opchar (char-set "+-*/~^<>=%#$@&|?!"))
  (symchar (rx~ (char-set " \n\r\t()[]{}.:;,\"'"))))


(define liso-raw-lexer
  (lexer

   ;; comments
   ((rx: ";;" (rx+ (rx~ "\n"))) (liso-raw-lexer input-port))

   ;; brackets
   ((rxor "(" "[" "{") (token-OPEN (string->symbol lexeme)))
   ((rxor ")" "]" "}") (token-CLOSE (string->symbol lexeme)))

   ;; 'char'
   ("'\\''" (token-ID #\'))
   ((rx: "'" (rx* (rx~ "'")) "'")
    (let* ((len (string-length lexeme))
           (name (substring lexeme 1 (- len 1))))
      (token-ID (read (open-input-string (string-append "#\\" name))))))

   ;; s"symbol"
   ((rx: "s\"" (rx* (rxor (rx~ "\"") "\\\"")) "\"")
    (token-ID (read (open-input-string (format "(string->symbol ~a)"
                                               (substring lexeme 1))))))

   ;; "string"
   ((rx: "\"" (rx* (rxor (rx~ "\"") "\\\"")) "\"")
    (token-ID (read (open-input-string lexeme))))
   
   ;; :
   ((rx: ":" (rx+ (rxor opchar "." ":")))
    (token-OP (string->symbol lexeme)))
   (":" (token-OP '|:|))

   ;; .
   ((rx: ".." (rx+ ".")) (token-ID (string->symbol lexeme)))
   ((rx: "." (rx+ (rxor opchar "." ":")))
    (token-OP (string->symbol lexeme)))
   (".." (token-PFX '|..|))
   ("." (token-PFX '|.|))

   ;; ,
   ((rx+ ",") (token-OP '|,|))
   ((rx: opchar (rx? (rx: (rx* symchar) opchar)))
    (token-OP (string->symbol lexeme)))
   ;; ((rx+ opchar)
   ;;  (let ((c (peek-char input-port)))
   ;;    (if (or (eof-object? c) (set-member? $boundary c))
   ;;        (token-OP lexeme)
   ;;        (token-PFX lexeme))))
   ((rx+ symchar) (token-ID (read (open-input-string lexeme))))
   ((rx+ (char-set " \t")) (liso-raw-lexer input-port))
   ((rx: (rx+ (rx: "\n" (rx* " "))) "\\")
    (liso-raw-lexer input-port))
   ((rx+ (rx: "\n" (rx* " ")))
    (let ((parts (reverse (cons "" (string-split lexeme "\n")))))
      (token-INDENT (string-length (car parts)))))
   ((eof) 'EOF)))

(define (liso-raw port)
  (stream-cons
   (token-OPEN '|{|)
   (let loop ((port port))
     (let ((next (liso-raw-lexer port)))
       (if (eq? next 'EOF)
           (stream (token-CLOSE '|}|) 'EOF)
           (stream-cons
            next
            (loop port)))))))

(define $linebreak '|,|)
(define $indent-open '|{|)
(define $indent-close '|}|)

(define (liso-indent tokens (cont (lambda (_) (error "Unbalanced brackets."))))
  (define curr #f)
  (define stack (make-queue))
  (define (process tokens)
    (if (stream-empty? tokens)
        (stream)
        (let ((token (stream-first tokens))
              (rest (stream-rest tokens)))
          (case (token-name token)
            ((INDENT)
             (define inserts
               (let ((new-indent (token-value token)))
                 (cond
                  ((not curr)
                   (set! curr new-indent)
                   (list (token-OP $linebreak)))
                  (#t
                   (cond
                    ((> new-indent curr)
                     (enqueue-front! stack curr)
                     (set! curr new-indent)
                     (list (token-OPEN $indent-open)))
                    ((= new-indent curr)
                     (list (token-OP $linebreak)))
                    (#t
                     (define rval (list (token-OP $linebreak)))
                     (let loop ()
                       (when (and (< new-indent curr)
                                  (not (queue-empty? stack)))
                             (set! curr (dequeue! stack))
                             (set! rval (cons (token-CLOSE $indent-close) rval))
                             (loop)))
                     (when (and (< new-indent curr)
                                (queue-empty? stack))
                           (set! curr 'inconsistent))
                     rval))))))
             (stream-append inserts (process rest)))
            ((OPEN)
             (if (eq? curr 'inconsistent)
                 (error "Inconsistent indent")
                 (stream-cons token
                              (liso-indent rest process))))
            ((CLOSE)
             (stream-append
              (map (lambda (_) (token-CLOSE $indent-close))
                   (queue->list stack))
              (list token)
              (cont rest)))
            (else
             (if (eq? curr 'inconsistent)
                 (error "Inconsistent indent")
                 (stream-cons token
                              (process rest))))))))
  (process tokens))


(define void (when #f #f))

(define (liso-alternate tokens (last 'OP))
  (if (stream-empty? tokens)
      (stream)
      (let* ((token (stream-first tokens))
             (rest (stream-rest tokens))
             (_type (token-name token))
             (type (case _type ((OPEN) 'PFX) ((CLOSE) 'SFX) (else _type))))
        (define inserts
          `(((ID ID) | |)
            ((ID OP))
            ((ID PFX) | | ,void)
            ((ID SFX))
            ((ID EOF))

            ((OP ID))
            ((OP OP) ,void)
            ((OP PFX) ,void)
            ((OP SFX) ,void)
            ((OP EOF) ,void)

            ((PFX ID))
            ((PFX OP) ,void)
            ((PFX PFX) ,void)
            ((PFX SFX) ,void)
            ((PFX EOF) ,void)

            ((SFX ID) ,void | |)
            ((SFX OP) ,void)
            ((SFX PFX) ,void | | ,void)
            ((SFX SFX) ,void)
            ((SFX EOF) ,void)))
        (stream-append
         (cdr (assoc (list last type) inserts))
         (if (eq? token 'EOF)
             (list)
             (list (token-value token)))
         (liso-alternate rest type)))))

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
         (#t (order left-op right-op))))
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
         (error "Cannot order" left-op right-op)))
      (loop)))))


(define (order o1 o2)

  (define ord
    `((0 10000 |{|)
      (10000 0 |}|)
      (0 10000 |(|)
      (10000 0 |)|)
      (0 10000 |[|)
      (10000 0 |]|)

      (1 1 |,|)
      (5 6 |=>|)
      (10 11 = : := -> !! $)

      ;; logical
      (100 100 ,(string->symbol "||"))
      (101 101 &&)

      ;; comparison
      (150 150 < > <= >= == /=)

      ;; arith
      (201 200 + -)
      (301 300 * /)
      (400 401 **)

      (900 10000 |.| |..|)
      (1001 1000 | |)
      ))
  (define (prio op sel)
    (let loop ((p ord))
      (if (null? p)
          20
          (let ((candidates (car p))
                (rest (cdr p)))
            (if (memq op (cddr candidates))
                (sel candidates)
                (loop rest))))))
  (let ((p1 (prio o1 car))
        (p2 (prio o2 cadr)))
    (cond
     ((> p1 p2) 'left)
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

;; (define (split-lead expr)
;;   (match expr
;;     ((list 'apply f arg)
;;      (let* ((results (split-lead f))
;;             (lead (car results))
;;             (args (cdr results)))
;;        (cons lead (append (list arg) args))))
;;     ((list f fargs ...)
;;      (let* ((results (split-lead f))
;;             (lead (car results))
;;             (args (cdr results)))
;;        (cons lead (append (cons 'list fargs) args))))
;;     (_ (cons expr '()))))

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

            ('(|(| |)|)
             (let ((arg (cadr args)))
               (match arg
                      ((list '#%seq args ...)
                       `(list ,@args))
                      ((== void eq?)
                       `(list))
                      (_ `(list ,arg)))))

            ('(|[| |]|)
             (let ((arg (cadr args)))
               (match arg
                      ((list '#%seq args ...)
                       `(sqbr ,@args))
                      ((== void eq?)
                       `(sqbr))
                      (_ `(sqbr ,arg)))))

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

            ('(:)
             (let* ((lhs (car args))
                    (body (cadr args))
                    (results (split-lead lhs))
                    (lead (car results))
                    (arg (cdr results)))
               (define rest
                 (match body
                   ((list 'begin stmts ...)
                    stmts)
                   (_
                    (list body))))
               (cons
                lead
                (if (eq? arg absent)
                    rest
                    (cons arg rest)))))

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

(define (parse in)
  (let ((tokens (liso-alternate (liso-indent (liso-raw in)))))
    ;; (pretty-print (stream->list tokens))
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


