
(module _ racket
 (provide
   (except-out (all-from-out racket) = ==)
   (rename-out (define =) (= ==))))

