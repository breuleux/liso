
Liso
====

Liso is an alternative syntax for Lisp-like languages, implemented
here for Racket.


Examples
--------

    fib[n] =
       @if n <= 1:
          n
          fib[n - 1] + fib[n - 2]
    
    fib[30]

<=>

    (= (fib n)
       (if (<= n 1)
           n
           (+ (fib (- n 1))
              (fib (- n 2)))))
    (fib 30)


There are many more examples
[here](https://github.com/breuleux/liso/tree/master/liso/examples),
including examples of macros and operator macros.


Using
-----

You can try out the _[first version] by putting the following line at
the beginning of a source file:

    #lang planet breuleux/liso

For the bleeding edge version, just download the code somewhere and
then put this at the beginning:

    #lang reader "/path/to/liso/lang/reader.rkt"

Then, execute as follows:

    racket file.liso


Highlights
----------

* It is a pure operator syntax, a particular implementation of what I
  call "o-expressions". I made o-expressions for a new programming
  language that I am designing, so they were not designed to work with
  Lisp-like languages. However, I believe that they are a competent
  alternative to s-expressions, so there is no harm trying.

* The syntax cannot be customized from within itself: operator
  priority is predetermined, even for custom operators.

* It is general and homoiconic. All syntactic elements behave the same
  in all contexts, they have no associated semantics, and they reduce
  to extremely regular structures.

* It *does* support macros. In fact, you can use Racket's macro system
  without any changes to their underlying logic. See
  [here](https://github.com/breuleux/liso/blob/master/liso/examples/macros.liso)
  for example.


Rules
-----

    + Rule       + O-expression              + S-expression
    | Operator   | x <operator> y            | (<operator> x y)
    |            | x <op> y <op> z           | (<op> x y z)
    |            | x <op1> y <op2> z         | (<op1>_<op2> x y z) (op1 != op2)
    | Apply      | x y                       | (apply x y)
    |            | x y z                     | (apply (apply x y) z)
    | List       | []                        | (list)
    |            | [x, ...]                  | (list x ...)
    | Apply+List | x[y, ...]                 | (x y ...)
    | Group      | {x}                       | x
    |            | {x, y, ...}               | (begin x y ...)
    | Arrow      | x => y                    | (x y) (for all x)
    | Contro     | @K : x                    | (K x)
    |            | @K x : y                  | (K x y)
    |            | @K x, y : {a, b, c}       | (K (begin x y) a b c)
    | Sexp       | (...)                     | (...)


Aliases
-------

    + Usual syntax         + Equivalent operator
    | @define spec: body   | spec = body
    | @lambda args: body   | args -> body
    | @set! var: value     | var := value
    | @quote: expr         | ..expr
    | @quasiquote: expr    | .expr
    | @unquote: expr       | ^expr
    | expt[x, y]           | x ** y
    | cons[x, y]           | x :: y
    | string-append[x, y]  | x ++ y
    | not[x == y]          | x /= y
    | or[a, b, c]          | a || b || c
    | and[a, b, c]         | a && b && c
    | not[x]               | ! x

