
#lang scribble/doc

@require[scribble/manual]

@title{LISO: an alternative to s-expressions}
@author[(author+email "Olivier Breuleux" "olivier@breuleux.net")]

@hyperlink["https://github.com/breuleux/liso"]{Source and additional documentation}

@hyperlink["https://github.com/breuleux/liso/tree/master/liso/examples"]{Examples}

Use it like this:

@codeblock|{
  #lang planet breuleux/liso

  fib(n) =
     if {n <= 1}:
        then: n
        else: fib(n - 1) + fib(n - 2)

  fib(30)
}|

So far, it only works with the @racket[racket] language, not
@racket[racket/base] or @racket[typed/racket].

