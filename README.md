
Liso
====

Liso is an alternative syntax for Lisp-like languages, implemented
here for Racket.


Examples
--------

There are several examples in the liso-rkt/examples/ subdirectory,
including examples of macros and operator macros.


Using
-----

To try it out, put the following line at the beginning of a source
file:

    #lang planet breuleux/liso

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

* It _does support macros. In fact, you can use Racket's macro system
  without any changes to their underlying logic. See
  [https://github.com/breuleux/liso/blob/master/liso-rkt/examples/macros.liso](here)
  for example.

