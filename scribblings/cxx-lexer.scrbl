#lang scribble/manual
@require[cxx-lexer
         @for-label[cxx-lexer
                    racket/base]]

@title{C++ Lexer}
@author{Tero Hasu}

@defmodule[cxx-lexer]

The @racket[Cxx] and @racket[Cxx-block] forms are used for typesetting C++ code.

@defform[(Cxx str-expr ...+)
         #:contracts ([str-expr string?])]{
Similar to the @racket[code] function.
Parses C++ code from strings into the inline text of the document.

For example,

@codeblock[#:keep-lang-line? #f]|<|{
  #lang scribble/manual
  This is @Cxx{1 + 2}.
}|>|

produces the typeset result:

@nested[#:style 'inset]{
  This is @Cxx{1 + 2}.
}

@racket[str-expr] is a list of strings representing C++ code.
}

@defform[(Cxx-block str-expr ...+)
         #:contracts ([str-expr string?])]{
Similar to the @racket[codeblock] function.
Parses C++ code from strings into a block in the document.

For example,

@codeblock[#:keep-lang-line? #f]|<|{
  #lang scribble/manual
  @Cxx-block|{
    int f(int x) {
      if (f <= 1) return 1;
      else        return x*f(x-1);
    }
  }|
}|>|

produces the typeset result:

@nested[#:style 'inset]{
  @Cxx-block|{
    int f(int x) {
      if (f <= 1) return 1;
      else        return x*f(x-1);
    }
  }|
}

@racket[str-expr] is a list of strings representing C++ code.
}

@section{Acknowledgements}

This software and document is derived from Leif Andersen's work on the @racketmodfont{r-lexer} package.
