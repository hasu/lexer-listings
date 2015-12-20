#lang scribble/manual
@require[cxx-lexer
         @for-label[cxx-lexer
                    racket/base]]

@title{C++ Lexer}
@author{Tero Hasu}

Well this is not really a C++ lexer. No attempt has been made to be correct or complete, but this just happens to work for the author's use cases at this time.

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

This document and parts of the software is based on Leif Andersen's work on the @racketmodfont{r-lexer} package. The rest of the software is based on code from Racket's @racketmodfont{scribble-lib} package.
