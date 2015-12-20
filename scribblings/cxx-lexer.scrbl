#lang scribble/manual
@require[cxx-lexer
         @for-label[cxx-lexer
		    scribble/core
                    racket/base]]

@title{C++ Lexer}
@author{Tero Hasu}

Well this is not really a C++ lexer. No attempt has been made to be correct or complete, but this just happens to work for the author's use cases at this time. Also, the code is horribly fragile, as it refers to internal Scribble APIs; if you're not using Racket 6.3 with stock packages, then best of luck.

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

@defparam[current-keyword-style style element-style?]{
A parameter that controls the style used to render symbols that are C++ keywords or otherwise common names. The default is to use the @tt{RktValLink} Racket manual style, to make known C++ names look similar to Racket ones.

The @tt{RktValLink} choice may cause confusion, however, since symbols so styled look like links, but do not get linked to anything. If that doesn't work for your use case, see @secref["manual-css" #:doc '(lib "scribblings/scribble/scribble.scrbl")] for a list of Racket manual styles you might use, or come up with your own alternative.}

@section{Acknowledgements}

This document and parts of the software are based on Leif Andersen's work on the @racketmodfont{r-lexer} package. The rest of the software is based on code from Racket's @racketmodfont{scribble-lib} package.
