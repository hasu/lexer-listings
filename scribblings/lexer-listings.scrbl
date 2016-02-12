#lang scribble/manual
@require[scribble/manual scribble/racket
         lexer-listings lexer-listings/cxx
         @for-label[racket/base
		    scribble/core scribble/manual scribble/racket
		    syntax-color/default-lexer
		    syntax-color/racket-lexer
                    lexer-listings]]

@(current-lexcode-keyword-style value-link-color)

@title{Lexer Listings}
@author{Tero Hasu}

This library implements something similar to @racketmodname[scribble/manual] module's @racket[codeblock], @racket[codeblock0], @racket[code], and @racket[typeset-code]. However, the typesetting forms and functions here assume foreign code, and no @hash-lang[] to specify the language used. Instead, one must explicitly specify a color lexer function to use, one with the same signature as that of @racket[default-lexer] and @racket[racket-lexer].

This package presently includes only one lexer, namely the @racket[cxx-lexer] in the @racketmodfont{lexer-listings/cxx} module.

@defmodule[lexer-listings]

The @racket[lexcodeblock], @racket[lexcodeblock0], @racket[lexcode], and @racket[typeset-lexcode] forms are used for typesetting foreign code, with a lexer explicitly specified as the first argument. The styling of @racket['keyword] tokens can be specified via the @racket[current-lexcode-keyword-style] parameter.

@defform[(lexcode lexer-expr str-expr ...+)
         #:contracts ([str-expr string?])]{
Similar to @racket[code], but 
parses code from strings into the inline text of the document
using the specified lexer.

For example,

@codeblock[#:keep-lang-line? #f]|<|{
  #lang scribble/manual
  This is @lexcode[cxx-lexer]{1 + 2}.
}|>|

produces the typeset result:

@nested[#:style 'inset]{
  This is @lexcode[cxx-lexer]{1 + 2}.
}

@racket[str-expr] is a list of strings representing code.}

@defform[(lexcodeblock lexer-expr str-expr ...+)
         #:contracts ([str-expr string?])]{
Similar to @racket[codeblock], but 
parses code from strings into a block in the document
using the specified lexer.

For example,

@codeblock[#:keep-lang-line? #f]|<|{
  #lang scribble/manual
  @lexcodeblock[cxx-lexer]|{
    int f(int x) {
      if (f <= 1) return 1;
      else        return x*f(x-1);
    }
  }|
}|>|

produces the typeset result:

@nested[#:style 'inset]{
  @lexcodeblock[cxx-lexer]|{
    int f(int x) {
      if (f <= 1) return 1;
      else        return x*f(x-1);
    }
  }|
}

@racket[str-expr] is a list of strings representing code.}

@defform[(lexcodeblock0 lexer-expr option ... str-expr ...+)]{
Similar to @racket[codeblock0], but uses the specified lexer.}

@defproc[(typeset-lexcode [strs string?] ...) block?]{
Similar to @racket[typeset-lexcode], but uses the specified lexer.}

@defparam[current-lexcode-keyword-style style element-style?]{
A parameter that controls the style used to render symbols that are keywords or perhaps otherwise predefined names. The default is to use @racket[keyword-color] (mapping to @tt{RktKw} in LaTeX).

One might also consider @racket[value-link-color] (or @tt{RktValLink}), to make known names look similar to @racket[for-label] defined names in Racket listings (as is configured for this document). The @tt{RktValLink} choice could cause confusion, however, since symbols so styled look like links, but do not get linked to anything.

See @secref["manual-css" #:doc '(lib "scribblings/scribble/scribble.scrbl")] for a list of Racket manual styles you might use, or come up with your own alternative @racket[style?] definition.}

@section{Acknowledgements}

This document and parts of the software are based on Leif Andersen's work on the @racketmodfont{r-lexer} package. The rest of the software is based on code from Racket's @racketmodfont{scribble-lib} package.

@; Copyright (c) 2015 leif, Tero
@; 
@; This file is distributed under the GNU Lesser General Public
@; License (LGPL). This means that you can link this software into
@; proprietary applications, provided you follow the rules stated in the
@; LGPL. You can also modify this software; if you distribute a modified
@; version, you must distribute it under the terms of the LGPL, which in
@; particular means that you must release the source code for the
@; modified software. See http://www.gnu.org/copyleft/lesser.html for
@; more information.
