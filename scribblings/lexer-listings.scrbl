#lang scribble/manual
@require[scribble/manual scribble/racket
         lexer-listings lexer-listings/cxx
         @for-label[racket/base
		    scribble/core scribble/manual scribble/racket
		    syntax-color/default-lexer
		    syntax-color/racket-lexer
                    lexer-listings]]

@title{Lexer Listings}
@author{Tero Hasu}

This library implements something similar to @racketmodname[scribble/manual] module's @racket[codeblock], @racket[codeblock0], @racket[code], and @racket[typeset-code]. However, the typesetting forms and functions here assume foreign code, and no @hash-lang[] to specify the language used. Instead, one must explicitly specify a color lexer function to use, one with the same signature as that of @racket[default-lexer] and @racket[racket-lexer].

This package presently includes only one lexer, namely the @racket[cxx-lexer] in the @racketmodfont{lexer-listings/cxx} module.

@section{Language-Agnostic API}

@defmodule[lexer-listings]

The @racket[lexcodeblock], @racket[lexcodeblock0], @racket[lexcode], and @racket[typeset-lexcode] forms are used for typesetting foreign code, with a lexer explicitly specified as the first argument.

@defform[(lexcode lexer-expr str-expr ...+)
         #:contracts ([str-expr string?])]{
Similar to @racket[code], but parses code from strings into the inline text of the document using the specified lexer. The @racket[str-expr]essions must yield strings of code to be tokenized.

For example,

@codeblock[#:keep-lang-line? #f]|<|{
  #lang scribble/manual
  This is @lexcode[cxx-lexer]{1 + 2}.
}|>|

produces the typeset result:

@nested[#:style 'inset]{
  This is @lexcode[cxx-lexer]{1 + 2}.
}}

@defform[(lexcodeblock lexer-expr str-expr ...+)
         #:contracts ([str-expr string?])]{
Similar to @racket[codeblock], but parses code from strings into a block in the document using the lexer specified by @racket[lexer-expr]. The @racket[str-expr]essions must yield strings of code to be tokenized.

For example,

@codeblock[#:keep-lang-line? #f]|<|{
  #lang scribble/manual
  @lexcodeblock[cxx-lexer]|{
    int f(int const& x) {
      if (f <= 1) return 1;
      else        return x*f(x-1);
    }
  }|
}|>|

produces the typeset result:

@nested[#:style 'inset]{
  @lexcodeblock[cxx-lexer]|{
    int f(int const& x) {
      if (f <= 1) return 1;
      else        return x*f(x-1);
    }
  }|
}}

@defform[(lexcodeblock0 lexer-expr option ... str-expr ...+)]{
Similar to @racket[codeblock0], but uses the specified lexer.}

@defproc[(typeset-lexcode [strs string?] ...) block?]{
Similar to @racket[typeset-code], but uses the specified lexer.}

@deftogether[
 (@defthing[foreign-keyword-color style?]
  @defthing[foreign-name-color style?]
  @defthing[foreign-symbol-color style?])]{
Additional styles supported by @racket[typeset-lexcode] (in addition to Racket's @racket[meta-color], @racket[value-color], @etc).
These are intended to be used for foreign keywords, other predefined names, and non-predefined names, respectively.
These styles correspond to the additional token types @racket['frg-keyword], @racket['frg-name], @racket['frg-symbol], as may be assigned by a language-specific color lexer.

When rendering a document, the styles translate as @tt{FrgKw}, @tt{FrgName}, and @tt{FrgSym}. If tokens of the associated types are emitted, definitions for those style classes must then be provided in a renderer-specific way.
See @secref["manual-css" #:doc '(lib "scribblings/scribble/scribble.scrbl")] for a list of Racket manual style classes, which can serve as examples for providing definitions for @tt{FrgKw}, etc. See this package's @filepath{style.css} for an example definition for CSS.}

@section{Acknowledgements}

This document and parts of the software are based on Leif Andersen's work on the @racketmodfont{r-lexer} package. The rest of the software is based on code from Racket's @racketmodfont{scribble-lib} package.

@; Copyright (c) 2015-2016 leif, Tero
@; 
@; This file is distributed under the GNU Lesser General Public
@; License (LGPL). This means that you can link this software into
@; proprietary applications, provided you follow the rules stated in the
@; LGPL. You can also modify this software; if you distribute a modified
@; version, you must distribute it under the terms of the LGPL, which in
@; particular means that you must release the source code for the
@; modified software. See http://www.gnu.org/copyleft/lesser.html for
@; more information.
