#lang info
(define collection "lexer-listings")
(define deps '("base"
               "parser-tools-lib"
               "scribble-lib"
               "syntax-color-lib"))
(define build-deps '("racket-doc" "scribble-doc" "syntax-color-doc"))
(define scribblings '(("scribblings/lexer-listings.scrbl" ())))
(define pkg-desc "Scribble forms for color-lexer colored code listings.")
(define compile-omit-paths '("experiments"))
