#lang info
(define collection "cxx-lexer")
(define deps '("base"
               "rackunit-lib"
               "scribble-lib"
               "parser-tools-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/cxx-lexer.scrbl" ())))
(define pkg-desc "Lexer for C++")
(define compile-omit-paths '("experiments"))
