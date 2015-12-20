#lang info
(define collection "cxx-lexer")
(define deps '("base"
               "parser-tools-lib"
               "scribble-lib"
               "syntax-color-lib"))
(define build-deps '("racket-doc" "scribble-doc"))
(define scribblings '(("scribblings/cxx-lexer.scrbl" ())))
(define pkg-desc "Lexer-like thing for C++")
(define compile-omit-paths '("experiments"))
