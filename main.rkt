#lang racket/base

(require scribble/core "manual-code.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out))

;; from Ben Lerner
(define-syntax (Cxx-block stx)
  (syntax-parse stx
    [(_ (~seq (~or (~optional
                    (~seq #:indent indent)
                    #:defaults ([indent #'0]))
                   (~optional
                    (~seq #:line-numbers line-numbers)
                    #:defaults ([line-numbers #'#f]))
                   (~optional
                    (~seq #:line-number-sep line-number-sep)
                    #:defaults ([line-number-sep #'1])))
              ...)
        args ...)
     #'(codeblock #:indent indent
                  #:line-numbers line-numbers
                  #:line-number-sep line-number-sep
                  #:keep-lang-line? #f
                  "#lang cxx-lexer\n" args ...)]))

(define-syntax-rule (Cxx args ...)
  (make-element (make-style "RktBlk" '(tt-chars))
                (code #:lang "cxx-lexer" args ...)))
