#lang racket/base

(require scribble/core "manual-code.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide Cxx-block Cxx)

(define-syntax (Cxx-block stx)
  (define (maybe kw-stx attr-val)
    (if attr-val (list kw-stx attr-val) null))
  
  (syntax-parse stx
    [(_ (~seq (~or (~optional (~seq #:indent indent))
                   (~optional (~seq #:line-numbers line-numbers))
                   (~optional (~seq #:line-number-sep line-number-sep)))
              ...)
        args ...)
     #`(codeblock #,@(maybe #'#:indent (attribute indent))
                  #,@(maybe #'#:line-numbers (attribute line-numbers))
                  #,@(maybe #'#:line-number-sep (attribute line-number-sep))
                  #:keep-lang-line? #f
                  "#lang cxx-lexer\n" args ...)]))

;; from Ben Lerner
(define-syntax-rule (Cxx args ...)
  (make-element (make-style "RktBlk" '(tt-chars))
                (code #:lang "cxx-lexer" args ...)))
