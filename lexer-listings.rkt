#lang racket/base
(require scribble/racket
         scribble/base
         scribble/core
         (only-in scribble/struct make-blockquote)
         racket/pretty
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide
 (rename-out
  [codeblock lexcodeblock]
  [codeblock0 lexcodeblock0]
  [typeset-code typeset-lexcode]
  [code lexcode])
 foreign-keyword-color
 foreign-name-color
 foreign-symbol-color)

(define (make-color-style name)
  (style name '(tt-chars)))
(define foreign-keyword-color (make-color-style "FrgKw"))
(define foreign-name-color (make-color-style "FrgName"))
(define foreign-symbol-color (make-color-style "FrgSym"))

;; from scribble/private/manual-style.rkt
(define code-inset-style 
  (make-style 'code-inset null))
(define (code-inset b)
  (make-blockquote code-inset-style (list b)))

(define-syntax-rule (codeblock . args)
  (code-inset (typeset-code . args)))

(define-syntax-rule (codeblock0 . args)
  (typeset-code . args))

(define (typeset-code lexer ;; like `racket-lexer`
                      #:indent [indent 0]
                      #:line-numbers [line-numbers #f]
                      #:line-number-sep [line-number-sep 1]
                      #:block? [block? #t]
                      . strs)
  (define-values (tokens bstr) (get-tokens lexer strs))
  ;;(pretty-print (list tokens bstr))
  (define default-color meta-color)
  ((if block? table (lambda (style lines) (make-element #f lines)))
   block-color
   (list->lines
    indent
    #:line-numbers line-numbers
    #:line-number-sep line-number-sep
    #:block? block?
    (let loop ([pos 0]
               [tokens tokens])
      (cond
        [(null? tokens)
         (split-lines default-color (substring bstr pos))]
        [(eq? (caar tokens) 'white-space)
         (loop pos (cdr tokens))]
        [(= pos (cadar tokens))
         (append (let ([style (caar tokens)]
                       [get-str (lambda ()
                                  (substring bstr
                                             (cadar tokens)
                                             (caddar tokens)))])
                   (cond
                     [(symbol? style)
                      (let ([scribble-style
                             (case style
                               [(symbol) symbol-color]
                               [(keyword) keyword-color]
                               [(parenthesis hash-colon-keyword) paren-color]
                               [(constant string) value-color]
                               [(comment) comment-color]
                               [(frg-symbol) foreign-symbol-color]
                               [(frg-name) foreign-name-color]
                               [(frg-keyword) foreign-keyword-color]
                               [else default-color])])
                        (split-lines scribble-style (get-str)))]
                     [(procedure? style)
                      (list (style (get-str)))]
                     [else (list style)]))
                 (loop (caddar tokens) (cdr tokens)))]
        [(> pos (cadar tokens))
         (loop pos (cdr tokens))]
        [else (append
               (split-lines default-color (substring bstr pos (cadar tokens)))
               (loop (cadar tokens) tokens))])))))

;; (listof string) -> tokens string
;; tokens is a
;; (listof (list T natural natural natural))
;; T being a symbol returned as a token type from the languages lexer
;;   OR a function created by get-tokens
;; the first number being the start position
;; the second being the end position
;; the third 0 if T is a symbol, and 1 or greater if its a function or element
;; the tokens are sorted by the start and end positions
(define (get-tokens lexer strs)
  (let* ([bstr (regexp-replace* #rx"(?m:^$)"
                                (apply string-append strs)
                                "\xA0")]
         [tokens
          (let ([in (open-input-string bstr)])
            (port-count-lines! in)
            (let loop ()
              (let-values ([(lexeme type data start end)
                            (lexer in)])
                ;;(writeln (list lexeme type data start end))
                (if (equal? type 'eof)
                    null
                    (cons (list type (sub1 start) (sub1 end) 0)
                          (loop))))))])
    ;;(pretty-print `(tokens: ,tokens))
    (values tokens bstr)))

(define (code lexer . strs)
  (typeset-code lexer
   #:block? #f
   #:indent 0
   (let ([s (regexp-replace* #px"(?:\\s*(?:\r|\n|\r\n)\\s*)+"
                             (apply string-append strs) " ")])
     s)))

(define (split-lines style s)
  (cond
   [(regexp-match-positions #rx"(?:\r\n|\r|\n)" s)
    => (lambda (m)
         (append (split-lines style (substring s 0 (caar m)))
                 (list 'newline)
                 (split-lines style (substring s (cdar m)))))]
   [(regexp-match-positions #rx" +" s)
    => (lambda (m)
         (append (split-lines style (substring s 0 (caar m)))
                 (list (hspace (- (cdar m) (caar m))))
                 (split-lines style (substring s (cdar m)))))]
   [else (list (element style s))]))

(define omitable (make-style #f '(omitable)))

(define (list->lines indent-amt l 
                     #:line-numbers line-numbers
                     #:line-number-sep line-number-sep
                     #:block? block?)
  (define indent-elem (if (zero? indent-amt)
                          ""
                          (hspace indent-amt)))
  ;(list of any) delim -> (list of (list of any))
  (define (break-list lst delim)
    (let loop ([l lst] [n null] [c null])
      (cond
       [(null? l) (reverse (if (null? c) n (cons (reverse c) n)))]
       [(eq? delim (car l)) (loop (cdr l) (cons (reverse c) n) null)]
       [else (loop (cdr l) n (cons (car l) c) )])))

  (define lines (break-list l 'newline))
  (define line-cnt (length lines))
  (define line-cntl
    (string-length (format "~a" (+ line-cnt (or line-numbers 0)))))

  (define (prepend-line-number n r)
    (define ln (format "~a" n))
    (define lnl (string-length ln))
    (define diff (- line-cntl lnl))
    (define l1 (list (tt ln) (hspace line-number-sep)))
    (cons (make-element 'smaller 
                        (make-element 'smaller  
                                      (if (not (zero? diff))
                                          (cons (hspace diff) l1)
                                          l1)))
          r))

  (define (make-line accum-line line-number)
    (define rest (cons indent-elem accum-line))
    (list ((if block? paragraph (lambda (s e) e))
           omitable 
           (if line-numbers
               (prepend-line-number line-number rest)
               rest))))

  (for/list ([l (break-list l 'newline)]
             [i (in-naturals (or line-numbers 1))])
    (make-line l i)))

#|

This file was adapted from the "manual-code.rkt" file in Racket, and
it is used under the following license.

Copyright (c) 2010-2015 PLT Design Inc.
Copyright (c) 2015-2016 Tero

This file is distributed under the GNU Lesser General Public License
(LGPL). This means that you can link this software into proprietary
applications, provided you follow the rules stated in the LGPL. You
can also modify this software; if you distribute a modified version,
you must distribute it under the terms of the LGPL, which in
particular means that you must release the source code for the
modified software. See http://www.gnu.org/copyleft/lesser.html for
more information.

|#
