#lang racket/base
(require syntax-color/module-lexer
         syntax-color/lexer-contract
         scribble/racket
         scribble/base
         scribble/private/manual-scheme
         scribble/private/manual-style
         scribble/core
         racket/pretty
         (for-syntax racket/base
                     syntax/parse))

(provide codeblock
         codeblock0
         typeset-code
         code
         current-keyword-style)

(define-for-syntax (do-codeblock stx)
  (syntax-parse stx
    [(_ (~seq (~or (~optional
                    (~seq #:indent indent-expr:expr)
                    #:defaults ([indent-expr #'0])
                    #:name "#:indent keyword")
                   (~optional
                    (~seq #:keep-lang-line? keep-lang-line?-expr:expr)
                    #:defaults ([keep-lang-line?-expr #'#t])
                    #:name "#:keep-lang-line? keyword")
                   (~optional
                    (~seq #:line-numbers line-numbers:expr)
                    #:defaults ([line-numbers #'#f])
                    #:name "#:line-numbers keyword")
                   (~optional
                    (~seq #:line-number-sep line-number-sep:expr)
                    #:defaults ([line-number-sep #'1])
                    #:name "#:line-number-sep keyword"))
              ...)
        str ...)
     #'(typeset-code str ...
                     #:keep-lang-line? keep-lang-line?-expr
                     #:indent indent-expr
                     #:line-numbers line-numbers
                     #:line-number-sep line-number-sep)]))

(define-syntax (codeblock stx) #`(code-inset #,(do-codeblock stx)))
(define-syntax (codeblock0 stx) (do-codeblock stx))

(define current-keyword-style (make-parameter value-link-color))

(define (typeset-code #:indent [indent 2]
                      #:keep-lang-line? [keep-lang-line? #t]
                      #:line-numbers [line-numbers #f]
                      #:line-number-sep [line-number-sep 1]
                      #:block? [block? #t]
                      . strs)
  (define-values (tokens bstr) (get-tokens strs))
  ;;(pretty-print (list tokens bstr))
  (define default-color meta-color)
  ((if block? table (lambda (style lines) (make-element #f lines)))
   block-color
   ((if keep-lang-line? values cdr) ; FIXME: #lang can span lines
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
                                [(keyword) (current-keyword-style)]
                                [(parenthesis hash-colon-keyword) paren-color]
                                [(constant string) value-color]
                                [(comment) comment-color]
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
                (loop (cadar tokens) tokens))]))))))

;; (listof string) -> tokens string
;; tokens is a
;; (listof (list T natural natural natural))
;; T being a symbol returned as a token type from the languages lexer
;;   OR a function created by get-tokens
;; the first number being the start position
;; the second being the end position
;; the third 0 if T is a symbol, and 1 or greater if its a function or element
;; the tokens are sorted by the start and end positions
(define (get-tokens strs)
  (let* ([bstr (regexp-replace* #rx"(?m:^$)"
                                (apply string-append strs)
                                "\xA0")]
         [tokens
          (let ([in (open-input-string bstr)])
            (port-count-lines! in)
            (let loop ([mode #f])
              (let-values ([(lexeme type data start end backup-delta mode)
                            (module-lexer in 0 mode)])
                (if (equal? type 'eof)
                    null
                    (cons (list type (sub1 start) (sub1 end) 0)
                          (loop (if (dont-stop? mode)
                                    (dont-stop-val mode)
                                    mode)))))))]
         [program-source 'prog]
         [e (parameterize ([read-accept-reader #t])
              (let ([p (open-input-string bstr)])
                (port-count-lines! p)
                (let loop ()
                  (let ([v (read-syntax program-source p)])
                    (cond
                      [(eof-object? v) null]
                      [else (datum->syntax #f (cons v (loop)) v v)])))))]
         [link-mod (lambda (mp-stx priority #:orig? [always-orig? #f])
                     (if (or always-orig?
                             (syntax-original? mp-stx))
                         (let ([mp (syntax->datum mp-stx)]
                               [pos (sub1 (syntax-position mp-stx))])
                           (list (list (racketmodname #,mp)
                                       pos
                                       (+ pos (syntax-span mp-stx))
                                       priority)))
                         null))]
         ;; This makes sense when `expand' actually expands, and
         ;; probably not otherwise:
         [mods (let loop ([e e])
                 (syntax-case e (module #%require begin)
                   [(module name lang (mod-beg form ...))
                    (apply append
                           (link-mod #'lang 2)
                           (map loop (syntax->list #'(form ...))))]
                   [(#%require spec ...)
                    (apply append
                           (map (lambda (spec)
                                  ;; Need to add support for renaming forms, etc.:
                                  (if (module-path? (syntax->datum spec))
                                      (link-mod spec 2)
                                      null))
                                (syntax->list #'(spec ...))))]
                   [(begin form ...)
                    (apply append
                           (map loop (syntax->list #'(form ...))))]
                   [else null]))]
         [has-hash-lang?
          (regexp-match? #rx"^#lang " bstr)]
         [hash-lang
          (if has-hash-lang?
              (list (list (hash-lang)
                          0
                          5
                          1)
                    (list 'white-space 5 6 0))
              null)]
         [language
          (if has-hash-lang?
              (let ([m (regexp-match #rx"^#lang ([-0-9a-zA-Z/._+]+)" bstr)])
                (if m
                    (link-mod
                     #:orig? #t
                     (datum->syntax #f
                                    (string->symbol (cadr m))
                                    (vector 'in 1 6 7 (string-length (cadr m))))
                     3)
                    null))
              null)]
         [m-tokens
          (sort (append mods
                        hash-lang
                        language
                        (if has-hash-lang?
                            ;; Drop #lang entry:
                            (cdr tokens)
                            tokens))
                (lambda (a b)
                  (or (< (cadr a) (cadr b))
                      (and (= (cadr a) (cadr b))
                           (> (cadddr a) (cadddr b))))))])
    ;;(pretty-print `(tokens: ,tokens))
    ;;(pretty-print `(tokens: ,m-tokens))
    (values m-tokens bstr)))

(define (typeset-code-line lang-line . strs)
  (typeset-code
   #:keep-lang-line? (not lang-line)
   #:block? #f
   #:indent 0
   (let ([s (regexp-replace* #px"(?:\\s*(?:\r|\n|\r\n)\\s*)+"
                             (apply string-append strs) " ")])
     (if lang-line
         (string-append "#lang " lang-line "\n" s)
         s))))

(define-syntax (code stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:lang lang-line-expr:expr)
                   #:defaults ([lang-line-expr #'#f]))
        str ...)
     #'(typeset-code-line lang-line-expr
                          str ...)]))

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

Racket is distributed under the GNU Lesser General Public License
(LGPL).  This means that you can link Racket into proprietary
applications, provided you follow the rules stated in the LGPL.  You can
also modify Racket; if you distribute a modified version, you must
distribute it under the terms of the LGPL, which in particular means
that you must release the source code for the modified software.  See
share/COPYING_LESSER.txt for more information.

|#
