#lang racket/base

(provide cxx-read
         cxx-read-syntax
         cxx-get-info)

(require parser-tools/lex
         (prefix-in re: parser-tools/lex-sre))

(define (cxx-read in)
  (syntax->datum (cxx-read-syntax #f in)))

(define (cxx-read-syntax src in)
  (let-values ([(lexeme type data start end) (get-syntax-token in)])
    (if (eq? type 'eof)
        eof
        (datum->syntax #f
                       type
                       (list src #f #f start (- end start))))))

(define (get-color-lexer in)
  (let-values ([(lexeme type data start end) (get-syntax-token in)])
    ;;(writeln (list lexeme type data start end))
    (values lexeme
            type
            data
            start
            end)))

(define (cxx-get-info key default default-filter)
  (case key
    [(color-lexer)
     get-color-lexer]
    [else
     (default-filter key default)]))

(define (syn-val lex a b c d)
  (values lex a b (position-offset c) (position-offset d)))

(define-lex-abbrevs
   (Digits (re:+ (re:/ "09")))
   (DigitsOpt (re:* (re:/ "09")))

   (IntegerTypeSuffix (char-set "lL"))
   (DecimalNumeral (re:or #\0
                          (re:: (re:/ "19") (re:* (re:/ "09")))))
   (HexDigit (re:/ "09" "af" "AF"))
   (HexNumeral (re:: #\0 (char-set "xX") (re:+ HexDigit)))
   (OctalNumeral (re:: #\0 (re:+ (re:/ "07"))))

   ;; 3.10.2
   (FloatTypeSuffix (char-set "fF"))
   (DoubleTypeSuffix (char-set "dD"))

   (FloatA (re:: Digits #\. DigitsOpt (re:? ExponentPart)))
   (FloatB (re:: #\. Digits (re:? ExponentPart)))
   (FloatC (re:: Digits ExponentPart))
   (FloatD (re:: Digits (re:? ExponentPart)))

   (ExponentPart (re:: (char-set "eE") (re:? (char-set "+-")) Digits))
   (EscapeSequence (re:or "\\b" "\\t" "\\n" "\\f" "\\r" "\\\"" "\\'" "\\\\"
                          (re:: #\\ (re:? (re:/ "03")) (re:/ "07") (re:/ "07"))
                          (re:: #\\ (re:/ "07"))))
   (Identifier (re:: RLetter (re:* RLetterOrDigit))) ;; `RktSym`, see "Manual CSS Style Classes"
   (RLetter (re:or (re:/ "AZ" "az") "_" "$"))
   (RLetterOrDigit (re:or RLetter (re:/ "09")))
   (KnownNames
    (re:or
     ;; namespace names
     "std"
     ;; function names
     "make_shared"
     ;; type names
     "bool" "char" "double" "float" "int" "long"
     "short" "signed" "string" "unsigned" "void"))
   (Keyword (re:or "auto" "class" "const" "const_cast" "dynamic_cast" "else" "enum" "explicit" "for" "goto" "if" "private" "protected" "public" "reinterpret_cast" "return" "static_cast" "struct" "switch" "template" "typedef" "typeid" "union" "virtual" "while" "MGL_PROTO" "MGL_FUNC" "MGL_API_PROTO" "MGL_API_FUNC" KnownNames)) ;; `RktValLink` (or `RktStxLink`)
   (Operator (re:or "=" ">" "<" "!" "~" "?" "::" ":"
                    "==" "<=" ">=" "!=" "&&" "||" "+"
                    "-" "*" "/" "&" "|" "^" "%" "<<" ">>"))
   (CR #\015)
   (LF #\012)
   (LineTerminator (re:or CR
                          LF
                          (re:: CR LF)))
   (InputCharacter (re:~ CR LF))
   (FF #\014)
   (TAB #\011)
   (NBSP #\uA0)
   (WhiteSpace (re:or #\space
                      TAB
                      FF
                      LineTerminator
                      NBSP)))

(define read-line-comment
  (lexer
   [(re:~ #\newline) (read-line-comment input-port)]
   [#\newline end-pos]
   [(eof) end-pos]
   [(special) (read-line-comment input-port)]
   [(special-comment) (read-line-comment input-port)]))

(define (colorize-double-string my-start-pos)
  (lexer
   (#\" (syn-val "" 'string #f my-start-pos end-pos))
   ((re:or CR LF) (syn-val "" 'error #f my-start-pos end-pos))
   ((eof) (syn-val "" 'error #f my-start-pos end-pos))
   (EscapeSequence ((colorize-double-string my-start-pos) input-port))
   (InputCharacter ((colorize-double-string my-start-pos) input-port))))

(define (colorize-single-string my-start-pos)
  (lexer
   (#\' (syn-val "" 'string #f my-start-pos end-pos))
   ((re:or CR LF) (syn-val "" 'error #f my-start-pos end-pos))
   ((eof) (syn-val "" 'error #f my-start-pos end-pos))
   (EscapeSequence ((colorize-single-string my-start-pos) input-port))
   (InputCharacter ((colorize-single-string my-start-pos) input-port))))

(define get-syntax-token
  (lexer
   (Operator (syn-val lexeme 'parenthesis #f start-pos end-pos))
   ((char-set "(){}[];,.")
    (syn-val lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos))
   ((re:or "nullptr" "true" "false"
           ;;(re:: #\' (re:~ CR LF #\' #\\) #\')
           ;;(re:: #\' EscapeSequence #\')
           FloatA FloatB FloatC
           (re:: (re:or FloatA FloatB FloatC FloatD) FloatTypeSuffix)
           (re:: (re:or FloatA FloatB FloatC FloatD) FloatTypeSuffix)
           DecimalNumeral
           HexNumeral
           OctalNumeral
           (re:: DecimalNumeral IntegerTypeSuffix)
           (re:: HexNumeral IntegerTypeSuffix)
           (re:: OctalNumeral IntegerTypeSuffix)
           )
    (syn-val lexeme 'constant #f start-pos end-pos))
   ;;(#\' ((colorize-single-string start-pos) input-port))
   (#\" ((colorize-double-string start-pos) input-port))
   (Keyword (syn-val lexeme 'keyword #f start-pos end-pos))
   (Identifier (syn-val lexeme 'symbol #f start-pos end-pos))
   ("//" (syn-val lexeme 'comment #f start-pos (read-line-comment input-port)))
   ((re:+ WhiteSpace) (syn-val lexeme 'white-space #f start-pos end-pos))
   ;;(#\032 (values lexeme 'eof #f start-pos end-pos))
   ((eof) (values lexeme 'eof #f start-pos end-pos))
   (any-char (syn-val lexeme 'error #f start-pos end-pos))))
