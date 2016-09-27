#lang racket/base

(provide cxx-read cxx-read-syntax cxx-get-info
         cxx-lexer)

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

(define (cxx-lexer in)
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
     cxx-lexer]
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
   (Identifier (re:: RLetter (re:* RLetterOrDigit)))
   (RLetter (re:or (re:/ "AZ" "az") "_" "$"))
   (RLetterOrDigit (re:or RLetter (re:/ "09")))
   (KnownNames
    (re:or
     ;; C++ type names
     "bool" "char" "double" "float" "int" "long"
     "short" "signed" "unsigned" "void"
     ;; namespace names
     "std"
     ;; `std` type names
     "std::function" "std::string" "std::vector" "std::wstring"
     ;; `std` function names
     "std::make_shared"
     ;; Qt type names
     "qreal" "QCompass" "QObject" "QString" "QWidget"
     ;; Qt method names
     "QObject::connect"
     ;; Qt slot names
     ;;"readingChanged"
     ;; Symbian type names
     "CActive" "CBase" "CleanupStack" "ELeave" "TInt" "TTime" "User"
     ;; Symbian method names
     "CleanupStack::PushL" "CleanupStack::Pop" "CleanupStack::PopAndDestroy"
     "User::LeaveIfError"
     ))
   (Keyword
    (re:or
     ;; C syntax
     "FALSE" "NULL" "TRUE"
     ;; C++ syntax
     "auto" "class" "const" "const_cast" "constexpr" "decltype" "delete" "dynamic_cast" "else" "enum" "explicit" "false" "for" "goto" "if" "inline" "new" "nullptr" "private" "protected" "public" "reinterpret_cast" "return" "static" "static_assert" "static_cast" "struct" "switch" "template" "this" "true" "typedef" "typeid" "typename" "union" "virtual" "while"
     ;; CPP syntax
     "#if" "#else" "#elif" "#endif" "#ifdef" "#ifndef" "#define" "#undef" "#include" "#line"
     ;; Magnolisp syntax
     "MGL_PROTO" "MGL_FUNC" "MGL_API_PROTO" "MGL_API_FUNC"
     ;; Qt syntax
     "emit" "Q_DECL_CONSTEXPR" "Q_DECL_DELETE" "Q_DECL_FINAL" "Q_DECL_OVERRIDE" "Q_OBJECT" "Q_PROPERTY" "Q_STATIC_ASSERT" "qobject_cast" "SIGNAL" "signals" "SLOT" "slots"))
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
   ((re:or ;;(re:: #\' (re:~ CR LF #\' #\\) #\')
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
   (Keyword (syn-val lexeme 'frg-keyword #f start-pos end-pos))
   (KnownNames (syn-val lexeme 'frg-name #f start-pos end-pos))
   (Identifier (syn-val lexeme 'frg-symbol #f start-pos end-pos))
   ("//" (syn-val lexeme 'comment #f start-pos (read-line-comment input-port)))
   ((re:+ WhiteSpace) (syn-val lexeme 'white-space #f start-pos end-pos))
   ;;(#\032 (values lexeme 'eof #f start-pos end-pos))
   ((eof) (values lexeme 'eof #f start-pos end-pos))
   (any-char (syn-val lexeme 'error #f start-pos end-pos))))

#|

Copyright (c) 2015-2016 leif, Tero

This file is distributed under the GNU Lesser General Public
License (LGPL). This means that you can link this software into
proprietary applications, provided you follow the rules stated in the
LGPL. You can also modify this software; if you distribute a modified
version, you must distribute it under the terms of the LGPL, which in
particular means that you must release the source code for the
modified software. See http://www.gnu.org/copyleft/lesser.html for
more information.

|#
