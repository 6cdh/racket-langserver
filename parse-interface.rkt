#lang racket/base
(require (for-syntax racket/base
                     compiler/cm-accomplice
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/parse
                     syntax/location
                     parser-tools/yacc
                     parser-tools/lex
                     (prefix-in : parser-tools/lex-sre))
         racket/format
         racket/match)

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ f:str)
     (quasisyntax/loc stx
       (#%module-begin
        #,@(compile-interface
            stx
            (parse-interface
             (build-path (syntax-source-directory #'f)
                         (syntax-e #'f))))))]))

(begin-for-syntax
  (define-tokens program-tokens
    (NAME))
  (define-empty-tokens program-punc
    (INTERFACE LBRACE RBRACE SEMI COLON EOF LCOMMENT RCOMMENT))

  (define-lex-abbrev id-chars (char-complement (char-set "(,)=;:.~?\"% \n")))
  (define-lex-abbrev variable-re (:: id-chars (:* id-chars)))
  (define-lex-abbrev comment-re
    (:or (:: "//" any-string (:or #\newline #\return ""))
         (:: "/*" (complement (:: any-string "*/" any-string)) "*/")))
         

  (define program-lexer
    (lexer-src-pos
     [(union whitespace comment-re)
      (return-without-pos (program-lexer input-port))]
     ["interface" (token-INTERFACE)]
     ["{" (token-LBRACE)]
     ["}" (token-RBRACE)]
     [";" (token-SEMI)]
     [":" (token-COLON)]
     [(eof) (token-EOF)]
     [variable-re
      (token-NAME (string->symbol lexeme))]))

  (define (make-srcloc start-pos end-pos)
    (list (file-path)
          (position-line start-pos)
          (position-col start-pos)
          (position-offset start-pos)
          (- (position-offset end-pos) (position-offset start-pos))))

  (define program-parser
    (parser (start program)
            (end EOF)
            (tokens program-tokens program-punc)
            (src-pos)
            (error
             (λ (tok-ok? tok-name tok-value start-pos end-pos)
               (raise-syntax-error
                'interface
                (if tok-ok?
                    (format "Unexpected token ~S" tok-name)
                    (format "Invalid token ~S" tok-name))
                (datum->syntax #f tok-value (make-srcloc start-pos end-pos)))))
            (grammar
             (program [(interfaces) $1])
             (interfaces [() (hasheq)]
                         [(INTERFACE NAME LBRACE fields RBRACE SEMI interfaces)
                          (hash-set $7 $2 $4)])
             (fields [() (hasheq)]
                     [(NAME COLON NAME SEMI fields)
                      (hash-set $5 $1 $3)]))))

  (define (parse-interface f)
    (parameterize ([file-path f])
      (register-external-file f)
      (call-with-input-file f
        (λ (ip)
          (port-count-lines! ip)
          (program-parser (λ () (program-lexer ip)))))))

  (define (type-name->expander stx ty field_)
    (match ty
      ['boolean (quasisyntax/loc stx (? boolean? #,field_))]
      ['number (quasisyntax/loc stx (? number? #,field_))]
      ['string (quasisyntax/loc stx (? string? #,field_))]
      [_ field_]))
  
  (define (compile-interface stx ifaces)
    (eprintf "~v\n" ifaces)
    (for/list ([(type-name type-def)
                (in-hash ifaces)])
      (with-syntax ([type-name-stx (datum->syntax stx type-name)]
                    [([field field-pat field_] ...)
                     (for/list ([(field-name field-type)
                                 (in-hash type-def)])
                       (define field_ (format-id stx "~a_" field-name))
                       (list
                        field-name
                        (type-name->expander stx field-type field_)
                        field_))])
        (syntax/loc stx
          (begin
            (define-match-expander type-name-stx
              (λ (stx)
                (syntax-parse stx
                  [(_ field_ ...)
                   #'(hash-table ['field field-pat] ...)])))
            (provide type-name-stx)))))))

(provide
 (rename-out [module-begin #%module-begin]))