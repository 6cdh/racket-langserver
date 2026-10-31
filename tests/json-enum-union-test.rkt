#lang racket

(module+ test
  (require rackunit
           "../common/json-util.rkt")

  ;; define-json-enum
  (define-json-enum SymbolKind
    [file 1]
    [module 2]
    [constant 14])

  (test-case "enum: construction and predicate"
    (check-true (SymbolKind? SymbolKind-constant))
    (check-equal? (SymbolKind-v SymbolKind-file) 'file))

  (test-case "enum: variant aliases"
    (check-equal? SymbolKind-file (SymbolKind 'file))
    (check-equal? SymbolKind-module (SymbolKind 'module))
    (check-equal? SymbolKind-constant (SymbolKind 'constant)))

  (test-case "enum: encoding and json predicate"
    (check-equal? (->jsexpr SymbolKind-constant) 14)
    (check-equal? (->jsexpr SymbolKind-file) 1)
    (check-true (SymbolKind-js? 14))
    (check-true (SymbolKind-js? 1))
    (check-false (SymbolKind-js? 999)))

  (test-case "enum: decode and json matcher"
    (check-equal? (jsexpr->SymbolKind 1) SymbolKind-file)
    (check-equal? (jsexpr->SymbolKind 14) SymbolKind-constant)
    (check-exn exn:fail? (lambda () (jsexpr->SymbolKind 999))))

  (test-case "enum: jsexpr match styles"
    (check-equal? (match 1 [(SymbolKind-js 'file) 'ok] [_ #f]) 'ok)
    (check-equal? (match 14 [(? SymbolKind-js? (SymbolKind-js k)) k] [_ #f]) 'constant))

  (test-case "enum: as-Name"
    (check-equal? (match 1 [(as-SymbolKind k) (SymbolKind-v k)] [_ #f]) 'file)
    (check-false (match 999 [(as-SymbolKind _) #t] [_ #f])))

  (test-case "enum: ^Name alias"
    (check-equal? (match 1 [(^SymbolKind 'file) 'ok] [_ #f]) 'ok)
    (check-false (match 999 [(^SymbolKind _) #t] [_ #f])))

  ;; define-json-union
  (define-json-struct Markup
    [kind string?]
    [value string?])

  ;; Union of primitive + compound
  (define-json-union DocContent string? Markup)

  (define-json-struct DocBox
    [payload DocContent])

  (define-json-union NestedContent (optional (listof (union string? Markup))))
  (define-json-union TableContent (hashof symbol? Markup))
  (define-json-union CallbackContent string? (contract (-> integer? integer?)))
  (define-json-union LiteralContent (contract 'ready) string?)

  (test-case "composed predicates validate complete container contents"
    (check-true (NestedContent? (Nothing)))
    (check-true (NestedContent? (list "text" (Markup "md" "value"))))
    (check-false (NestedContent? (list "text" 1)))
    (check-true (NestedContent-js? (list "text" (hasheq 'kind "md" 'value "value"))))
    (check-false (NestedContent-js? (list (hasheq 'kind "md" 'value 1))))
    (check-true (TableContent? (hasheq 'a (Markup "md" "value"))))
    (check-false (TableContent? (hasheq 'a "bad")))
    (check-false (TableContent? (hash "bad-key" (Markup "md" "value"))))
    (check-true (TableContent-js? (hasheq 'a (hasheq 'kind "md" 'value "value"))))
    (check-false (TableContent-js? (hasheq 'a (hasheq 'kind "md" 'value 1)))))

  (test-case "explicit contracts support first-order membership tests"
    (check-true (CallbackContent? add1))
    (check-true (CallbackContent? "text"))
    (check-false (CallbackContent? #f))
    (check-true (LiteralContent? 'ready))
    (check-false (LiteralContent? 'other)))

  (define-json-union RecursiveContent string? RecursiveNode)
  (define-json-struct RecursiveNode [children (listof RecursiveContent)])

  (test-case "recursive type references remain lazy"
    (define json (hasheq 'children (list "text" (hasheq 'children '()))))
    (define decoded (jsexpr->RecursiveContent json))
    (check-true (RecursiveContent? decoded))
    (check-true (RecursiveContent-js? json))
    (check-equal? (->jsexpr decoded) json))

  (test-case "union: runtime predicate"
    (check-true (DocContent? "hello"))
    (check-true (DocContent? (Markup #:kind "markdown" #:value "# hi")))
    (check-false (DocContent? 42)))

  (test-case "union: json predicate"
    (check-true (DocContent-js? "hello"))
    (check-true (DocContent-js? (hasheq 'kind "markdown" 'value "# hi")))
    (check-false (DocContent-js? 42)))

  (test-case "union: decode and match expander"
    (check-equal? (jsexpr->DocContent "hello") "hello")
    (define decoded (jsexpr->DocContent (hasheq 'kind "markdown" 'value "# hi")))
    (check-true (Markup? decoded))
    (check-equal? (Markup-kind decoded) "markdown")
    (check-equal? (match "hi" [(DocContent-js v) v]) "hi")
    (check-true (Markup? (match (hasheq 'kind "md" 'value "x")
                           [(DocContent-js v) v]))))

  (test-case "union: decode failure reports unmatched value and expected union"
    (check-exn
      (lambda (e)
        (and (exn:fail? e)
             (regexp-match? #rx"no union alternative matched value: 42" (exn-message e))
             (regexp-match? #rx"expected: '\\(union string\\? Markup\\)" (exn-message e))))
      (lambda ()
        (jsexpr->DocContent 42))))

  (test-case "union: match expanders as-Name and ^Name"
    (check-equal? (match "ok" [(as-DocContent v) v] [_ #f]) "ok")
    (check-equal? (match "ok" [(^DocContent v) v] [_ #f]) "ok")
    (check-true (Markup? (match (hasheq 'kind "md" 'value "x")
                           [(as-DocContent v) v]
                           [_ #f])))
    (check-true (Markup? (match (hasheq 'kind "md" 'value "x")
                           [(^DocContent v) v]
                           [_ #f])))
    (check-false (match 42 [(as-DocContent _) #t] [_ #f]))
    (check-false (match 42 [(^DocContent _) #t] [_ #f])))

  (test-case "union: decode pattern with as-Name"
    (check-equal?
      (match (hasheq 'kind "md" 'value "x")
        [(as-DocContent (Markup #:kind k #:value v)) (list k v)]
        [_ #f])
      '("md" "x"))
    (check-false
      (match "plain"
        [(as-DocContent (Markup #:kind _ #:value _)) #t]
        [_ #f])))

  (test-case "union: encode and decode struct with union field"
    (define string-box (DocBox "hello"))
    (check-equal? (->jsexpr string-box) (hasheq 'payload "hello"))
    (define markup-box (DocBox (Markup #:kind "md" #:value "x")))
    (define markup-json (hasheq 'payload (hasheq 'kind "md" 'value "x")))
    (check-equal? (->jsexpr markup-box) markup-json)
    (define decoded-string (jsexpr->DocBox (hasheq 'payload "hello")))
    (check-equal? (DocBox-payload decoded-string) "hello")
    (define decoded-markup (jsexpr->DocBox markup-json))
    (check-true (Markup? (DocBox-payload decoded-markup)))
    (check-equal? (Markup-kind (DocBox-payload decoded-markup)) "md")))

