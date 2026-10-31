#lang racket
(require "../common/json-util.rkt"
         rackunit syntax/macro-testing)

(module+ test
  (define (capture-fail-exn thunk)
    (with-handlers ([exn:fail? values])
      (thunk)
      (fail "expected an exception")))

  ;; Define test structs
  (define-json-struct Pos
    [line exact-integer?]
    [char exact-integer? #:json character])

  (define pos-1 (Pos 1 10))
  (define pos-2 (Pos #:char 20 #:line 2))

  (define-json-struct Range
    [start Pos]
    [end Pos])

  (define-json-struct MaybePosHolder
    [pos (optional Pos)])

  (define-json-struct PosListHolder
    [items (listof Pos)])

  (define-json-struct PosHashHolder
    [items (hashof symbol? Pos)])

  (define-json-struct OrPosHolder
    [item (union string? Pos)])

  (define-json-struct MaybeOrPosHolder
    [item (optional (union string? Pos))])

  (define-json-struct OrPosListHolder
    [items (listof (union string? Pos))])

  (define-json-struct CustomContractHolder
    [item (contract integer?)])

  (define-json-struct FastCoordinate
    [value exact-nonnegative-integer?])

  (define-json-struct SlowCoordinate
    [value (contract exact-nonnegative-integer?)])

  (define-json-struct GeoPos
    [line exact-nonnegative-integer?]
    [char exact-nonnegative-integer? #:json character])

  (define-json-struct GeoRange
    [start GeoPos]
    [end GeoPos])

  (define-json-struct GeoLocation
    [uri string?]
    [range GeoRange])

  (define-json-struct FastExtensible
    [value (contract (lambda (value) (or (string? value) (exact-integer? value))))]
    #:rest extras)

  (define-json-struct TreeNode
    [value string?]
    [children (listof TreeNode)])

  (define-json-struct Extensible
    [name string?]
    [enabled (optional boolean?)]
    #:rest extras)

  (define p-hash (hasheq 'line 10 'character 5))
  (define p-hash-2 (hasheq 'line 3 'character 30))
  (define p-hash-invalid (hasheq 'line "not-an-int" 'character 5))
  (define range-val (Range pos-1 pos-2))
  (define r-hash (hasheq 'start p-hash 'end (hasheq 'line 20 'character 15)))

  (test-case "construction: positional and keyword"
    (check-equal? (Pos-line pos-1) 1)
    (check-equal? (Pos-char pos-1) 10)
    (check-equal? (Pos-line pos-2) 2)
    (check-equal? (Pos-char pos-2) 20))

  (test-case "construction: identifier reference works as procedure value"
    (check-true (procedure? Pos))
    (check-equal? (map Pos '(1 2) '(10 20)) (list (Pos 1 10) (Pos 2 20))))

  (test-case "construction: contract enforcement"
    (check-exn exn:fail:contract? (λ () (Pos "1" 10)))
    (check-exn exn:fail:contract? (λ () (Pos #:line 1 #:char "10"))))

  (test-case "fast guard preserves contract errors"
    (define (normalize-message error name)
      (regexp-replace #rx"\n  at: [^\n]*"
                      (string-replace (exn-message error) name "Coordinate")
                      ""))
    (for ([invalid (in-list (list -1 1.0 "1" #f))])
      (define fast-error
        (capture-fail-exn (lambda () (FastCoordinate invalid))))
      (define slow-error
        (capture-fail-exn (lambda () (SlowCoordinate invalid))))
      (check-true (exn:fail:contract? fast-error))
      (check-equal? (normalize-message fast-error "FastCoordinate")
                    (normalize-message slow-error "SlowCoordinate"))))

  (test-case "explicit contract fallback supports predicates and rest fields"
    (define json (hasheq 'value "ok" 'extra 3))
    (check-equal? (->jsexpr (jsexpr->FastExtensible json)) json)
    (check-equal? (FastExtensible-value (FastExtensible 0 (hasheq))) 0)
    (check-exn exn:fail:contract? (lambda () (FastExtensible #f (hasheq))))
    (check-exn exn:fail:contract? (lambda () (FastExtensible "ok" #f))))

  (test-case "constructor and decoder retain higher-order field protection"
    (define-json-struct CallbackHolder
      [callback (contract (-> integer? integer?))])
    (for ([holder (in-list
                    (list (CallbackHolder add1)
                          (CallbackHolder #:callback add1)
                          (jsexpr->CallbackHolder (hasheq 'callback add1))))])
      (define callback (CallbackHolder-callback holder))
      (check-equal? (callback 1) 2)
      (check-exn exn:fail:contract? (lambda () (callback "bad"))))
    (define bad-result (CallbackHolder (lambda (_) "bad")))
    (check-exn exn:fail:contract? (lambda () ((CallbackHolder-callback bad-result) 1)))
    (check-exn exn:fail:contract? (lambda () (CallbackHolder #f))))

  (test-case "optional and union contracts retain higher-order protection"
    (define-json-struct OptionalCallback
      [callback (optional (contract (-> integer? integer?)))])
    (define-json-struct UnionCallback
      [callback (union string? (contract (-> integer? integer?)))])
    (check-true (Nothing? (OptionalCallback-callback (jsexpr->OptionalCallback (hasheq)))))
    (check-equal? (UnionCallback-callback (UnionCallback "ok")) "ok")
    (check-exn exn:fail:contract?
               (lambda () ((OptionalCallback-callback (OptionalCallback add1)) "bad")))
    (check-exn exn:fail:contract?
               (lambda () ((UnionCallback-callback (UnionCallback add1)) "bad"))))

  (test-case "hash fields retain protection of later mutations"
    (define-json-struct StringTable [items (hashof symbol? string?)])
    (define table (StringTable (make-hasheq '((a . "ok")))))
    (check-exn exn:fail:contract?
               (lambda () (hash-set! (StringTable-items table) 'b 42)))
    (check-exn exn:fail:contract?
               (lambda () (StringTable (hasheq 'a 42)))))

  (test-case "named unions with arbitrary predicates keep a single contract check"
    (define calls 0)
    (define-json-union Stateful
      (contract (lambda (_)
                  (set! calls (add1 calls))
                  (even? calls))))
    (define-json-struct StatefulHolder [value Stateful])
    (check-exn exn:fail:contract? (lambda () (StatefulHolder 'value)))
    (check-equal? calls 1))

  (test-case "shadowed built-in names keep their actual contracts"
    (let ([string? (-> integer? integer?)])
      (define-json-struct Shadowed [value string?])
      (define callback (Shadowed-value (Shadowed add1)))
      (check-true (Shadowed-js? (hasheq 'value add1)))
      (check-false (Shadowed-js? (hasheq 'value #f)))
      (check-equal? (callback 1) 2)
      (check-exn exn:fail:contract? (lambda () (callback "bad")))))

  (test-case "keyword arguments evaluate once in caller order"
    (define observed '())
    (define (observe label value)
      (set! observed (append observed (list label)))
      value)
    (define value (Pos #:char (observe 'char 20) #:line (observe 'line 2)))
    (check-equal? observed '(char line))
    (check-equal? value (Pos 2 20))
    (set! observed '())
    (check-exn exn:fail?
               (lambda ()
                 (Pos #:char (begin (observe 'char 20) (error 'stop))
                      #:line (observe 'line 2))))
    (check-equal? observed '(char)))

  (test-case "construction: syntax errors for invalid keyword usage"
    (check-exn
      (λ (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"missing keyword argument #:char"
                            (exn-message e))))
      (λ ()
        (convert-compile-time-error (Pos #:line 1))))
    (check-exn
      (λ (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"unknown keyword argument"
                            (exn-message e))))
      (λ ()
        (convert-compile-time-error (Pos #:line 1 #:char 2 #:other 3))))
    (check-exn
      (λ (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"mixed positional and keyword arguments are not allowed"
                            (exn-message e))))
      (λ ()
        (convert-compile-time-error
          (Pos 1 #:char 2))))
    (check-exn
      (λ (e)
        (and (exn:fail:syntax? e)
             (regexp-match? #rx"duplicate keyword argument #:line"
                            (exn-message e))))
      (λ ()
        (convert-compile-time-error
          (Pos #:line 1 #:line 2 #:char 3)))))

  (test-case "decoders evaluate fields in declaration order"
    (define observed '())
    (define (OrderValue? value) (string? value))
    (define OrderValue-js? string?)
    (define (jsexpr->OrderValue value)
      (set! observed (append observed (list value)))
      value)
    (define-json-struct Ordered
      [first OrderValue]
      [second OrderValue])
    (define value (jsexpr->Ordered (hasheq 'second "b" 'first "a")))
    (check-equal? observed '("a" "b"))
    (check-equal? (Ordered-first value) "a"))

  (test-case "fast guards preserve constructors and JSON decoding for non-negative integers and nested structs"
    (define huge (expt 2 80))
    (define start (GeoPos 0 huge))
    (define end (GeoPos #:char 0 #:line huge))
    (define range (GeoRange start end))
    (define location (GeoLocation #:range range #:uri "file:///test.rkt"))
    (check-eq? (GeoRange-start range) start)
    (check-eq? (GeoLocation-range location) range)
    (check-equal? (jsexpr->GeoLocation (->jsexpr location)) location)
    (check-match range (GeoRange (GeoPos 0 _) (GeoPos _ 0)))
    (for ([invalid (in-list (list -1 0.0 "0" #f))])
      (check-exn exn:fail:contract? (lambda () (GeoPos invalid 0)))
      (check-exn exn:fail:contract? (lambda () (GeoPos #:line 0 #:char invalid)))
      (check-exn exn:fail:contract?
                 (lambda ()
                   (jsexpr->GeoPos (hasheq 'line 0 'character invalid)))))
    (check-exn exn:fail:contract? (lambda () (GeoRange #f end)))
    (check-exn exn:fail:contract? (lambda () (GeoRange #:start start #:end #f)))
    (check-exn exn:fail:contract? (lambda () (GeoLocation #f range)))
    (check-exn exn:fail:contract? (lambda () (GeoLocation "file:///test.rkt" #f)))
    (check-exn exn:fail:contract?
               (lambda ()
                 (jsexpr->GeoLocation
                   (hasheq 'uri "file:///test.rkt"
                           'range (hasheq 'start (hasheq 'line -1 'character 0)
                                          'end (hasheq 'line 0 'character 0)))))))

  (test-case "type specs: arbitrary contracts require contract form"
    (check-exn
      exn:fail:syntax?
      (lambda ()
        (convert-compile-time-error
          (define-json-struct BadContract
            [x (or/c string? exact-integer?)])))))

  (test-case "matching: Pos matches structs only"
    (check-equal?
      (match pos-1 [(Pos #:line l) l] [_ #f])
      1)
    (check-equal?
      (match pos-2 [(Pos l c) (list l c)] [_ #f])
      '(2 20))
    (check-equal?
      (match p-hash [(Pos #:line l) l] [_ #f])
      #f))

  (test-case "matching: Pos-js matches hashes"
    (check-equal?
      (match p-hash [(Pos-js #:line l) l] [_ #f])
      10)
    (check-equal?
      (match p-hash-2 [(Pos-js l c) (list l c)] [_ #f])
      '(3 30))
    (check-equal?
      (match pos-1 [(Pos-js l c) (list l c)] [_ #f])
      #f))

  (test-case "validation: Pos-js? validates JSON hash only"
    (check-true (Pos-js? p-hash))
    (check-false (Pos-js? pos-1))
    (check-false (Pos-js? p-hash-invalid)))

  (test-case "matching: as-Pos decodes valid JSON hash"
    (check-equal?
      (match p-hash
        [(as-Pos pos) (Pos-line pos)]
        [_ #f])
      10)
    (check-equal?
      (match p-hash-invalid
        [(as-Pos _pos) #t]
        [_ #f])
      #f)
    (check-equal?
      (match pos-1
        [(as-Pos _pos) #t]
        [_ #f])
      #f))

  (test-case "matching: jsexpr styles for Pos"
    ;; optional schema guard for raw JSON
    (check-equal? (match p-hash [(? Pos-js? (Pos-js #:line l)) l] [_ #f])
                  10)
    ;; decode-then-match with as-Name
    (check-equal? (match p-hash [(as-Pos (Pos #:line l #:char c)) (list l c)] [_ #f])
                  '(10 5))
    ;; generated alias: ^Name => (as-Name (Name ...))
    (check-equal? (match p-hash [(^Pos #:line l #:char c) (list l c)] [_ #f])
                  '(10 5))
    (check-equal? (match p-hash [(^Pos l c) (list l c)] [_ #f])
                  '(10 5)))

  (test-case "encoding: simple and recursive"
    (check-equal? (->jsexpr pos-1)
                  (hasheq 'line 1 'character 10))
    (check-equal? (->jsexpr range-val)
                  (hasheq 'start (hasheq 'line 1 'character 10)
                          'end (hasheq 'line 2 'character 20))))

  (test-case "rest fields decode and encode as direct object properties"
    (define json
      (hasheq 'name "tool"
              'enabled #t
              'count 3
              'metadata (hasheq 'stable #t)))
    (define decoded (jsexpr->Extensible json))
    (check-equal? (Extensible-extras decoded)
                  (hasheq 'count 3 'metadata (hasheq 'stable #t)))
    (check-equal? (->jsexpr decoded) json)
    (check-equal?
      (->jsexpr
        (Extensible #:name "declared"
                    #:enabled (Nothing)
                    #:extras (hasheq 'name "extra" 'count 1)))
      (hasheq 'name "declared" 'count 1)))

  (test-case "rest fields participate in decoded struct matching"
    (check-equal?
      (match (hasheq 'name "tool" 'count 3)
        [(^Extensible #:name name #:extras extras)
         (list name extras)]
        [_ #f])
      (list "tool" (hasheq 'count 3))))

  (test-case "decoding: recursive jsexpr->Range"
    (define decoded (jsexpr->Range r-hash))
    (check-true (Range? decoded))
    (check-true (Pos? (Range-start decoded)))
    (check-equal? (Pos-line (Range-start decoded)) 10)
    (check-equal? (Pos-char (Range-end decoded)) 15))

  (test-case "validation: Range-js? validates nested json shape"
    (check-true (Range-js? r-hash))
    (check-false (Range-js? (hasheq 'start pos-1 'end pos-2)))
    (check-false (Range-js? (hasheq 'start p-hash))))

  (test-case "decoding: optional field can be absent"
    (define holder (jsexpr->MaybePosHolder (hasheq)))
    (check-true (Nothing? (MaybePosHolder-pos holder)))
    (define holder2 (jsexpr->MaybePosHolder (hasheq 'pos p-hash)))
    (check-true (Pos? (MaybePosHolder-pos holder2))))

  (test-case "decoding: listof decodes compound entries"
    (define holder (jsexpr->PosListHolder (hasheq 'items (list p-hash p-hash-2))))
    (check-true (Pos? (first (PosListHolder-items holder))))
    (check-equal? (Pos-line (second (PosListHolder-items holder))) 3))

  (test-case "decoding: hashof decodes compound values"
    (define holder (jsexpr->PosHashHolder (hasheq 'items (hasheq 'a p-hash))))
    (define decoded-pos (hash-ref (PosHashHolder-items holder) 'a))
    (check-true (Pos? decoded-pos))
    (check-equal? (Pos-char decoded-pos) 5))

  (test-case "decoding: union chooses string variant"
    (define from-str (jsexpr->OrPosHolder (hasheq 'item "hello")))
    (check-equal? (OrPosHolder-item from-str) "hello"))

  (test-case "decoding: union chooses decoded struct variant"
    (define from-pos (jsexpr->OrPosHolder (hasheq 'item p-hash)))
    (check-equal? (Pos-line (OrPosHolder-item from-pos)) 10))

  (test-case "decoding: union failure reports type, field, unmatched value, and expected union"
    (check-exn
      (lambda (e)
        (and (exn:fail:contract? e)
             (regexp-match? #rx"jsexpr->OrPosHolder: failed to decode field" (exn-message e))
             (regexp-match? #rx"type: 'OrPosHolder" (exn-message e))
             (regexp-match? #rx"field: 'item" (exn-message e))
             (regexp-match? #rx"no union alternative matched value: 42" (exn-message e))
             (regexp-match? #rx"expected: '\\(union string\\? Pos\\)" (exn-message e))))
      (lambda ()
        (jsexpr->OrPosHolder (hasheq 'item 42)))))

  (test-case "validation: union json predicate"
    (check-true (OrPosHolder-js? (hasheq 'item "s")))
    (check-true (OrPosHolder-js? (hasheq 'item p-hash)))
    (check-false (OrPosHolder-js? (hasheq 'item 42))))

  (test-case "decoding: optional with union"
    (define missing (jsexpr->MaybeOrPosHolder (hasheq)))
    (check-true (Nothing? (MaybeOrPosHolder-item missing)))
    (define from-str (jsexpr->MaybeOrPosHolder (hasheq 'item "ok")))
    (check-equal? (MaybeOrPosHolder-item from-str) "ok")
    (define from-pos (jsexpr->MaybeOrPosHolder (hasheq 'item p-hash-2)))
    (check-true (Pos? (MaybeOrPosHolder-item from-pos)))
    (check-equal? (Pos-char (MaybeOrPosHolder-item from-pos)) 30))

  (test-case "decoding: listof with union"
    (define holder
      (jsexpr->OrPosListHolder (hasheq 'items (list "a" p-hash "b" p-hash-2))))
    (define items (OrPosListHolder-items holder))
    (check-equal? (first items) "a")
    (check-true (Pos? (second items)))
    (check-equal? (third items) "b")
    (check-true (Pos? (fourth items)))
    (check-exn exn:fail?
               (lambda ()
                 (jsexpr->OrPosListHolder (hasheq 'items (list "a" 99))))))

  (test-case "decoding: explicit contract field passes through raw value"
    (define holder (jsexpr->CustomContractHolder (hasheq 'item 1.0)))
    (check-equal? (CustomContractHolder-item holder) 1.0)
    (check-exn exn:fail:contract?
               (lambda ()
                 (jsexpr->CustomContractHolder (hasheq 'item "bad")))))

  (test-case "decoding: recursive type decodes nested structs"
    (define tree-json
      (hasheq 'value "root"
              'children (list (hasheq 'value "leaf"
                                      'children '()))))
    (define tree (jsexpr->TreeNode tree-json))
    (check-true (TreeNode? tree))
    (check-true (TreeNode? (first (TreeNode-children tree))))
    (check-equal? (TreeNode-value (first (TreeNode-children tree))) "leaf"))

  (test-case "decoding: field errors include type field expected value and cause"
    (check-exn
      (lambda (e)
        (and (exn:fail:contract? e)
             (regexp-match? #rx"jsexpr->TreeNode: failed to decode field" (exn-message e))
             (regexp-match? #rx"type: 'TreeNode" (exn-message e))
             (regexp-match? #rx"field: 'children" (exn-message e))
             (regexp-match? #rx"expected: '\\(listof TreeNode\\)" (exn-message e))
             (regexp-match? #rx"value: '\\(\"bad\"\\)" (exn-message e))
             (regexp-match? #rx"cause:" (exn-message e))))
      (lambda ()
        (jsexpr->TreeNode (hasheq 'value "root"
                                  'children (list "bad"))))))

  (test-case "matching: nested Range-js hash"
    (match r-hash
      [(Range-js #:start (Pos-js #:line l))
       (check-equal? l 10)]))

  (test-case "matching: compound struct and hash pattern forms"
    (match r-hash
      [(Range-js s e)
       (check-true (hash? s))
       (check-equal? (hash-ref s 'line) 10)])
    (match (Range (Pos 10 5) (Pos 20 15))
      [(Range (Pos l1 c1) (Pos l2 c2))
       (check-equal? l1 10)
       (check-equal? c2 15)])
    (match (Range (Pos 10 5) (Pos 20 15))
      [(Range s e)
       (check-true (Pos? s))
       (check-equal? (Pos-line s) 10)]))

  (test-case "matching: nested decode pattern with as-Range"
    (check-equal?
      (match r-hash
        [(as-Range (Range #:start (Pos #:line sl #:char sc)
                          #:end (Pos #:line el #:char ec)))
         (list sl sc el ec)]
        [_ #f])
      '(10 5 20 15)))
  )

;; Export submodule test
(module export-test racket
  (require "../common/json-util.rkt")
  (define-json-struct Exported [x exact-integer?])
  (provide (json-type-out Exported)))

(module+ test
  (require (submod ".." export-test) rackunit)

  (test-case "shadowed imported predicates retain a single contract check"
    (define calls 0)
    (let ([Exported? (lambda (_)
                       (set! calls (add1 calls))
                       (even? calls))])
      (define-json-struct Holder [value Exported])
      (check-exn exn:fail:contract? (lambda () (Holder 'value)))
      (check-equal? calls 1)))

  (test-case "Verify json-type-out exports"
    ;; Struct constructor and predicate
    (define exported-val (Exported 10))
    (check-true (Exported? exported-val))
    (check-equal? (Exported-x exported-val) 10)

    ;; Keyword wrapper (Exported supports keywords)
    (define exported-kw (Exported #:x 20))
    (check-equal? (Exported-x exported-kw) 20)

    ;; JSON matcher
    (check-match (hasheq 'x 30) (Exported-js 30))

    ;; Contract: JSON-only
    (check-false (Exported-js? exported-val))
    (check-true (Exported-js? (hasheq 'x 40)))

    ;; as-Name should be exported by json-type-out
    (check-equal?
      (match (hasheq 'x 41)
        [(as-Exported ex) (Exported-x ex)]
        [_ #f])
      41)
    ;; ^Name should be exported by json-type-out
    (check-equal?
      (match (hasheq 'x 42)
        [(^Exported x) x]
        [_ #f])
      42)))
