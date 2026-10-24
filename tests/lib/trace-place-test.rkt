#lang racket/base

(require rackunit
         racket/class
         racket/dict
         racket/file
         racket/list
         racket/match
         racket/place
         racket/port
         racket/runtime-path
         racket/set
         racket/string
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/source.rkt"
         "../../common/path-util.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/internal-types.rkt"
         "../../lsp/analysis.rkt"
         "../../workspace/source-store.rkt")

(define-runtime-path worker-path "trace-place-worker.rkt")
(define (without-dependency-resolution dependency thunk)
  (define resolver (current-module-name-resolver))
  (define (dependency-path? value)
    (or (equal? value dependency)
        (equal? value (path->string dependency))
        (equal? value "dep.rkt")
        (and (pair? value) (ormap dependency-path? value))))
  ;; Completion's module->exports resolves required module paths. Restoration
  ;; may resolve its language's lexer, but must not inspect this dependency.
  (parameterize ([current-module-name-resolver
                  (lambda args
                    (when (ormap dependency-path? args)
                      (error 'restore-analysis "dependency resolved again"))
                    (apply resolver args))])
    (thunk)))

(define (snapshot uri text)
  (Source-Snapshot (uri->source uri) (gensym 'revision) 'buffer (string->immutable-string text)))

(define (place-result submitted)
  (define-values (worker input output error-output)
    (dynamic-place* worker-path 'run #:out #f #:err #f))
  (close-output-port input)
  (dynamic-wind
    void
    (lambda ()
      (place-channel-put worker
                         (list (Source-Snapshot-uri submitted) (Source-Snapshot-text submitted)
                               (current-directory)))
      (define ready
        (sync/timeout 60 worker (handle-evt (place-dead-evt worker) (lambda (_) 'dead))))
      ;; A one-shot worker can exit with its result already queued.
      (define reply (if (eq? ready 'dead) (sync/timeout 0 worker) ready))
      (unless (list? reply)
        (place-kill worker)
        (error 'place-result "worker failed or timed out: ~s; output: ~s"
               reply (port->bytes output)))
      (check-equal? (place-wait worker) 0)
      ;; Read through EOF so output forwarding cannot race this assertion.
      (check-equal? (port->bytes output) #"")
      (check-equal? (port->bytes error-output) #"")
      reply)
    (lambda ()
      (place-kill worker)
      (close-input-port output)
      (close-input-port error-output))))

(define (restore submitted)
  (match-define (list (? boolean? succeeded?) data) (place-result submitted))
  (restore-analysis submitted succeeded? data))

(define (sorted-diags product)
  (sort (Analysis-Product-diagnostics product) string<? #:key (lambda (d) (hash-ref d 'message))))

;; Observe every stored feature and declaration relation through service reads.
;; Scanning positions also catches a restored map with the right values but
;; wrong boundaries. No second serialization is required after an edit.
(define (trace-observations trace length)
  (define decl (send trace get-declaration))
  (define hover (send trace get-hover))
  (define types (send trace get-typed-racket))
  (list
    (sort (send trace get-completions) symbol<?)
    (dict->list (send trace get-docs))
    (dict->list (send trace get-requires))
    (for/list ([(range action) (in-dict (send trace get-quickfixs))])
      (cons range (->jsexpr action)))
    (send trace get-semantic-tokens)
    (send (send trace get-inlay-hints) hints-in-range 0 length)
    (send decl module-binding-definitions)
    (send decl module-binding-uses)
    (for/list ([pos (in-range length)])
      (list (send decl occurrence-at pos) (send decl definition-at pos)
            (send decl uses-at pos) (send decl module-binding-at pos)
            (call-with-values (lambda () (send hover annotation-at pos)) list)
            (call-with-values (lambda () (send hover source-detail-at pos)) list)
            (call-with-values (lambda () (send types inferred-type-at pos)) list)))))

(define (compare-products submitted direct restored)
  (check-equal? (Analysis-Product-succeeded? restored) (Analysis-Product-succeeded? direct))
  (check-equal? (Analysis-Product-summary restored) (Analysis-Product-summary direct))
  (check-equal? (sorted-diags restored) (sorted-diags direct))
  (check-equal? (Analysis-Product-contribution restored) (Analysis-Product-contribution direct))
  (define original (Analysis-Product-trace direct))
  (define incoming (Analysis-Product-trace restored))
  (define length (string-length (Source-Snapshot-text submitted)))
  (check-equal? (trace-observations incoming length) (trace-observations original length))
  ;; Apply the same shift, replacement notification, insert, and prefix deletion
  ;; to every fixture. Span-specific hint invalidation is checked separately.
  (for ([trace (in-list (list original incoming))])
    (send trace text-replaced 0 0)
    (send trace expand 0 3)
    (send trace text-replaced 25 26)
    (send trace expand 26 28)
    (send trace contract 0 2))
  (check-equal? (trace-observations incoming (+ length 3))
                (trace-observations original (+ length 3))))

(module+ test
  (check-true (place-enabled?))
  (for ([fixture (in-list
                   (list
                     (list 'racket
                           (string-append
                             "#lang racket/base\n"
                             "(require racket/list)\n"
                             ";; A comment 🙂.\n"
                             "(define (f λ) (let ([unused 1]) (+ λ 2)))\n"
                             "(f 3)\n"
                             "(struct point (x y))\n"
                             "(point 1 2)\n"))
                     (list 'typed-racket
                           (string-append
                             "#lang typed/racket\n"
                             "(define λ-name 42)\n"
                             "(define (f [x : Integer]) (+ x λ-name))\n"
                             "(f 1)\n"))
                     (list 'type-error
                           (string-append
                             "#lang typed/racket\n"
                             "(: n Integer)\n"
                             "(define n \"bad\")\n"))
                     (list 'read-error
                           (string-append
                             "#lang racket/base\n"
                             "(define (broken\n"))
                     (list 'expand-error
                           (string-append
                             "#lang racket/base\n"
                             "(unbound-name 1)\n"))
                     (list 'info
                           (string-append
                             "#lang info\n"
                             "(define collection \"test\")\n"))))])
    (match-define (list name text) fixture)
    (test-case (format "real place trace fidelity ~a" name)
      (define submitted (snapshot "file:///tmp/trace%2dplace.rkt" text))
      (define direct (analyze-source submitted))
      (define restored (restore submitted))
      (compare-products submitted direct restored)
      (case name
        [(racket)
         (check-true (Analysis-Product-succeeded? restored))
         (check-not-false (memq 'f (send (Analysis-Product-trace restored) get-completions)))
         (check-true (pair? (dict->list (send (Analysis-Product-trace restored) get-quickfixs))))]
        [(typed-racket)
         (check-true (pair? (send (send (Analysis-Product-trace restored) get-inlay-hints)
                                  hints-in-range 0 (string-length text))))]
        [(type-error read-error expand-error)
         (check-false (Analysis-Product-succeeded? restored))
         (check-true (pair? (Analysis-Product-diagnostics restored)))])))

  (test-case "virtual source keeps local declarations and original diagnostic URI"
    (define uri "untitled:Trace%20Cell?rev=one#cell")
    (define submitted (snapshot uri "#lang racket/base\n(define (f unused) 1)\n(f 2)\n"))
    (define direct (analyze-source submitted))
    (define restored (restore submitted))
    (compare-products submitted direct restored)
    (check-false (Analysis-Product-contribution restored))
    (check-true (for/or ([diag (in-list (Analysis-Product-diagnostics restored))])
                  (equal? (hash-ref diag 'source) uri))))

  (test-case "relative dependencies, original URI, and restoration without collection"
    (define root (make-temporary-file "trace-snapshot~a" 'directory))
    (define (write-fixture name text)
      (call-with-output-file (build-path root name) (lambda (out) (display text out))))
    (dynamic-wind
      void
      (lambda ()
        ;; A reader with a visible read counter and output during startup and
        ;; reading. Counters are absolute so directory mistakes cannot hide calls.
        (define read-counter (build-path root "reads"))
        (define expand-counter (build-path root "expansions"))
        (write-fixture
          "reader.rkt"
          (format
            (string-append
              "#lang s-exp syntax/module-reader\nracket/base\n"
              "#:wrapper1 (lambda (thunk)\n"
              "  (call-with-output-file ~s (lambda (out) (displayln 'read out)) #:exists 'append)\n"
              "  (displayln 'reader-output) (thunk))\n(displayln 'reader-startup)\n")
            (path->string read-counter)))
        (write-fixture "dep.rkt" "#lang racket/base\n(provide dep)\n(define dep 8)\n")
        (define text
          (format
            (string-append
              "#lang reader \"reader.rkt\"\n(require \"dep.rkt\" (for-syntax racket/base))\n"
              "(begin-for-syntax\n"
              " (call-with-output-file ~s (lambda (out) (displayln 'expand out)) #:exists 'append)\n"
              " (displayln 'expansion-output))\n"
              ";; Documented function.\n(define (f unused) dep)\n(f 1)\n")
            (path->string expand-counter)))
        (define uri
          (string-replace (path->uri (build-path root "trace-source.rkt"))
                          "trace-source" "trace%2dsource"))
        (define submitted (snapshot uri text))
        (match-define (list succeeded? data) (place-result submitted))
        (check-true succeeded?)
        (define reads (file->string read-counter))
        (define expansions (file->string expand-counter))
        (check-true (non-empty-string? reads))
        (check-true (non-empty-string? expansions))
        (define restored
          (without-dependency-resolution
            (build-path root "dep.rkt")
            (lambda () (restore-analysis submitted succeeded? data))))
        (check-equal? (file->string read-counter) reads)
        (check-equal? (file->string expand-counter) expansions)
        (define trace (Analysis-Product-trace restored))
        (define (position-of pattern)
          (caar (regexp-match-positions pattern text)))
        (define definition (position-of #rx"f unused"))
        (define use (add1 (position-of #rx"\\(f 1")))
        (define declarations (send trace get-declaration))
        (check-equal? (send declarations definition-at use) (CharRange definition (add1 definition)))
        (check-equal? (send declarations uses-at definition) (list (CharRange use (add1 use))))
        (check-equal? (Module-Binding-filepath (send declarations module-binding-at use))
                      (source-file-path (Source-Snapshot-source submitted)))
        (check-not-false (member (build-path root "dep.rkt")
                                 (dict-values (send trace get-requires))))
        (check-true (for/or ([(range action) (in-dict (send trace get-quickfixs))])
                      (hash-has-key? (WorkspaceEdit-changes (CodeAction-edit action))
                                     (string->symbol uri))))
        (define direct
          (parameterize ([current-output-port (open-output-nowhere)])
            (analyze-source submitted)))
        (compare-products submitted direct restored))
      (lambda () (delete-directory/files root))))

  (test-case "restored document features and hint invalidation"
    (define text
      "#lang racket/base\n;; A point.\n(struct point (x y))\n(define (f unused) (+ 1 2))\n(f 3)\n(point 4 5)\n")
    (define submitted (snapshot "untitled:Features.rkt" text))
    (define product (restore submitted))
    (define doc (make-source-doc (Source-Snapshot-source submitted) text))
    (doc-update-trace! doc (Analysis-Product-trace product) #f (Doc-version doc))
    (define (pos pattern [offset 0])
      (doc-abs-pos->pos doc (+ offset (caar (regexp-match-positions pattern (doc-get-text doc))))))
    (define hints (doc-inlay-hints doc (Range (Pos 0 0) (Pos 6 0))))
    (check-equal? (map InlayHint-label hints) '("x " "y "))
    (check-not-false (doc-hover doc (pos #rx"\\(point 4" 1)))
    (define hover-before (Hover-contents (doc-hover doc (pos #rx"\\(point 4" 1))))
    (check-true (string-contains? hover-before "A point."))
    (check-true (pair? (doc-code-action doc (Range (Pos 3 0) (Pos 3 35)))))
    (define call-start (pos #rx"\\(point 4"))
    (doc-apply-edit! doc (Range call-start call-start) "  ")
    (check-equal? (Hover-contents (doc-hover doc (pos #rx"\\(point 4" 1))) hover-before)
    (check-equal? (map InlayHint-label (doc-inlay-hints doc (Range (Pos 0 0) (Pos 6 0))))
                  '("x " "y "))
    ;; Changing the declaration invalidates constructor labels at another span.
    (define field (pos #rx"x y"))
    (doc-apply-edit! doc (Range field (Pos (Pos-line field) (add1 (Pos-char field)))) "z")
    (check-equal? (doc-inlay-hints doc (Range (Pos 0 0) (Pos 6 0))) '()))

  (test-case "documentation tags support signature help after restoration"
    (define text "#lang racket/base\n(map add1 '(1 2))\n")
    (define submitted (snapshot "untitled:Signature.rkt" text))
    (define direct (analyze-source submitted))
    (define restored (restore submitted))
    (define (signature product)
      (define doc (make-source-doc (Source-Snapshot-source submitted) text))
      (doc-update-trace! doc (Analysis-Product-trace product) #f 0)
      (doc-signature-help doc (Pos 1 5)))
    (check-true (pair? (dict->list (send (Analysis-Product-trace restored) get-docs))))
    (check-not-false (signature restored))
    (check-equal? (signature restored) (signature direct))))
