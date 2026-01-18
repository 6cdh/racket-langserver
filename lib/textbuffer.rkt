#lang typed/racket/base

(provide TextBuffer
         make-textbuffer
         textbuffer-replace
         textbuffer->string
         iposition->char-position
         char-position->iposition)

(require racket/treelist
         racket/match
         lsptool/common/macro
         "type.rkt")

(define-type CharTL (TreeListof Char))
(define-type IndexTL (TreeListof Index))

;; buffer stores all characters.
;; newline-pos stores all positions of newline character ('\n').
;; A char position of a text buffer is a index, starts from 0.
;; A iposition of a text buffer is a pair of line number and char position.
;; Line number and char position both start from 0.
(struct TextBuffer
  ([buffer : CharTL]
   [newline-pos : IndexTL]))

(: treelist-update (All (a) (-> (TreeListof a) Index (-> a a) (TreeListof a))))
(define (treelist-update tl index updater)
  (treelist-set tl index (updater (treelist-ref tl index))))

(: make-textbuffer (-> String TextBuffer))
(define (make-textbuffer str)
  (textbuffer-add (TextBuffer (treelist) (treelist)) (IPosition 0 0) str))

(: iposition->char-position (-> TextBuffer IPosition Index))
(define (iposition->char-position tb position)
  (match-define (IPosition ln ch) position)
  (position->char-index tb ln ch))

(: position->char-index (-> TextBuffer Index Index Index))
(define (position->char-index tb ln ch)
  (if (zero? ln)
      ch
      (cast (+ (treelist-ref (TextBuffer-newline-pos tb) (sub1 ln)) 1 ch)
            Index)))

(: char-position->iposition (-> TextBuffer Index IPosition))
(define (char-position->iposition tb index)
  (match-define
    (cons line character)
    (or (for/last : (U #f (Pair Index Index))
          ([(p i) (in-indexed (treelist->list (TextBuffer-newline-pos tb)))]
           #:when (< p index))
          (cons (cast (add1 i) Index) (cast (- index p 1) Index)))
        (cons 0 index)))
  (IPosition line character))

(: treelist-increase-from (-> IndexTL Index Integer IndexTL))
(define (treelist-increase-from tl from delta)
  (: inc_delta (-> Index Index))
  (define (inc_delta old)
    (cast (+ old delta) Index))

  (for/fold ([tl tl])
            ([i (in-range from (treelist-length tl))])
    (treelist-update tl (cast i Index) inc_delta)))

(: textbuffer-add (-> TextBuffer IPosition String TextBuffer))
(define (textbuffer-add tb position str)
  (match-define (TextBuffer buffer newlines) tb)
  (match-define (IPosition ln ch) position)
  (define char-index (iposition->char-position tb position))
  (define added-newlines-pos
    (for/list : (Listof Index) ([(c i) (in-indexed (in-string str))]
                                #:when (char=? c #\newline))
      (cast (+ i char-index) Index)))

  (TextBuffer
    (treelist-append (treelist-take buffer char-index)
                     (list->treelist (string->list str))
                     (treelist-drop buffer char-index))
    (for/fold ([newlines (treelist-increase-from newlines ln (string-length str))])
              ([(pos i) (in-indexed (in-list added-newlines-pos))])
      (treelist-insert newlines (cast (+ ln i) Index) pos))))

(: textbuffer-delete (-> TextBuffer IRange TextBuffer))
(define (textbuffer-delete tb range)
  (match-define (TextBuffer buffer newlines) tb)
  (match-define
    (IRange (IPosition st-ln st-ch)
            (IPosition ed-ln ed-ch))
    range)

  (define start-char-index (position->char-index tb st-ln st-ch))
  (define end-char-index (position->char-index tb ed-ln ed-ch))
  (define delete-len (- end-char-index start-char-index))

  (TextBuffer
    (treelist-append (treelist-take buffer start-char-index)
                     (treelist-drop buffer end-char-index))
    (let loop ([newlines newlines]
               [pos : Index (cast (max 0 (sub1 st-ln)) Index)])
      (cond [(= pos (treelist-length newlines))
             newlines]
            [(>= (treelist-ref newlines pos) end-char-index)
             (treelist-increase-from newlines pos (- delete-len))]
            [(< (treelist-ref newlines pos) start-char-index)
             (loop newlines (cast (add1 pos) Index))]
            [(loop (treelist-delete newlines pos) pos)]))))

(: textbuffer-replace (-> TextBuffer IRange String TextBuffer))
(define (textbuffer-replace tb range str)
  (~> tb
      (textbuffer-delete tb range)
      (textbuffer-add tb (IRange-start range) str)))

(: textbuffer->string (-> TextBuffer String))
(define (textbuffer->string tb)
  (list->string (treelist->list (TextBuffer-buffer tb))))
