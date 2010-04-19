;; todo: handle <examples>, <pre>?, <table>, <blockquote>
;; todo: handle <nowiki>.  nearly 100% of existing <nowiki> tags are used
;;       solely to allow HTML character entities such as &lt;
;;       currently assumes <nowiki> is block tag, but it can be inline

(module svnwiki-sxml *

(import scheme chicken)
(require-library srfi-13 ports data-structures extras)
(import extras)
(import (only ports with-input-from-port))
(import (only data-structures string-intersperse string-split))
(import (only srfi-13 string-trim-right string-trim-both))
(use regex)
(use matchable)
(use html-parser)

(import irregex)

;; (import (rename irregex (irregex irr)))         ; debug
;; (define (irregex x)
;;   (display "compiling ")
;;   (write x)
;;   (newline)
;;   (time (irr x)))

(define +identifier-tags+
    (list "procedure" "macro" "read" "parameter"
          "record" "string" "class" "method" "constant"))

(define re:header
  (irregex '(: (submatch (>= 2 #\=)) (+ space) (submatch (+ any)))))
;; SVNWIKI interprets every character of *#*# and converts this into
;; ul ol ul ol -- if one char does not match in the next, a new list is
;; started at that nesting depth.  We currently just rely on the last
;; character, and only at list open
(define re:unordered-list
  (irregex '(: (submatch (* (or #\* #\#)) #\*) (+ space) (submatch (+ any)))))
(define re:ordered-list
  (irregex '(: (submatch (* (or #\* #\#)) #\#) (+ space) (submatch (+ any)))))
(define re:definition-list
  (irregex '(: #\; (* space)
               (submatch (+ (~ #\:)))     ; WARNING: submatch 1 may have suffixed whitespace
               (* space) #\: (* space)
               (submatch (* any)))))
(define re:preformatted
  (irregex '(: space (submatch (+ any)))))
(define sre:definition-tag
  `(: #\< (submatch (or ,@+identifier-tags+)) #\>
               (submatch (: any (*? any)))
               "</" (backref 1) #\>
               (* any)))
(define sre:horizontal-rule '(: "---" (? #\-)))  ; svnwiki is ---- only
(define re:definition-tag (irregex sre:definition-tag))
(define re:horizontal-rule (irregex sre:horizontal-rule))

;; This expression is very costly for irregex to compile
;; (define sre:enscript-tag-start               ; crappy
;;   '(: "<enscript"
;;       (? (: space (or "highlight" "language") #\=
;;             (? (or #\" #\'))
;;             (submatch (+ (~ (or #\" #\'))))
;;             (? (or #\" #\'))))
;;       #\>
;;       (submatch (* any))))

(define sre:enscript-tag-start
  '(: #\< "enscript" (submatch (* (~ #\>)))
      #\> (submatch (* any))))

;; (define sre:tag-attribute            ; compilation ok
;;   '(: (* space) (submatch (+ alpha)) #\= #\' (submatch (+ (~ #\'))) #\'))
;; (define sre:tag-attribute            ; compilation very slow
;;   '(: (* space) (submatch (+ alpha)) #\=
;;       (or (: #\' (submatch (+ (~ #\'))) #\')
;;           (: #\" (submatch (+ (~ #\"))) #\")
;;           (: (submatch (+ (~ space)))))
;;       ))
(define sre:tag-attribute                   ; trade compile time for runtime by using greedy
  '(: (* space) (submatch (+ alpha)) #\=    ; alt: use a multi-fold and no (or ...)
      (or (: #\' (submatch (*? any)) #\')
          (: #\" (submatch (*? any)) #\")
          (: (submatch (+ (~ space)))))))
(define re:tag-attribute (irregex sre:tag-attribute))

(define sre:nowiki-tag-start '(: "<nowiki>" (submatch (* any))))
(define re:nowiki-tag-start (irregex sre:nowiki-tag-start))
(define sre:table-tag-start '(: (submatch "<table" (? #\space) (*? any) #\>)
                                (submatch (* any))))
(define sre:examples-tag-start '(: "<examples>" (submatch (* any))))
(define re:examples-tag-start (irregex sre:examples-tag-start))
(define re:table-tag-start (irregex sre:table-tag-start))
(define re:enscript-tag-start (irregex sre:enscript-tag-start))
(define re:enscript-tag-end (irregex '(: (submatch (* any))
                                         "</enscript>"
                                         (submatch (* any)))))
(define re:nowiki-tag-end (irregex '(: (submatch (* any))
                                       "</nowiki>"
                                       (submatch (* any)))))
(define re:table-tag-end (irregex '(: (submatch (* any))
                                      "</table>"
                                      (submatch (* any)))))
(define re:examples-tag-end (irregex '(: (submatch (* any))
                                         "</examples>"
                                         (submatch (* any)))))
(define sre:directive '(: "[["
                          (submatch (or "tags" "toc"))
                          ":"
                          (submatch (* (~ #\])))
                          "]]"))
(define re:directive (irregex sre:directive))

(define re:block      ; Should probably use existing REs.
  (irregex `(or (: (>= 2 #\=) (+ space) (+ any))          ; header
                (: (+ (or #\* #\#)) (+ space) (+ any))    ; item-list
                (: #\; (+ (~ #\:)) #\: (+ any))           ; definition-list
                (: space (+ any))                         ; pre
                ,sre:definition-tag
                ,sre:horizontal-rule
                ,sre:enscript-tag-start
                ,sre:nowiki-tag-start
                ,sre:table-tag-start
                ,sre:examples-tag-start
                ,sre:directive
             )))

;; Single-line readahead (effectively, adds peek-line to read-line)
(define *buffered-line* #f)
(define (read-buffered-line #!optional (p (current-input-port)))
  ;; NOTE: buffer only works for one port and is not SRFI-18 compatible
  (let ((b *buffered-line*))
    (cond (b (set! *buffered-line* #f)
             b)
          (else (read-line p)))))
(define (peek-buffered-line/normal #!optional (p (current-input-port)))
  (cond (*buffered-line*)
        (else
         (let ((line (read-line p)))
           (set! *buffered-line* line)
           line))))
(define (peek-buffered-line/debug #!optional (p (current-input-port)))
  (let ((line   (peek-buffered-line/normal p)))
    (print "line: " line)
    line))
(define peek-buffered-line peek-buffered-line/normal)
(define (poke-line line #!optional (p (current-input-port)))
  (set! *buffered-line* line))
(define (discard-line #!optional (p (current-input-port)))
  (set! *buffered-line* #f))

(define (section-body depth)
  (let ((line (peek-buffered-line)))
    (cond ((eof-object? line)
           '())
          ((string-match re:header line)
           => (lambda (m)
                (let ((next-depth (string-length (cadr m)))
                      (title (caddr m)))
                  (cond ((> next-depth depth)
                         (discard-line)
                         (let ((sec `(section ,next-depth ,title .
                                              ,(section-body next-depth))))
                           (cons sec (section-body depth))))
                        (else
                         '())))))
          ((string=? line "")
           (discard-line)
           (section-body depth))
          (else (cons (block)
                      (section-body depth))))))

(define (block)
  (let ((line (peek-buffered-line)))
    (cond ((or (string-match re:unordered-list line)
               (string-match re:ordered-list line)) ; could do simple match here
           (item-list 0))
          ((string-match re:definition-list line)
           (definition-list))
          ((string-match re:preformatted line)
           (preformatted))
          ((string-match re:definition-tag line)
           (definition-block))
          ((horizontal-rule? line) => horizontal-rule)
          ((enscript? line) => enscript)
          ((nowiki? line)   => nowiki)
          ((table? line)    => table)
          ((examples? line) => examples)
          ((directive? line) => directive)
          ;; WARNING: If a line is not matched above but does match re:block,
          ;; then (paragraph) will enter an infinite loop.
          (else (paragraph)))))

(define (directive? line) (string-match re:directive line))
(define directive
  (match-lambda ((_ dir args)
            (discard-line)
            (cond ((string=? dir "toc")
                   `(toc))
                  ((string=? dir "tags")
                   `(tags . ,(string-split args)))))))
(define (horizontal-rule? line) (string-match re:horizontal-rule line))
(define (horizontal-rule m)
  (discard-line)
  '(hr))

(define (parse-attrs str)         ; parse html attributes string, return alist
  (irregex-fold re:tag-attribute
                (lambda (i m s)
                  (cons (cons (string->symbol (irregex-match-substring m 1))
                              (or (irregex-match-substring m 2)
                                  (irregex-match-substring m 3)
                                  (irregex-match-substring m 4)))
                        s))
                '() str))
(define (enscript? line) (string-match re:enscript-tag-start line))
(define enscript
  (match-lambda ((_ attrs ln)
            (discard-line)
            (let ((attrs (parse-attrs attrs)))
              (let ((lang (cond ((or (assq 'highlight attrs)
                                     (assq 'language attrs))
                                 => (lambda (x) (string->symbol (cdr x))))
                                (else 'scheme))))
                `(script ,lang
                         ,(read-verbatim re:enscript-tag-end ln)))))))
(define (nowiki? line) (string-match re:nowiki-tag-start line))
(define nowiki
  (match-lambda ((_ ln)
            (discard-line)
            ;; Should be interpolated into result, but we can't do that
            (cdr (html->sxml (read-verbatim re:nowiki-tag-end ln))))))

(define (examples? line) (string-match re:examples-tag-start line))
;; Read <examples> block and pass through verbatim to sxml.
;; There will be some extraneous NLs.
(define examples
  (match-lambda ((_ ln)
            (discard-line)  ; --actually, we don't have to discard, we can just
                            ; --allow html->sxml to read the entire thing
            `(examples . ,(cdr (html->sxml
                                (read-verbatim re:examples-tag-end ln)))))))

;;; table handling

(define (table? line) (string-match re:table-tag-start line))
(use sxml-transforms)
(define (pre-post-order-text doc proc)
  (pre-post-order-splice doc `((*text* . ,(lambda (tag str)
                                            (proc str)))
                               (*default* . ,(lambda x x)))))
(define table
  (match-lambda ((_ tag ln)
            (discard-line)
            (let* ((table-str (string-append tag (read-verbatim re:table-tag-end ln)))
                   (table-sxml (cadr (html->sxml table-str))))
              ;; Transform inline elements in strings.  Fails when open/close pair
              ;; crosses strings.  Usual failure case is interceding char entity.
              (pre-post-order-text table-sxml inline)))))

;;; block start tag to end tag reading

;; Returns string with NL-delimited lines until end-re
;; orphaned start and end tags don't count as separate lines
(define (read-verbatim end-re ln)
  (unless (string=? ln "")    ;; special handling for orphan start tag
    (poke-line ln))
  (string-intersperse (read-until-end-tag end-re)
                      "\n"))
(define (read-until-end-tag end-re)  ; returns list of lines
  (let lp ((lines '()))
    (let ((line (read-buffered-line)))
      (cond ((eof-object? line)
             (reverse lines))
            ((string-search end-re line)
             => (match-lambda ((_ pre post)
                          (poke-line post)
                          (reverse
                           (if (string=? pre "") ; special handling for orphan end tag
                               lines
                               (cons pre lines))))))
            ;; NOTE: Abort if we hit a new section?
            (else
             (lp (cons line lines)))))))

;;; definitions (procedures, etc.)

(define (definition-block)
  `(def (sig . ,(definition-sigs))
        . ,(definition-body)))
(define (definition-sigs)
  (let ((line (peek-buffered-line)))
    (cond ((or (eof-object? line)
               (string=? line ""))
           (discard-line)
           '())
          ((string-match re:definition-tag line)
           => (match-lambda ((_ tag sig)
                        (discard-line)
                        (cons `(,(string->symbol tag) ,sig)
                              (definition-sigs)))))
          (else '()))))
(define (definition-body)
  (let ((line (peek-buffered-line)))
    (cond ((eof-object? line) '())
          ((string=? line "") (discard-line) (definition-body)) ; put in (block)?
          ((string-match re:header line)
           '())
          ((string-match re:definition-tag line)
           '())
          (else
           (cons (block)
                 (definition-body))))))

;;; lists

(define (item-list depth)
  (let ((line (peek-buffered-line)))
    (cond ((eof-object? line) '())
          ((string=? line "")
           (discard-line)
           (item-list depth))
          ((string-match re:unordered-list line)
           => (lambda (m)
                (let ((next-depth (string-length (cadr m))))
                  (cond ((> next-depth depth)
                         `(ul . ,(item-list-items next-depth)))
                        (else '())))))
          ((string-match re:ordered-list line)
           => (lambda (m)
                (let ((next-depth (string-length (cadr m))))
                  (cond ((> next-depth depth)
                         `(ol . ,(item-list-items next-depth)))
                        (else '())))))
          (else '()))))

(define (item-list-items depth)
  (let ((line (peek-buffered-line)))
    (cond ((eof-object? line) '())
          ((string=? line "")
           (discard-line)
           (item-list-items depth))
          ((or (string-match re:unordered-list line)
               (string-match re:ordered-list line))
           => (lambda (m)
                (let ((next-depth (string-length (cadr m)))
                      (item (caddr m)))
                  (cond ((> next-depth depth)
                         (item-list next-depth))
                        ((= next-depth depth)
                         (discard-line)
                         (let ((item (item-list-item (list item))))
                           (let* ((next-list (item-list next-depth))
                                  (next-list (if (pair? next-list) ; hack for proper nesting
                                                 (list next-list) '())))
                             ;; ITEM is inline not block and should be interpolated.
                             `((li ,@(inline item) . ,next-list)
                               . ,(item-list-items depth)))))
                        (else '())))))
          (else '()))))

;; List items may extend across lines; read the lines until reaching a block item
;; and coalesce them.  Special case: initial whitespace does not trigger a PRE block.
(define (item-list-item lines)
  (let ((line (peek-buffered-line)))
    (cond ((or (eof-object? line)
               (string=? line ""))
           (discard-line)
           (string-intersperse (reverse lines) " "))
          (else
           (let ((trimmed-line (string-trim-both line)))
             (cond ((string-match re:block trimmed-line)
                    (string-intersperse (reverse lines) " "))
                   (else (discard-line)
                         (item-list-item (cons trimmed-line lines)))))))))

(define (definition-list)
  `(dl . ,(definition-list-items)))

(define (definition-list-items)
  (let loop ((items '()))
    (let ((line (peek-buffered-line)))
      (cond ((eof-object? line)
             (reverse items))
            ((string=? line "")
             (discard-line)
             (loop items))
            ((string-match re:definition-list line)
             => (match-lambda ((_ term def)
                          (discard-line)
                          (loop `((dd . ,(inline def))
                                  (dt . ,(inline (string-trim-right term)))
                                  . ,items)))))
            (else (reverse items))))))

;;; pre

(define (preformatted)
  `(pre ,(slurp-preformatted)))
(define (slurp-preformatted)
  (let loop ((lines '()))
    (let ((line (peek-buffered-line)))
      (cond ((or (eof-object? line)
                 (string=? line ""))
             (discard-line)
             (string-intersperse (reverse lines) "\n"))
            ((string-match re:preformatted line)
             => (match-lambda ((_ text)
                          (discard-line)
                          (loop (cons text lines)))))
            (else
             (string-intersperse (reverse lines) "\n"))))))

;;; para

(define (paragraph)
  `(p . ,(inline (slurp-paragraph))))
(define (slurp-paragraph)
  (let loop ((lines '()))
    (let ((line (peek-buffered-line)))
      (cond ((or (eof-object? line)
                 (string=? line ""))
             (discard-line)
             (string-intersperse (reverse lines) " "))
            ((string-match re:block line)
             (string-intersperse (reverse lines) " "))
            (else (discard-line)
                  (loop (cons (string-trim-both line) lines)))))))

;;; svnwiki->sxml

(define (svnwiki->sxml in)
  (discard-line) ; clear buffer
  (with-input-from-port in
    (lambda ()
      (section-body 1))))

#|
document :: section-body
section :: section-header section-body
section-header :: ==+ Title
section-body :: block* section>*

|#


;;; Felix's wiki2html inline parser modified for sxml output

(require-library srfi-1 data-structures)
(import (only srfi-1 first second third find))
(import (only data-structures string-translate*))

;; inline elements

(define +code+ '(: #\{ #\{ (submatch (*? any)) #\} #\}))
(define +bold+ '(: (= 3 #\') (submatch (+ (~ #\'))) (= 3 #\')))
(define +italic+ '(: (= 2 #\') (submatch (+ (~ #\'))) (= 2 #\')))
(define +html-tag+ '(: #\< (submatch (+ (~ #\>))) #\>))
(define +nowiki+ '(: "<nowiki>" (submatch (*? any)) "</nowiki>"))

(define +link+
  '(: #\[ #\[ (submatch (* (~ #\] #\|))) (? #\| (submatch (* (~ #\])))) #\] #\]))
(define +image-link+
  '(: #\[ #\[ (* space) "image:" (* space)
      (submatch (* (~ #\] #\|))) (? #\| (submatch (* (~ #\])))) #\] #\]))
(define +inline-element+
  `(or ,+code+ ,+image-link+ ,+link+ ;; ,+html-tag+
       ,+nowiki+
       ,+bold+ ,+italic+))
(define +http-url+ '(: (* space) "http://" (* any)))

(define *manual-pages* '())
(define-constant rx irregex)

;; Parse nowiki contents as html; do no
;; further parsing for inline elements.
(define (nowiki-inline str)
  (cdr (html->sxml str)))

(define inline
  (let ((rx:inline-element (rx +inline-element+))
        (rx:code           (rx `(: bos ,+code+)))
        (rx:html-tag       (rx `(: ,+html-tag+)))
        (rx:image-link     (rx `(: bos ,+image-link+)))
        (rx:link           (rx `(: bos ,+link+)))
        (rx:http-url       (rx +http-url+))
        (rx:bold           (rx `(: bos ,+bold+)))
        (rx:italic         (rx `(: bos ,+italic+)))
        (rx:nowiki         (rx `(: bos ,+nowiki+))))
    (lambda (str)
      (let ((m (string-search-positions rx:inline-element str)))
        (if (not m)
            (if (string=? str "") '() (list str))
            (let ((before (substring str 0 (caar m)))
                  (after
                   (let ((rest (substring str (caar m))))
                     (define (continue m)
                       (inline (substring rest (string-length (first m)))))
                     (cond ((string-search rx:code rest) =>
                            (lambda (m)
                              (cons `(tt ,(second m))
                                    (continue m))))
                           ((string-search rx:image-link rest) =>
                            (lambda (m)
                              (cons `(image-link ,(second m))
                                    (continue m))))
                           ((string-search rx:link rest) =>
                            (lambda (m)
                              (let ((href (string-trim-both (second m))) ; ?
                                    (desc (third m)))
                                (cons
                                 (cond ;; ((find (cut string-ci=? <> m1) *manual-pages*)
                                       ;;  `(a (@ (href ,m1 ".html"))
                                       ;;      ,(inline m1)))
                                  ((string-match rx:http-url href)
                                   (if desc `(link ,href ,desc) `(link ,href)))
                                  ;; Note: internal links ("int-link") make no
                                  ;;  sense outside of the wiki unless they refer directly
                                  ;;  to an identifier or egg name.
                                  ;; Note: Ideally handle #xxx links as node pointers.
                                  ;; But hard to ensure a node is meant.
                                  (else
                                   (if desc `(int-link ,href ,desc) `(int-link ,href))))
                                 (continue m)))))
                           ((string-search rx:bold rest) =>
                            (lambda (m)
                              (cons `(b . ,(inline (second m)))
                                    (continue m))))
                           ((string-search rx:italic rest) =>
                            (lambda (m)
                              (cons `(i . ,(inline (second m)))
                                    (continue m))))
                           ((string-search rx:nowiki rest) =>
                            (lambda (m)
                              (append (nowiki-inline (second m))
                                      (continue m))))
                           (else (error "unknown inline match" m rest))))))
              (if (string=? before "")
                  after
                  (cons before after)))
              
              
            )

        )))))
