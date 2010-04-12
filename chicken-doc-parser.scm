(module chicken-doc-parser (parse-svnwiki extract-definitions signature->identifier)

(import scheme chicken)
(use regex) (import irregex)
(use extras)
(use ports)
(use svnwiki-sxml)

(define +rx:ivanism+
  (irregex '(: ":" eos)))

;; Convert signature (usually a list or bare identifier) into an identifier
;; At the moment, this just means taking the car of a list if it's a list,
;; or otherwise returning the read item.  If it cannot be read as a
;; scheme expression, fail.
(define (signature->identifier sig type)
  (condition-case
   (let ((L (with-input-from-string sig read)))
     (cond ((pair? L) (car L))
           ((symbol? L)
            ;; SPECIAL HANDLING: handle e.g. MPI:init:: -> MPI:init.
            ;; Remove this once these signatures are normalized.
            ;; (Warning: usually read as keywords, if so symbol->string
            ;;  will strip one : itself)
            (let ((str (irregex-replace +rx:ivanism+
                                        (symbol->string L)
                                        "")))
              (if str (string->symbol str) L)))
           (else sig)))
   ((exn)
    (warning "Could not parse signature" sig)
    #f)))

;; Read svnwiki format text file at pathname (or port) FN and
;; parse to SXML, returning parsed sxml doc.
(define (parse-svnwiki fn-or-port)
  (cond ((port? fn-or-port)
         (svnwiki->sxml fn-or-port))
        (else
         (call-with-input-file fn-or-port svnwiki->sxml))))  ;unsafe for exns

(use sxpath)   ; FIXME replace with walker

;; Return a list of DEF blocks in sxml document DOC.
(define extract-definitions
  (let ((defs (sxpath '(// def))))
    (lambda (doc)
      (defs doc))))

)

