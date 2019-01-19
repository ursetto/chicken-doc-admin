(module chicken-doc-parser (parse-svnwiki extract-definitions)

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use extras)
  (use ports)
  (use svnwiki-sxml)
  (require-library srfi-1)
  (import (only srfi-1 append-map)))
 (else
  (import (chicken base))
  (import svnwiki-sxml)
  (import (only srfi-1 append-map))
  ))

;; Read svnwiki format text file at pathname (or port) FN and
;; parse to SXML, returning parsed sxml doc.
(define (parse-svnwiki fn-or-port)
  (parameterize ((svnwiki-signature-parser svnwiki-signature->identifier))
    (cond ((port? fn-or-port)
           (svnwiki->sxml fn-or-port))
          (else
           (call-with-input-file fn-or-port svnwiki->sxml))))) ;unsafe for exns

;; Return a list of DEF blocks in sxml document DOC.
(define (extract-definitions doc)
  (define (gather doc sym)              ;(sxpath '(// sym))
    (cond ((null? doc) '())
          ((pair? doc)
           (if (eq? (car doc) sym)
               (list doc)
               (append-map (lambda (x) (gather x sym))
                           doc)))
          (else '())))
  (gather doc 'def))

)

