(module chicken-doc-admin-cmd (main)

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use chicken-doc-admin)
  (require-library chicken-doc)
  (import (only chicken-doc verify-repository))
  (use ports))
 (else
  (import (chicken base)
          (chicken platform)
          (chicken port)
          (chicken process-context))
  (import chicken-doc-admin)
  (import (only chicken-doc verify-repository))
  ))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage: " (program-name) " COMMAND")
      (print "  -l             list repository information")
      (print "  -i             initialize repository non-destructively")
      (print "  -e dir  [root] process egg directory DIR [and store under node ROOT]")
      (print "  -E file [path] process egg file FILE [and store to node PATH]")
      (print "  -m dir         process manual directory DIR")
      (print "  -M file [path] process manual file FILE [and store to node PATH]")
      (print "  -H             process docs for all installed eggs on this host")
      (print "  -H egg ...     process docs for one or more installed eggs on this host")
      (when (feature? #:cross-chicken)
        (print "  -T             process docs for all installed eggs on cross-compiler target")
        (print "  -T egg ...     process docs for one or more installed eggs on cross-compiler target"))
      (print "  -t type        document type (valid with -e -E -m -M)")
      (print "                    types: eggdoc, svnwiki (default: svnwiki)")
      (print "  -r             regenerate indices (only if broken)")
      (print "  -f             force processing (ignore timestamp checks)")
      (print "  -d path        delete node PATH recursively (EMPTY PATH DELETES ENTIRE REPO)")
      (print "  -D             destroy entire repository")      
      (exit 1))))

(define (main)
  (let loop ((type 'svnwiki)
             (args (command-line-arguments))
             (force? #f))
    (when (null? args) (usage))
    (let ((o (car args))
          (r (cdr args)))
      (cond ((string=? o "-i")
             (create-repository!))
            ((string=? o "-D")
             (destroy-repository!))
            (else
             (verify-repository) ;; throws error on failure
             (cond ((string=? o "-r")
                    (print "Rebuilding ID cache...")
                    (refresh-id-cache))
                   ((string=? o "-l")
                    (describe-repository))
                   ((string=? o "-d")
                    (delete-key (map string->symbol r)))
                   ((string=? o "-t")
                    (unless (pair? r) (usage)) ;; adhoc arg parsing getting out of hand
                    (loop (string->symbol (car r)) (cddr args) force?))
                   ((string=? o "-f")
                    (loop type r #t))
                   ((string=? o "-e")
                    (unless (pair? r) (usage))
                    (parse-egg-directory (car r) type
                                         ;; root path specification
                                         (cdr r)
                                         force?))
                   ((string=? o "-m")
                    (unless (pair? r) (usage))
                    (parse-man-directory (car r) type force?))
                   ((string=? o "-E")
                    (unless (pair? r) (usage))
                    (unless (parse-individual-egg (car r) type
                                                  ;; path override
                                                  (and (pair? (cdr r)) (cdr r))
                                                  force?)
                      (error "Unable to parse egg file" (car r))))
                   ((string=? o "-M")
                    (unless (pair? r) (usage))
                    (unless (parse-individual-man (car r) type
                                                  (and (pair? (cdr r)) (cdr r))
                                                  force?)
                      ;; Might return #f when path required but not provided
                      ;; (so message is misleading).
                      (error "Unable to parse man file" (car r))))
                   ((string=? o "-H")
                    (exit (if (parse-installed-eggs r type force?) 0 1)))
                   ((string=? o "-T")
                    ;; On non-cross-compilers this is equivalent to -H.
                    (exit (if (parse-installed-eggs r type force? 'target) 0 1)))
                   (else
                    (usage))))))))

)

(import chicken-doc-admin-cmd)
(main)
