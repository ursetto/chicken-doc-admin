#!/usr/bin/env csi4 -script

(require-library chicken-doc-admin)
(use ports)

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage: " (program-name) " COMMAND")
      (print "  -l           list repository information")
      (print "  -i           initialize repository non-destructively")
      (print "  -d path      delete key path recursively (EMPTY PATH DELETES ENTIRE REPO)")
      (print "  -e dir       process svnwiki egg directory DIR")
      (print "  -m dir       process svnwiki manual directory DIR")
      (print "  -E file      process svnwiki egg file FILE")
      (print "  -M file path process svnwiki man file FILE, placing at key path PATH")
      (print "  -r           regenerate indices (required after -E or -M)")
      (exit 1))))

(when (null? (command-line-arguments))
  (usage))

(let ((o (car (command-line-arguments)))
      (r (cdr (command-line-arguments))))
  (cond ((string=? o "-i")
         (create-repository!))
        (else
         (unless (verify-repository)
           (fprintf (current-error-port)
                    "No repository found at ~a\nUse -i to initialize\n" (repository-base))
           (exit 1))
         (cond ((string=? o "-r")
                (print "Rebuilding ID cache...")
                (refresh-id-cache))
               ((string=? o "-l")
                (describe-repository))
               ((string=? o "-d")
                (delete-key (map string->symbol r)))
               ((string=? o "-e")
                (unless (pair? r) (usage))
                (parse-egg-directory (car r) 'svnwiki))
               ((string=? o "-m")
                (unless (pair? r) (usage))
                (parse-man-directory (car r) 'svnwiki))
               ((string=? o "-E")
                (unless (pair? r) (usage))
                (parse-individual-egg (car r) 'svnwiki))
               ((string=? o "-M")
                (unless (pair? r) (usage))
                (parse-individual-man (car r) 'svnwiki))
               (else
                (usage))))))
