(define file (car (command-line-arguments)))

(use chicken-syntax)
(use extras) ; read-file

(let ((str
       (condition-case
        (eval `(begin
                 (use eggdoc eggdoc-svnwiki)
                 (eggdoc:warnings #f)
                 (eggdoc:svnwiki-override!)
                 (with-output-to-string
                   (lambda ()
                     ,@(read-file file)))
                 ))
        (e (exn)
           (apply warning
                  (string-append "Parse failure: "
                                 (let ((loc ((condition-property-accessor 'exn 'location) e)))
                                   (if loc (conc loc ": ") ""))
                                 (or ((condition-property-accessor 'exn 'message) e) ""))
                  ((condition-property-accessor 'exn 'arguments) e))
           #f))))
  (when str
    (print str)))


