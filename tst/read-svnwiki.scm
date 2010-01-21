(define file (car (command-line-arguments)))

(use chicken-syntax)
(use extras) ; read-file

(print
 (eval `(begin
          (use eggdoc eggdoc-svnwiki)
          (eggdoc:warnings #f)
          (eggdoc:svnwiki-override!)
          (with-output-to-string
            (lambda ()
              ,@(read-file file))))
       (interaction-environment)))


