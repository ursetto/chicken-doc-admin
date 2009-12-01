;;; chicken-doc-admin

(include "chicken-doc-parser.scm")

(module chicken-doc-admin
;; Used by chicken-doc-admin command
(refresh-id-cache
 describe-repository
 delete-key
 parse-egg-directory parse-individual-egg
 parse-man-directory parse-individual-man
 create-repository!
 )

(import scheme chicken)
(require-library chicken-doc)
(import chicken-doc)
(use matchable srfi-69 posix regex data-structures files extras srfi-13 srfi-1)
(import irregex)

(import chicken-doc-parser)

;;; Locking

;; NOT SRFI-18 safe (multiple in-process locks don't block).
(define global-write-lock (make-parameter #f))
(define (acquire-global-write-lock!)
  (when (global-write-lock)
    ;; Not currently recursive.
      (error "Already acquired global write lock"))
  (let ((out (open-output-file (make-pathname (repository-base) "lock"))))
    (file-lock/blocking out)
    (global-write-lock out)))
(define (release-global-write-lock!)
  (unless (global-write-lock)
    (error "Releasing unlocked write lock"))
  (close-output-port (global-write-lock))
  (global-write-lock #f))
(define (with-global-write-lock thunk)
  (cond ((global-write-lock)
         (thunk))
        (else    ; FIXME use handle-exceptions
         (acquire-global-write-lock!)
         (handle-exceptions exn (begin
                                  (release-global-write-lock!)
                                  (signal exn))
           (thunk)
           (release-global-write-lock!)))))

;;; Util

(define (with-cwd dir thunk)          ;; FIXME: dynamic-wind
  (let ((old (current-directory)))
    (current-directory dir)
    (handle-exceptions exn (begin (current-directory old)
                                  (signal exn))
      (thunk)
      (current-directory old))))

;;; Lowlevel

;; PATH: a list or string key path.  TEXT: String to write to text
;; key, or #f to skip.  TYPE: key type (container types are 'unit and 'egg;
;; tag types are 'procedure, 'macro, etc.)  SIGNATURE:
;; Function signature as string; also used for a short description
;; of containers.
(define (write-key path text type sig)
  (let* ((keys (path->keys path))
         (pathname (keys->pathname keys)))
    (with-global-write-lock
     (lambda ()
       (create-directory pathname #t) ;; mkdir -p
       (with-cwd
        pathname
        (lambda ()
          (with-output-to-file (field-filename 'meta)
            (lambda ()
              (for-each (lambda (x)
                          (write x) (newline))
                        `((type ,type)
                          (signature ,sig)
                          ;; (identifier ,id)
                          ))))
          (if text
              (with-output-to-file (field-filename 'text)
                (lambda ()
                  (display text))))))))))

;; find-files follows symlinks, doesn't do depth first unless we cons up
;; everything, and doesn't include DIR itself; easier to write our own
(define (recursive-delete-directory dir)
  (for-each
   (lambda (x)
     (let ((fn (make-pathname dir x)))
       (cond ((symbolic-link? fn)
              (delete-file fn))
             ((not (directory? fn))
              (delete-file fn))
             (else
              (recursive-delete-directory fn)))))
   (directory dir #t))
  (delete-directory dir))

;; Warning: delete-key deletes recursively.
(define (delete-key path)
  (let ((pathname (keys->pathname (path->keys path))))
    (unless (directory? pathname)
      (error 'delete-key "No such path" path))
    (recursive-delete-directory pathname)))

;;; Repo manipulation

(define (create-repository!)
  ;; FIXME: initialization should not occur if the version is wrong
  (when (file-exists? (repository-magic))
    (error "Repository already exists at" (repository-base)))
  (create-directory (repository-base))
;; (create-directory (cdoc-root))         ;; Created automatically in write-key
  (with-output-to-file (repository-magic)
    (lambda () (pp `((version . ,repository-version))))))
(define (describe-repository)
;;   (print "Repository information:")
  (pp (cons `(location . ,(repository-base))
            (repository-information))))

;;; Hilevel parsing (units, eggs)

(define (write-tags tags tag-body path)
  (for-each (match-lambda ((type sig id)
                      (if id
                          (write-key (append path (list id))
                                     (string-concatenate-reverse
                                      (intersperse tag-body "\n"))
                                     type sig)
;;                        (warning "Skipped writing tag for signature" sig)
                          )))
            (reverse tags)))

;; Open output port to the text key, which is passed to the parser
;; to write a transformed wiki document.  Semi-dumb.
(define (open-output-text path)
  (open-output-file
   (keys+field->pathname (path->keys path) 'text)))

(define (write-eggshell path)
  (let ((name (last path)))
    (write-key path #f 'egg
               (string-append name " egg"))))
(define (write-manshell path name)
  (write-key path #f 'unit name))

;; (define +wikidir+ "~/scheme/chicken-wiki")
;; (define eggdir (make-parameter
;;                 (make-pathname `(,+wikidir+ "eggref" "4") #f)))
;; (define mandir (make-parameter
;;                 (make-pathname `(,+wikidir+ "man" "4") #f)))

(define (parse-egg fn path)
  (with-global-write-lock
   (lambda ()
     (write-eggshell path)
     (let ((t (open-output-text path)))
       (parse-and-write-tags/svnwiki fn (lambda (tags body)
                                          (write-tags tags body path))
                                     t)
       (close-output-port t))))
  #t)

(define (parse-man fn path name)
  (with-global-write-lock
   (lambda ()
     (write-manshell path name)
     (let ((t (open-output-text path)))
       (parse-and-write-tags/svnwiki fn (lambda (tags body)
                                          (write-tags tags body path))
                                     t)
       (close-output-port t))))
  #t)

;;; svnwiki egg and man tree parsing

(define (parse-individual-egg pathname type)
  type ;ignored
  (let ((name (pathname-file pathname)))
    (and (regular-file? pathname)
         (parse-egg pathname `(,name)))))

(define (parse-egg-directory dir type)
  type ;ignored -- e.g. 'svnwiki
  (with-global-write-lock
   (lambda ()
     (for-each (lambda (name)
                 (when (parse-individual-egg (make-pathname dir name) 'svnwiki)
                   (print name)))
               (directory dir))
     (refresh-id-cache))))

(define (parse-individual-man pathname type)
  type ;ignored
  (let ((name (pathname-file pathname)))
    (let ((path (man-filename->path name)))
      (and path (regular-file? pathname)
           (parse-man pathname path name)))))

(define man-filename->path
  (let ((re:unit (irregex "^Unit (.*)"))
        (symbolify-list (lambda (x) (and x (map (lambda (x)
                                             (if (symbol? x) x (string->symbol x)))
                                           x)))))
    (lambda (t)
      (symbolify-list
       (cond ((string-search re:unit t) => cdr) ; ("lolevel")
             ((string=? t "Interface to external functions and variables")
              '(foreign))
             ((string=? t "Accessing external objects")
              '(foreign access))
             ((string=? t "C Interface")
              '(foreign c-interface))
             ((string=? t "Embedding")
              '(foreign embedding))
             ((string=? t "Foreign type specifiers")
              '(foreign types))
             ((string=? t "Callbacks")
              '(foreign callbacks))
             ((string=? t "Locations")
              '(foreign locations))
             ((string=? t "Other support procedures")
              '(foreign support))

             ((string=? t "Extensions")
              '(extensions))                      ;; FIXME

             ((string=? t "Declarations")
              '(chicken declarations))
             ((string=? t "Parameters")
              '(chicken parameters))
             ((string=? t "Non-standard macros and special forms")
              '(chicken macros))
             ((string=? t "Modules and macros")
              '(chicken modules))                  ;; FIXME

             ((string=? t "Using the interpreter")
              '(csi))
             (else #f))))))

(define (parse-man-directory dir type)
  type ;ignored
  (with-global-write-lock
   (lambda ()
     (for-each (lambda (name)
                 (when (parse-individual-man (make-pathname dir name) 'svnwiki)
                   (print name)))
               (directory dir))
     (refresh-id-cache))))

;;; ID search cache (write) -- perhaps should be in chicken-doc proper

(define (write-id-cache!)
  (let ((tmp-fn (string-append (id-cache-filename) ".tmp")))
    (with-output-to-file tmp-fn
      (lambda () (write (hash-table->alist (id-cache)))))
    (rename-file tmp-fn (id-cache-filename))
    (id-cache-mtime (current-seconds)
                    ;; (file-modification-time (id-cache-filename))
                    )))
(define (refresh-id-cache)
  (with-global-write-lock
   (lambda ()
     (with-cwd (repository-root)
               (lambda ()
                 (id-cache (make-hash-table eq?))
                 (for-each id-cache-add-directory!
                           (find-files "" directory?))))
;;      (print "Writing ID cache...")
     (write-id-cache!))))


)  ;; end module