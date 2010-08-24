;;; chicken-doc-admin

(include "chicken-doc-parser.scm")

(use chicken-syntax) ; for eggdoc eval

(module chicken-doc-admin
;; Used by chicken-doc-admin command
(refresh-id-cache
 describe-repository
 delete-key
 parse-egg-directory parse-individual-egg
 parse-man-directory parse-individual-man
 create-repository!
 destroy-repository!

 man-filename->path       ; for chickadee
 )

(import scheme chicken)
(require-library chicken-doc)
(import chicken-doc)
(use matchable srfi-69 posix regex data-structures files extras srfi-13 srfi-1)
(import irregex)
(use setup-download)
(use ports)

(import chicken-doc-parser)

;;; Locking

;; NOT SRFI-18 safe (multiple in-process locks don't block).
(define global-write-lock (make-parameter #f))
(define (acquire-global-write-lock!)
  (when (global-write-lock)
    ;; Not currently recursive.
      (error "Already acquired global write lock"))
  (let ((r (current-repository)))
    (let ((out (open-output-file (make-pathname (repository-base r)
                                                "lock"))))
      #+(not mingw32) (file-lock/blocking out)
      (global-write-lock out))))
(define (release-global-write-lock!)
  (unless (global-write-lock)
    (error "Releasing unlocked write lock"))
  (close-output-port (global-write-lock))
  (global-write-lock #f))
(define (with-global-write-lock thunk)
  (cond ((global-write-lock)
         (thunk))
        (else
         (acquire-global-write-lock!)
         (handle-exceptions exn (begin
                                  (release-global-write-lock!)
                                  (signal exn))
           (thunk)
           (release-global-write-lock!)))))

;;; Util

(define (with-cwd dir thunk)
  (let ((old (current-directory)))
    (current-directory dir)
    (handle-exceptions exn (begin (current-directory old)
                                  (signal exn))
      (let ((rv (thunk)))
        (current-directory old)
        rv))))

;;; Lowlevel

;; PATH: a list path of strings or symbols.  SXML: sxml doc to write to 'sxml
;; key, or #f to skip.  TYPE: key type (container types are 'unit and 'egg;
;; tag types are 'procedure, 'macro, etc.)  SIGNATURE:
;; Function signature as string; also used for a short description
;; of containers.  TIMESTAMP: Source file update time in seconds since UNIX epoch,
;; or #f for no timestamp.
(define (write-key path sxml type sig timestamp)
  (let* ((keys (path->keys path))
         (pathname (keys->pathname keys)))
    (let* ((sxml-str (and sxml (with-output-to-string
                                 (lambda () (write sxml)))))
           (merge-sxml? (and sxml-str
                             (< (string-length sxml-str) 3072))))
      (with-global-write-lock
       (lambda ()
         (create-directory pathname #t) ;; mkdir -p
         (call-with-output-field
          path 'meta
          (lambda (p)
            (for-each (lambda (x)
                        (write x p) (newline p))
                      `((type ,type)
                        (signature ,sig)
                        ,@(if timestamp
                              `((timestamp ,timestamp))
                              '())
                        ,@(if (and sxml-str merge-sxml?)
                              `((sxml ,sxml))  ; WRITE won't work with sxml-str
                              '())
                        ))))
         (if (and sxml-str (not merge-sxml?))
             (call-with-output-field
              path 'sxml
              (lambda (p) (display sxml-str p))))
         (working-id-cache-add! path))))))

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
  (let ((r (make-repository-placeholder
             (locate-repository))))
    (when (file-exists? (repository-magic r))
      (error "Repository already exists at" (repository-base r)))
    (print "Creating repository at " (repository-base r) "...")
    (create-directory (repository-base r))
    (create-directory (repository-root r))
    (with-output-to-file (repository-magic r)
      (lambda () (pp (repository-information r))))))

(define (describe-repository)
;;   (print "Repository information:")
  (let ((r (current-repository)))
    (pp (cons `(location . ,(repository-base r))
              (repository-information r)))))
(define (destroy-repository!)
  (let ((r (current-repository)))
    (unless (file-exists? (repository-magic r))
      (error "No repository found at" (repository-base r)))
    (print "Destroying repository at " (repository-base r) "...")
    (recursive-delete-directory (repository-base r))))

;;; Hilevel parsing (units, eggs)

(define (write-definitions path defs ts)
  (for-each (lambda (def) (write-definition path def ts))
            defs))
(define (write-definition path def ts)
  (define (write-definition-key path id def type sig)
    (write-key (append path (list id))
               def type sig ts))   ;; don't bother timestamping
  (match def
         (('def ('sig . sigs) . body)
          (for-each
           (lambda (s)
             (match s
                    ((type sig)
                     (let ((id (signature->identifier sig type)))
                       (if id
                           ;; Skip non-parseable IDs.  We don't want gigantic keys.
                           ;; FIXME: For READ (read-syntax) type, maybe we do.
                           (write-definition-key path id def type sig))))))
           sigs))))

(define (call-with-output-field path field proc)
  (call-with-output-file
      (keys+field->pathname (path->keys path) field)
    proc))

(define (write-eggshell path doc timestamp)
  (let ((name (last path)))
    (write-key path doc 'egg
               (string-append name " egg")
               timestamp)))
(define (write-manshell path name doc timestamp)
  (write-key path doc 'unit name timestamp))

;; FIXME: PATH is expected to be list of strings, due to requirement in write-eggshell
(define (parse-egg/svnwiki fn-or-port path timestamp)
  (with-global-write-lock           ;; FIXME remove once locked in parse-individual-egg
   (lambda ()
     (let ((sxml-doc (parse-svnwiki fn-or-port)))
       (write-eggshell path sxml-doc timestamp)
       (write-definitions path (extract-definitions sxml-doc) timestamp))))
  #t)

(define (parse-man/svnwiki fn-or-port path name timestamp)
  (with-global-write-lock           ;; FIXME remove once locked in parse-individual-man
   (lambda ()
     (let ((sxml-doc (parse-svnwiki fn-or-port)))
       (write-manshell path name sxml-doc timestamp)
       (write-definitions path (extract-definitions sxml-doc) timestamp))))
  #t)

(define eggdoc-svnwiki-available?
  (let ((avail? (delay (condition-case
                        (eval '(begin (use eggdoc-svnwiki eggdoc) #t))
                        ((exn) #f)))))
    (lambda ()
      (force avail?))))

(define (parse-egg/eggdoc fn path timestamp)
  (unless (eggdoc-svnwiki-available?)
    (##sys#clear-trace-buffer)           ; you are a horrible person
    (error "eggdoc-svnwiki is required but not installed.\nUse `chicken-install eggdoc-svnwiki` to install it."))
  (let ((dir (pathname-directory fn))
        (file (pathname-strip-directory fn)))
    (let ((result
           (condition-case
            (with-cwd dir         ;; Change to eggdoc's basedir; may need local files
                      (lambda ()       ;; with-cwd not visible in eval, so do it outside
                        (let ((doc (read-file file))
                              (str (gensym 'str)))
                          (eval `(begin
                                   (use eggdoc) ; eggdoc-svnwiki loaded above
                                   (eggdoc:warnings #f)
                                   (eggdoc:svnwiki-override!)
                                   (let ((,str (with-output-to-string
                                                 (lambda () ,@doc))))
                                     (cons (cons 'text ,str)
                                           (eggdoc:result))))))))
            (e (exn)
               (apply warning
                      (string-append "Parse failure: "
                                     (let ((loc ((condition-property-accessor 'exn 'location) e)))
                                       (if loc (conc loc ": ") ""))
                                     (or ((condition-property-accessor 'exn 'message) e) ""))
                      ((condition-property-accessor 'exn 'arguments) e))
               #f))))
      ;(##sys#clear-trace-buffer)       ; we probably want a trace on eggdoc files
      (and (pair? result)
           (let ((name (alist-ref 'name result))
                 (text (alist-ref 'text result)))
             (unless (or path name)
               (error "Node path required for eggdocs"))
             (unless path
               (print name))
             (let ((path (or path `(,name))))
               (parse-egg/svnwiki (open-input-string text)
                                  path timestamp)))))))

;;; svnwiki egg and man tree parsing

;; Argument PATH allows computed path override.  It should be a list of strings.
;; The egg name SHOULD be printed to stdout if it is determined
;; programmatically (i.e. if PATH is #f); that is either done here
;; or in the individual parser if necessary.
;; Returns #f if the parse fails (currently, if error or not a regular file).
;; Returns 'added if the node was new, 'modified if the node existed but was modified,
;; and 'unchanged if the node was unchanged (based on timestamp comparison).
;; Note: Timestamp comparison cannot be done for eggdoc nodes as we do not
;; know the egg node name until after the document is parsed.
(define (parse-individual-egg pathname type #!optional path force?)
  (case type
    ((svnwiki)
     (and (regular-file? pathname)
          (let* ((basename (pathname-file pathname))
                 (path (or path `(,basename)))
                 (fts (file-modification-time pathname)))
            (let* ((node (handle-exceptions e #f (lookup-node path)))  ;; unfortunate API kink
                   (nts (if node (or (node-timestamp node) 0) 0))
                   (ntype (if node (node-type node) 'none)))
              (or (and (not force?)
                       (eq? ntype 'egg)
                       (<= fts nts)
                       'unchanged)
                  (and (parse-egg/svnwiki pathname path fts)
                       (if node 'modified 'added)))))))
    ((eggdoc)
     (and (regular-file? pathname)
          (let ((fts (file-modification-time pathname)))
            ;; Force node to #f, leave code scaffolding in place for future timestamp handling
            (let* ((node #f    ;; (handle-exceptions e #f (lookup-node path))
                         )
                   (nts (if node (or (node-timestamp node) 0) 0))
                   (ntype (if node (node-type node) 'none)))
              (or (and (not force?)
                       (eq? ntype 'egg)
                       (<= fts nts)
                       'unchanged)
                  (and (parse-egg/eggdoc pathname path fts)
                       (if node 'modified 'added)))))))
    (else
     (error "Invalid egg document type" type))))

(define ignore-filename?
  ;; Ignore not just #*# but #* due to issue with r/w invariance on sharp-syntax
  ;; in older Chicken.
  (let ((re:ignore (regexp "^#|\\.swp$|~$")))
    (lambda (fn)
      (string-search re:ignore fn))))

(define (parse-egg-directory dir type #!optional force?)
  (let ((egg-count 0) (updated 0))
    (with-global-write-lock
     (lambda ()
       (init-working-id-cache!)
       (case type
         ((svnwiki)
          (for-each (lambda (name)
                      ;; Can't count errors yet as we don't distinguish between non-regular files and errors.
                      ;; Therefore, don't include error/non-regular files in processed report.
                      (let ((code (parse-individual-egg (make-pathname dir name) type #f force?)))
                        (when code
                          (set! egg-count (+ egg-count 1)))
                        ;; Must print ONLY after successful parse, otherwise
                        ;; directories etc. will show up.  Any parse warnings
                        ;; will occur before the name appears.
                        (case code
                          ((added modified)
                           (set! updated (+ updated 1))
                           (print name))
                          ((unchanged)))))
                    (remove ignore-filename? (directory dir))))

         ((eggdoc)
          (print "Gathering egg information...")
          (let ((re:dir (irregex `(: bos ,(make-pathname dir ""))))) ; strip off dir name
            (for-each (lambda (pathname)
                        (let ((pretty-path
                               (string-substitute re:dir "" pathname)))
                          (display pretty-path) (display " -> ") (flush-output)
                          (let ((code (parse-individual-egg pathname type #f force?))) ; eggname printed in parse-egg/eggdoc
                            (when code
                              (set! egg-count (+ egg-count 1)))
                            (case code
                              ((added modified)
                               (set! updated (+ updated 1)))
                              ((unchanged))))))
                      (gather-eggdoc-pathnames dir))))
         (else
          (error "Invalid egg directory type" type)))
       (commit-working-id-cache!)
       (printf "~a eggs processed, ~a updated\n" egg-count updated)))))

;; Return list of eggdoc pathnames gathered from egg metadata in local
;; repository DIR.  Latest tagged version (failing that, trunk) is used.
;; Egg name is discarded--caller must gather it from the (name) elt in the doc.
(define (gather-eggdoc-pathnames dir)
  (filter-map
   (lambda (egg)
     (let ((x (alist-ref 'eggdoc (cdr egg))))
       (and x
            (pair? x)      ; the occasional egg may just have (eggdoc)
            (let* ((egg-name (->string (car egg)))
                   (filename (car x))
                   (pathname (locate-egg/local egg-name dir)))
              (make-pathname pathname filename)))))
   (gather-egg-information dir)))

(define (parse-individual-man pathname type #!optional path force?)
  (case type
    ((svnwiki)
     (let ((name (pathname-file pathname)))
       (let ((path (or path (man-filename->path name))))
         (and (regular-file? pathname)
              path
              (let* ((fts (file-modification-time pathname))
                     (node (handle-exceptions e #f (lookup-node path)))
                     (nts (if node (or (node-timestamp node) 0) 0))
                     (ntype (if node (node-type node) 'none)))
                (or (and (not force?)
                         (eq? ntype 'unit)   ;; FIXME unit?  what an odd design decision
                         (<= fts nts)
                         'unchanged)
                    (and (parse-man/svnwiki pathname path name fts)
                         (if node 'modified 'added))))))))
    (else
     (error "Invalid man document type" type))))

(define man-filename->path
  (let ((re:unit (irregex "^Unit (.+)"))
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
             ((string=? t "C interface")
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
             ((string=? t "Declarations")
              '(chicken declarations))
             ((string=? t "Parameters")
              '(chicken parameters))
             ((string=? t "Exceptions")
              '(chicken exceptions))
             ((string=? t "Modules and macros")    ;; legacy
              '(chicken modules))
             ((string=? t "Modules")
              '(chicken modules))
             ((string=? t "Macros")
              '(chicken macros))
             ((string=? t "Non-standard macros and special forms")
              '(chicken special-forms))
             ((string=? t "The R5RS standard")
              '(scheme))
             ((string=? t "Using the interpreter")
              '(csi))
             ((string=? t "Using the compiler")
              '(csc))
             ;; These are general reading pages which do not
             ;; contain identifiers.  They're in (chicken)
             ;; right now even though they don't really reside
             ;; in that namespace.
             ((string=? t "Extensions")
              '(chicken eggs))
             ((string=? t "Acknowledgements")
              '(chicken acknowledgements))
             ((string=? t "Bibliography")
              '(chicken bibliography))
             ((string=? t "Bugs and limitations")
              '(chicken bugs))
             ((string=? t "Data representation")
              '(chicken data-representation))
             ((string=? t "Deviations from the standard")
              '(chicken standard-deviations))
             ((string=? t "Extensions to the standard")
              '(chicken standard-extensions))
             ((string=? t "Getting started")
              '(chicken intro))
             ((string=? t "Non-standard read syntax")
              '(chicken read-syntax))
             ((string=? t "Basic mode of operation")
              '(chicken basic-operation))
             ((string=? t "Deployment")
              '(chicken deployment))
             ((string=? t "faq")
              '(chicken faq))
             ;; Hack because many internal links to "FAQ" exist.
             ;; Possibly we should ignore case altogether.
             ((string=? t "FAQ")
              '(chicken faq))
             ((string=? t "Supported language")
              '(chicken language))
             ;; User's Manual is kind of apropos, though "Supported language"
             ;; actually resides at toplevel.
             ((string=? t "The User's Manual")
              '(chicken))
             (else #f))))))

(define (parse-man-directory dir type #!optional force?)
  (let ((egg-count 0) (updated 0))
    (with-global-write-lock
     (lambda ()
       (init-working-id-cache!)
       (case type
         ((svnwiki)
          (for-each (lambda (name)
                      (let ((code (parse-individual-man (make-pathname dir name) 'svnwiki #f force?)))
                        (when code
                          (set! egg-count (+ egg-count 1)))
                        (case code
                          ((added modified)
                           (set! updated (+ updated 1))
                           (print name))
                          ((unchanged)))))
                    (remove ignore-filename? (directory dir))))
         (else
          (error "Invalid man directory type" type)))
       (commit-working-id-cache!)
       (printf "~a man pages processed, ~a updated\n" egg-count updated)))))

;;; ID search cache (write) -- perhaps should be in chicken-doc proper

;; Working ID cache used by WRITE-KEY etc.  The repository cache is
;; shared and its hash table contents cannot be mutated.
(define working-id-cache (make-parameter 'uninitalized-working-id-cache))

;; Read ID cache and copy it into working-id-cache.  Ensure you have a global
;; write lock so that the cache cannot change out from under you.
(define (init-working-id-cache!)
  (validate-id-cache! (current-repository))
  (working-id-cache (hash-table-copy
                     (id-cache-table
                      (repository-id-cache
                       (current-repository))))))
(define (working-id-cache-add! path)
  (let ((path (map (lambda (x) (if (string? x) (string->symbol x) x))
                   path)))
    (let ((id (last path))
          (parent (butlast path))
          (ht (working-id-cache)))
      (hash-table-update!/default ht id
                                  (lambda (old)
                                    (if (member parent old)
                                        old
                                        (cons parent old)))
                                  '()))))
(define (working-id-cache-delete! path)
  (let ((path (map (lambda (x) (if (string? x) (string->symbol x) x))
                   path)))
    (let ((id (last path))
          (parent (butlast path))
          (ht (working-id-cache)))
      (let ((old (hash-table-ref/default ht id #f)))
        (if old
            (if (null? old)
                (hash-table-delete! ht id)
                (hash-table-set! ht id
                                 (remove (lambda (x) (equal? x parent)) old))))))))
(define (commit-working-id-cache!)
  (write-id-cache! (working-id-cache)))

(define (write-id-cache! ht)    ; write HT to current cache and update repo's cache
  (define (write-id-cache c ht)   ; disk write table HT to cache C, return new cache obj
    (let* ((fn (id-cache-filename c))
           (tmp-fn (string-append fn ".tmp"))) ; fixme: mktmp
      (with-output-to-file tmp-fn
        (lambda () (write (hash-table->alist ht))))
      #+mingw32 (when (file-exists? fn)
                  (delete-file fn)) ;; Lose atomic update on MinGW.
      (rename-file tmp-fn fn)
      (make-id-cache
       ht
       (current-seconds)    ;; (file-modification-time (id-cache-filename))
       (id-cache-filename c))))
  (let* ((r (current-repository))
         (c (repository-id-cache r)))
    (set-repository-id-cache!
     r (write-id-cache c ht))))

;; Probably we could convert this to use the working-id-cache.
(define (refresh-id-cache)
  (define (make-id-cache-table)
    (make-hash-table eq?))

  (define (id-cache-table-add! ht pathname)
    (let ((id (key->id (pathname-file pathname)))
          ;; We don't save the ID name in the value (since it is in the key)
          (val (map key->id (butlast (string-split pathname "/\\")))))   ;; hmm
      (hash-table-update!/default ht id (lambda (old) (cons val old)) '())))

  (with-global-write-lock
   (lambda ()
     (let ((r (current-repository)))
       (with-cwd (repository-root r)
                 (lambda ()
                   (let ((ht (make-id-cache-table)))
                     (for-each (lambda (pathname)
                                 (id-cache-table-add! ht pathname))
                               (find-files "" directory?))
                     (write-id-cache! ht))))))))


)  ;; end module
