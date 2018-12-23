;;; chicken-doc-admin

(include "chicken-doc-parser.scm")
(include "file-locking.scm")

(module chicken-doc-admin
;; Used by chicken-doc-admin command
(refresh-id-cache
 describe-repository
 delete-key
 parse-egg-directory parse-individual-egg
 parse-man-directory parse-individual-man
 parse-installed-eggs
 create-repository!
 destroy-repository!

 man-filename->path       ; for chickadee
 )

(import scheme)

;; Chicken 5 support:
;;  eggdoc parsing (-e) will not work:
;;  - gather-egg-information and locate-egg/local have been removed from Chicken 5. You would need
;;    to port the gather-egg-information egg to Chicken 5, and export locate-egg from there, to
;;    restore `-t eggdoc` access.
;;  eggdoc parsing (-E) will not work:
;;  - requires porting eggdoc-sxml to Chicken 5, and also figuring out / removing the eval code
;;    which allows eggdoc-sxml to be loaded dynamically.
;;  host parsing (-H) may or may not work for target compilers:
;;  - For target mode, uses foreign function calls to get repo location. Would like to replace this with an actual
;;    API. Also may not be worth it; functionality not likely used. Virtually no eggs install .wiki files,
;;    and target mode is probably even less used. For host eggs, repository-path API is used.


;; FIXME: Remove regex dep, if possible. string-substitute and string-search have different APIs.
;; FIXME: Remove foreign dep in C5, if we can get binary versions a standard way.
(cond-expand
 (chicken-4
  (import chicken)
  (use chicken-doc)
  (use matchable regex srfi-69 posix data-structures files extras srfi-13 srfi-1)
  (import irregex)
  (import foreign)  ;; for parse-installed-eggs
  (use setup-download)
  (use ports)
  (use chicken-syntax) ; for eggdoc eval
  )
 (else
  (import (chicken base)
          (chicken condition)
          (rename (chicken file)
                  (file-executable? file-execute-access?))   ; c4 compat
          (except (chicken file posix) file-lock/blocking)   ; 5.0.0 bugfix in file-locking.scm
          (chicken foreign)
          (chicken format)
          (rename (only (chicken io) read-list)
                  (read-list read-file)) ; c4 compat
          (chicken irregex)
          (chicken platform)
          (chicken pathname)
          (chicken port)
          (chicken pretty-print)
          (chicken process-context)
          (chicken sort)
          (chicken string)
          (chicken time))
  (import matchable srfi-1 srfi-13 srfi-69)
  (import chicken-doc)
  (import regex)

  )
 )

(import chicken-doc-parser)
(import chicken-doc-file-locking)


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
           (let ((rc (thunk)))
             (release-global-write-lock!)
             rc)))))

;;; Util

(define (with-cwd dir thunk)
  (let ((old (current-directory)))
    (change-directory dir)
    (handle-exceptions exn (begin (change-directory old)
                                  (signal exn))
      (let ((rv (thunk)))
        (change-directory old)
        rv))))

;;; Lowlevel

;; PATH: a list path of strings or symbols.  SXML: sxml doc to write to 'sxml
;; key, or #f to skip.  TYPE: key type (container types are 'unit and 'egg;
;; tag types are 'procedure, 'macro, etc.)  SIGNATURE:
;; Function signature as string; also used for a short description
;; of containers.  TIMESTAMP: Source file update time in seconds since UNIX epoch,
;; or #f for no timestamp.
(define (write-doc-node path sxml type sig timestamp)
  (write-key path sxml type sig timestamp)
  (write-definitions path (extract-definitions sxml) timestamp))

(define (write-key path sxml type sig timestamp)
  (let* ((keys (path->keys path))
         (pathname (keys->pathname keys)))
    (let* ((sxml-str (and sxml (with-output-to-string
                                 (lambda () (write sxml)))))
           (merge-sxml? (and sxml-str
                             (< (string-length sxml-str) 3072))))
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
      (working-id-cache-add! path))))

;; find-files follows symlinks, doesn't do depth first unless we cons up
;; everything, and doesn't include DIR itself; easier to write our own
(define (recursive-delete-directory dir)
;;(define (delete-file fn) (print "deleting file " fn))
;;(define (delete-directory dir) (print "deleting dir  " dir))
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

;; delete-key deletes recursively.
;; Deleting root key deletes via plain recursive-delete-directory and wipes out index,
;; rather than traversing children.

(define (delete-key path)
  (with-global-write-lock
   (lambda ()
     (cond ((null? path)
            (let ((root (repository-root (current-repository))))
              (recursive-delete-directory root)
              (create-directory root)
              (empty-working-id-cache!)
              (commit-working-id-cache!)))
           (else
            (init-working-id-cache!)
            (delete-node (lookup-node path))   ;; Child deletion failure will leave id cache uncommitted.
            (commit-working-id-cache!))))))

(define (delete-node n)
  (for-each (lambda (c)
              ;; This is silly.  We should just get the list of real container (topic?) nodes
              ;; and delete them.
              (unless (node-definition-id? n (node-id c))
                (delete-node c)))
            (node-children n))
  (working-id-cache-delete! (node-path n))
  (for-each (lambda (defid)
              (working-id-cache-delete! (append (node-path n)
                                                (list defid))))
            (node-definition-ids n))
  (recursive-delete-directory
   (keys->pathname (path->keys (node-path n)))))

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
      (lambda () (pp (repository-information r))))
    (current-repository r)
    (write-id-cache! (make-id-cache-table))       ;; Empty index file required for working cache
    ))

(define (describe-repository)
;;   (print "Repository information:")
  (let ((r (current-repository)))
    (pp (cons `(location . ,(repository-base r))
              (repository-information r)))))
;; Unlike every other procedure, destroy-repository! can destroy old repository versions.
;; It does NOT use the current-repository; instead it uses the special open-repository*
;; call which can open old versions (if readable).
(define (destroy-repository!)
  (let ((r (open-repository* (locate-repository))))
    (let ((version (or (alist-ref 'version (repository-information r)) 0)))
      (case version
        ((1 2 3)
         (print "Destroying version " version " repository at " (repository-base r) "...")
         (recursive-delete-directory (repository-base r)))
        (else
         (error 'destroy-repository! "Unable to destroy repository version" version))))))

;;; Hilevel parsing (units, eggs)

(define (write-definitions path defs ts)
  (define (delete-definitions n) ;; Delete defs from working cache.  Doesn't delete ,defs
    (for-each (lambda (defid)
                (working-id-cache-delete! (append (node-path n)
                                                  (list defid))))
              (node-definition-ids n)))

  (delete-definitions (lookup-node path))
  (let next-def ((defs defs) (index '()) (defstrs '()) (offset 0))
    (cond ((null? defs)
           (when (pair? index)
             (call-with-output-field
              path 'defs
              (lambda (p)
                (write `(index . ,(reverse index)
                               ;; ,(sort index (lambda (x y)
                               ;;                (string< (car x) (car y))))
                               )
                       p)
                (newline p)
                (for-each (lambda (s) (display s p))
                          (reverse defstrs))))))
          (else
           (match (car defs)
                  (('def ('sig . sigs) . body)
                   (let next-sig ((sigs sigs)
                                  (index index)
                                  (parsed? #f))  ;; #t if at least one sig parsed in this def
                     (if (null? sigs)
                         (if parsed?
                             ;; FIXME: We should strip out the id, it wastes space.
                             (let ((str (with-output-to-string (lambda () (write (car defs)) (newline)))))
                               (next-def (cdr defs) index (cons str defstrs)
                                         (+ offset (string-length str))))
                             (next-def (cdr defs) index defstrs offset))
                         (match (car sigs)
                                ((type sig . alist)
                                 (cond ((cadr (assq 'id alist))
                                        => (lambda (id)   ;; signature parsed by svnwiki-sxml
                                             (working-id-cache-add! (append path (list id)))
                                             (next-sig (cdr sigs) (cons `(,(->string id) ,offset) index)
                                                       #t)))
                                       (else (warning "could not parse signature" sig)
                                             (next-sig (cdr sigs) index parsed?)))
                                       ;; Skip non-parseable IDs.  We don't want gigantic keys.
))))))))))

(define (call-with-output-field path field proc)
  (call-with-output-file
      (keys+field->pathname (path->keys path) field)
    proc
    #:binary))

(define (write-eggshell path doc timestamp)
  (let ((name (last path)))
    (write-doc-node path doc 'egg
                    (string-append name " egg")
                    timestamp)))
(define (write-manshell path name doc timestamp)
  (write-doc-node path doc 'unit name timestamp))

(define (warning/exception str e)
  (apply warning
         (string-append (if str (string-append str ": ") "")
                        (let ((loc ((condition-property-accessor 'exn 'location) e)))
                          (if loc (conc loc ": ") ""))
                        (or ((condition-property-accessor 'exn 'message) e) ""))
         ((condition-property-accessor 'exn 'arguments) e)))
(define (warn-on-exception str thunk)
  (handle-exceptions e
      (begin (warning/exception str e)
             #f)
    (thunk)))

;; FIXME: PATH is expected to be list of strings, due to requirement in write-eggshell
(define (parse-egg/svnwiki fn-or-port path timestamp)
  (let ((sxml-doc (warn-on-exception "Parse error" (lambda () (parse-svnwiki fn-or-port)))))
    (and sxml-doc
         (begin
           (write-eggshell path sxml-doc timestamp)
           #t))))

(define (parse-man/svnwiki fn-or-port path name timestamp)
  (let ((sxml-doc (warn-on-exception "Parse error" (lambda () (parse-svnwiki fn-or-port)))))
    (and sxml-doc
         (begin
           (write-manshell path name sxml-doc timestamp)
           #t))))

(define eggdoc-svnwiki-available?
  (let ((avail? (delay (condition-case
                        (eval '(begin (use eggdoc-svnwiki eggdoc) #t))
                        ((exn) #f)))))
    (lambda ()
      (force avail?))))

(define (parse-egg/eggdoc fn root path timestamp)
  (unless (eggdoc-svnwiki-available?)
    (##sys#clear-trace-buffer)           ; you are a horrible person
    (error "eggdoc-svnwiki is required but not installed.\nUse `chicken-install eggdoc-svnwiki` to install it."))
  (let ((dir (pathname-directory fn))
        (file (pathname-strip-directory fn)))
    (let ((result
           (condition-case
            (with-cwd dir         ;; Change to eggdoc's basedir; may need local files
                      (lambda ()       ;; with-cwd not visible in eval, so do it outside
                        (let ((doc (with-input-from-file file read-file))  ; c5 read-list expects a port
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
             (let* ((path (or path `(,name)))
                    (path (append root path)))
               (parse-egg/svnwiki (open-input-string text)
                                  path timestamp)))))))

;;; svnwiki egg and man tree parsing

;; Get final egg node path; mainly used because parse-one-egg does not return the final path,
;; and parse-egg-directory needs the final path to print it correctly.
(define (get-egg-path pathname root path)
  (let ((basename (pathname-file pathname)))
    (append root
            (or path
                (if (string=? basename "index")
                    '()
                    `(,basename))))))

;; Argument PATH allows computed path override.  It should be a list of strings.
;; The egg name SHOULD be printed to stdout if it is determined
;; programmatically (i.e. if PATH is #f); that is either done here
;; or in the individual parser if necessary.
;; For svnwiki, does recursive traversal of directories.  This is not supported for eggdoc.
;; Returns #f if the parse fails (currently, if error or not a regular file).
;; Returns 'added if the node was new, 'modified if the node existed but was modified,
;; and 'unchanged if the node was unchanged (based on timestamp comparison).
;; Note: Timestamp comparison cannot be done for eggdoc nodes as we do not
;; know the egg node name until after the document is parsed.
(define (parse-one-egg pathname type root path force?)
  (case type
    ((svnwiki)
     (cond ((regular-file? pathname)
            (let* ((path (get-egg-path pathname root path))
                   (fts (file-modification-time pathname)))
              (let* ((node (handle-exceptions e #f (lookup-node path))) ;; unfortunate API kink
                     (nts (if node (or (node-timestamp node) 0) 0))
                     (ntype (if node (node-type node) 'none)))
                (or (and (not force?)
                         (eq? ntype 'egg)
                         (<= fts nts)
                         'unchanged)
                    (and (parse-egg/svnwiki pathname path fts)
                         (if node 'modified 'added))))))
           ((and (directory? pathname)
                 (file-execute-access? pathname))
            (with-cwd pathname          ;; Possibly trap and warn on directory change failure.
                      (lambda ()
                        (let* ((basename (pathname-file pathname))
                               ;; Note we don't treat "index" specially here (nor can we)
                               ;; so we can't use get-egg-path here.
                               (path (append root
                                             (or path `(,basename)))))
                          (parse-eggdir (current-directory) type path force?)
                          'directory)
                        )))
           (else 'skipped)))
    ((eggdoc)
     (cond ((regular-file? pathname)
            (let ((fts (file-modification-time pathname)))
              ;; Force node to #f, leave code scaffolding in place for future timestamp handling
              (let* ((node #f ;; (handle-exceptions e #f (lookup-node path))
                           )
                     (nts (if node (or (node-timestamp node) 0) 0))
                     (ntype (if node (node-type node) 'none)))
                (or (and (not force?)
                         (eq? ntype 'egg)
                         (<= fts nts)
                         'unchanged)
                    (and (parse-egg/eggdoc pathname root path fts)
                         (if node 'modified 'added))))))
           (else 'skipped)))
    (else
     (error "Invalid egg document type" type))))

;; External interface to single egg parsing from command-line.
;; FIXME: We don't need to read the cache if the egg is unchanged,
;; but we can't check that until we call parse-one-egg.
(define (parse-individual-egg pathname type #!optional path force?)
  (with-global-write-lock
   (lambda ()
     (init-working-id-cache!)
     (let ((rc (parse-one-egg pathname type '() path force?)))
       (cond ((eq? rc 'unchanged)
              (print "no changes"))
             (rc
              (commit-working-id-cache!)))
       rc))))

(define ignore-filename?
  ;; Ignore not just #*# but #* due to issue with r/w invariance on sharp-syntax
  ;; in older Chicken.
  ;; Ignore every extension except none and .wiki, in case we encounter images
  ;; etc. in the wiki.  Any eggname with a dot in it will thus be rejected.
  (let ((re:ignore (irregex "^[#.]|\\.swp$|~$")))
    (lambda (fn)
      (let ((ext (pathname-extension fn)))
        (or
         (and ext
              (not (string=? ext "wiki")))
         (irregex-search re:ignore fn))))))

;; Internal interface.
(define (parse-eggdir dir type root #!optional force?)
  (let ((egg-count 0) (updated 0) (errors 0))
    (case type
      ((svnwiki)
       (for-each (lambda (name)
                   (let* ((pathname (make-pathname dir name))
                          ;; (We could move name portion from pathname to path arg.)
                          (code (parse-one-egg pathname type root #f force?))
                          (path (get-egg-path pathname root #f)))
                     (unless (or (eq? code 'skipped)
                                 (eq? code 'directory))
                       (set! egg-count (+ egg-count 1)))
                     ;; Must print ONLY after successful parse, otherwise
                     ;; directories etc. will show up.  Any parse warnings
                     ;; will occur before the name appears.
                     (let ((spath (string-intersperse path " ")))
                       (case code
                         ((added)
                          (set! updated (+ updated 1))
                          (print "A " spath))
                         ((modified)
                          (set! updated (+ updated 1))
                          (print "M " spath))
                         ((unchanged))
                         ((directory))
                         ((skipped))   ;; returned on non-regular file (including links)
                         ((unknown))   ;; not currently used for eggs
                         ((#f)         ;; returned on fatal parse error
                          (set! errors (+ errors 1))
                          (print "E " spath))
                         (else
                          (error "unknown parse return code" code))))))
                 (remove ignore-filename? (directory dir))))
      ((eggdoc)
       (print "Gathering egg information...")
       (let ((re:dir (irregex `(: bos ,(make-pathname dir ""))))) ; strip off dir name
         (for-each (lambda (pathname)
                     (let ((pretty-path
                            (string-substitute re:dir "" pathname)))
                       (display pretty-path) (display " -> ") (flush-output)
                       (let ((code (parse-one-egg pathname type root #f force?))) ; eggname printed in parse-egg/eggdoc
                         (unless (eq? code 'skipped)
                          (set! egg-count (+ egg-count 1)))
                         (case code
                           ((added modified)
                            (set! updated (+ updated 1)))
                           ((unchanged))
                           ((skipped))
                           ((#f)
                            (set! errors (+ errors 1)))
                           (else
                            (error "unknown parse return code" code))))))
                   (gather-eggdoc-pathnames dir))))
      (else
       (error "Invalid egg directory type" type)))

    ;; We should move this up a level, to allow 1) skip cache commit if all unchanged,
    ;; 2) gathering of all egg totals including subdirs.
    
    (display "; ")
    (when (pair? root)
      (printf "~a :: " (string-intersperse root " ")))
    (printf "~a eggs processed, ~a updated~a\n" egg-count updated
            (if (> errors 0)
                (sprintf ", ~a errors" errors)
                ""))))

;; Public (toplevel) command interface.
(define (parse-egg-directory dir type root #!optional force?)
  (with-global-write-lock
   (lambda ()
     (init-working-id-cache!)
     (parse-eggdir dir type root force?)
     (commit-working-id-cache!))))

;; Return list of eggdoc pathnames gathered from egg metadata in local
;; repository DIR.  Latest tagged version (failing that, trunk) is used.
;; Egg name is discarded--caller must gather it from the (name) elt in the doc.
(define (gather-eggdoc-pathnames dir)
  (cond-expand
   (chicken-4
    (filter-map
     (lambda (egg)
       (let ((x (alist-ref 'eggdoc (cdr egg))))
         (and x
              (pair? x)    ; the occasional egg may just have (eggdoc)
              (let* ((egg-name (->string (car egg)))
                     (filename (car x))
                     (pathname (locate-egg/local egg-name dir)))
                (make-pathname pathname filename)))))
     (gather-egg-information dir)))
   (else
    (error 'gather-eggdoc-pathnames "eggdoc source is not supported on this version of Chicken"))))

(define (parse-one-man pathname type path force?)
  (case type
    ((svnwiki)
     (let ((name (pathname-file pathname)))
       (let ((path (or path (man-filename->path name))))
         (cond ((symbolic-link? pathname) 'skipped)
               ((and (regular-file? pathname)
                     path)
                (let* ((fts (file-modification-time pathname))
                       (node (handle-exceptions e #f (lookup-node path)))
                       (nts (if node (or (node-timestamp node) 0) 0))
                       (ntype (if node (node-type node) 'none)))
                  (or (and (not force?)
                           (eq? ntype 'unit) ;; FIXME unit?  what an odd design decision
                           (<= fts nts)
                           'unchanged)
                      (and (parse-man/svnwiki pathname path name fts)
                           (if node 'modified 'added)))))
               ((not path) 'unknown)
               (else 'skipped)))))
    (else
     (error "Invalid man document type" type))))

(define (parse-individual-man pathname type #!optional path force?)
  (with-global-write-lock
   (lambda ()
     (init-working-id-cache!)
     (let ((rc (parse-one-man pathname type path force?)))
       (cond ((eq? rc 'unchanged)
              (print "no changes"))
             (rc
              (commit-working-id-cache!)))
       rc))))

;; Design goal is for man pages for both Chicken 4 and 5 to work without having to
;; distinguish version at the command line (or ideally, even heuristically).
;; So we make the following assumptions:
;;   - Pages that kept their old names also keep their old node paths.
;;   - Old node paths in the virtual (chicken) namespace do not conflict with
;;     new, real modules in (chicken).  Example: 'read-syntax', 'type',
;;   - (chicken) "The User's Manual" has a replacement that is module-oriented,
;;     or does not exist in the 5.
;;   - Modules like (chicken process signal) do not have a "signal" defsig in
;;     (chicken process), which would cause a conflict.
(define man-filename->path
  (let ((re:unit (irregex "^Unit (.+)"))
        (re:module-path (irregex "^Module (\\([^)]+\\))$"))
        (re:module (irregex "^Module (.+)$"))
        (symbolify-list (lambda (x) (and x (map (lambda (x)
                                             (if (symbol? x) x (string->symbol x)))
                                           x)))))
    (lambda (t)
      (symbolify-list
       ;; chicken 5 specific
       (cond ((string-search re:module-path t) =>
              (lambda (search-result)
                (let ((path-string (cadr search-result)))
                  (condition-case
                   (map (lambda (x) (->string x))
                        (with-input-from-string path-string read))
                   (exn (exn)
                        ;; Consider letting the caller handle this.
                        (print "Error parsing man filename: "
                               ((condition-property-accessor 'exn 'location) exn)
                               ": "
                               ((condition-property-accessor 'exn 'message) exn)
                               ": "
                               ((condition-property-accessor 'exn 'arguments) exn))
                        #f)))))
             ((string-search re:module t) => cdr) ; Module r5rs -> ("r5rs")
             ((string=? t "Debugging")
              '(chicken debugging))
             ((string=? t "Extension tools")
              '(chicken eggs tools))
             ((string=? t "Egg specification format")
              '(chicken eggs format))
             ;; "Included modules" is the C5 pointer to the modules list.
             ;; (chicken modules included) may not be the best place to put this,
             ;; but it doesn't pollute the namespace any further.
             ((string=? t "Included modules")
              '(chicken modules included))
             ((string=? t "Units and linking model")
              ;; Iffy naming. This page should probably not take up valuable namespace.
              '(chicken linking))

             ;; common between C4 and C5
             ((string=? t "Interface to external functions and variables")
              '(foreign))
             ((string=? t "Accessing external objects")
              '(foreign access))  ; Exists in C5, but is for examples.
             ((string=? t "C interface")
              '(foreign c-interface))
             ((string=? t "Embedding")
              '(foreign embedding))
             ((string=? t "Foreign type specifiers")
              '(foreign types))
             ((string=? t "Declarations")
              '(chicken declarations))    ; Might be more approriate in (csc declarations).
             ((string=? t "Extensions to the standard")
              '(chicken standard-extensions))
             ((string=? t "Modules")
              '(chicken modules))
             ;; Note: (chicken type) is the C5 module and its stub page points to
             ;; (chicken types) for documentation.
             ((string=? t "Types")
              '(chicken types))
             ((string=? t "Using the interpreter")
              '(csi))
             ((string=? t "Using the compiler")
              '(csc))

             ;; These are general reading pages which do not
             ;; contain identifiers.  They're in (chicken)
             ;; right now even though they don't really reside
             ;; in that namespace; in the C5 module system there is
             ;; a higher chance of conflict than in C4.
             ((string=? t "The User's Manual")
              '(chicken))       ; Chicken root node; no conflict since no (chicken) module.
             ((string=? t "Extensions")
              '(chicken eggs))
             ((string=? t "Acknowledgements")
              '(chicken acknowledgements))
             ((string=? t "Bibliography")
              '(chicken bibliography))
             ((string=? t "Bugs and limitations")
              '(chicken bugs))
             ((string=? t "Cross development")
              '(chicken cross-development))
             ((string=? t "Data representation")
              '(chicken data-representation))
             ((string=? t "Deployment")
              '(chicken deployment))
             ((string=? t "Deviations from the standard")
              '(chicken standard-deviations))
             ((string=? t "Getting started")
              '(chicken intro))
             
             ;; C4 specific
             
             ((string-search re:unit t) => cdr) ; Unit tcp -> ("tcp")
             ;; A few of these were moved to sections under other topics in C5,
             ;; so any links to these left in the C5 manual will not resolve.
             ((string=? t "Callbacks")
              '(foreign callbacks))      ; Moved to (chicken foreign) section
             ((string=? t "Locations")
              '(foreign locations))      ; Moved to (chicken foreign) section
             ((string=? t "Other support procedures") 
              '(foreign support))        ; Moved to (chicken process-context) in C5.
             ((string=? t "Parameters")
              '(chicken parameters))     ; Moved to (chicken base) section in C5.
             ((string=? t "The R5RS standard")
              '(scheme))                 ; Handled by `Module scheme` in C5.
             ((string=? t "Non-standard read syntax")
              ;; Moved to (chicken standard-extensions) in C5; thus the
              ;; (chicken read-syntax) C5 module does not conflict.
              '(chicken read-syntax))
             ((string-ci=? t "faq")  ; use ci compare because links to 'faq' and 'FAQ' both exist
              '(chicken faq))        ; FAQ removed in C5.
             ((string=? t "Exceptions")
              '(chicken exceptions))    ; Moved to (chicken condition) in C5
             ((string=? t "Macros")
              '(chicken macros))     ; Moved to (chicken syntax) in C5
             ((string=? t "Non-standard macros and special forms")
              '(chicken special-forms))        ; Moved elsewhere, e.g. (chicken base) in C5.
             ((string=? t "Basic mode of operation")
              '(chicken basic-operation))   ; Removed in C5
             ((string=? t "Supported language")
              '(chicken language))    ; Removed in C5; moved to User's Manual and Included Modules.

             ;; very legacy

             ((string=? t "Modules and macros")
              '(chicken modules))
             
             (else #f))))))

(define (parse-man-directory dir type #!optional force?)
  (let ((egg-count 0) (updated 0) (errors 0))
    (with-global-write-lock
     (lambda ()
       (init-working-id-cache!)
       (case type
         ((svnwiki)
          (for-each (lambda (name)
                      (let ((code (parse-one-man (make-pathname dir name) 'svnwiki #f force?)))
                        (unless (eq? code 'skipped)
                          (set! egg-count (+ egg-count 1)))
                        (case code
                          ((added)
                           (set! updated (+ updated 1))
                           (print "A " name))
                          ((modified)
                           (set! updated (+ updated 1))
                           (print "M " name))
                          ((unchanged))
                          ((skipped))   ;; don't be verbose
                          ((#f)
                           (set! errors (+ errors 1))
                           (print "E " name))
                          ((unknown)
                           (print "? " name))       ;; not an error; just a man with no path
                          (else
                           (error "unknown parse return code" code)))))
                    (sort (remove ignore-filename? (directory dir))
                          string<?)))
         (else
          (error "Invalid man directory type" type)))
       (commit-working-id-cache!)
       (printf "; ~a man pages processed, ~a updated~a\n" egg-count updated
               (if (> errors 0)
                   (sprintf ", ~a errors" errors)
                   ""))))))

;; If names is null, look for all .wiki docs in the repository.
;; If names are explicitly provided, look for matching .wiki docs (and record an error
;; if not present).  Note: on Chicken 5, looks in the installation repository, not all
;; repositories, just like chicken-status and chicken-install behave.
;; 
;; If target?, look for docs in the target repository. This will only
;; matter for cross-compilers; for regular compilers, target repo == host repo.
;; Note: on Chicken 5, the standard target lib repository is searched, but not the "runlib".
;; It is unclear if this matters.
(define (parse-installed-eggs names type #!optional force? target?)
  ;; ignored: names
  ;; From chicken-status.scm.  There needs to be an official API for this stuff.
  ;;
  ;; In C5, (destination-repository 'target) covers this and the "runlib" directory,
  ;; but it is not exported from egg-environment.scm.

  (define-foreign-variable C_TARGET_LIB_HOME c-string)
  (define-foreign-variable C_BINARY_VERSION int)
  (define (repo-path)
    (if target?
        (make-pathname C_TARGET_LIB_HOME (sprintf "chicken/~a" C_BINARY_VERSION))
        (cond-expand
         (chicken-4 (repository-path))
         (else      (installation-repository)))))
  (define (glob-wiki-docs)
    ;; shortcut: just look for *.wiki, instead of *.setup-info -> *.wiki
    (glob (make-pathname (repo-path) "*" "wiki")))
  (define (wiki-doc-filenames egg-names)
    (let ((repo (repo-path)))
      (map (lambda (name)
             (make-pathname repo name "wiki"))
           egg-names)))

  (let ((egg-count 0) (updated 0) (errors 0))
    (with-global-write-lock
     (lambda ()
       (init-working-id-cache!)
       (case type
         ((svnwiki)
          (for-each (lambda (fn)
                      (let ((code (parse-one-egg fn type '() #f force?))
                            (name (pathname-file fn)))
                        (set! egg-count (+ egg-count 1))
                        (case code
                          ((added)
                           (set! updated (+ updated 1))
                           (print "A " name))
                          ((modified)
                           (set! updated (+ updated 1))
                           (print "M " name))
                          ((unchanged))
                          ((skipped unknown)
                           (set! errors (+ errors 1))   ;; here, skipping is considered an error, as all *.wiki should be regular files
                           (print "? " name))
                          ((#f)
                           (set! errors (+ errors 1))
                           (print "E " name)))))
                    (if (pair? names)
                        (wiki-doc-filenames names)
                        (glob-wiki-docs))))
         (else
          (error "Invalid installed doc type" type)))
       (commit-working-id-cache!)
       (printf "; ~a eggs processed, ~a updated~a\n" egg-count updated
               (if (> errors 0)
                   (sprintf ", ~a errors" errors)
                   ""))))
    (= errors 0)))

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
(define (empty-working-id-cache!)
  (working-id-cache (make-id-cache-table)))
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
            (let ((new (remove (lambda (x) (equal? x parent)) old)))
              (if (null? new)
                  (hash-table-delete! ht id)
                  (hash-table-set! ht id new))))))))
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
      (cond-expand
       (chicken-4 (rename-file tmp-fn fn))
       (else      (rename-file tmp-fn fn 'clobber)))  ;; odd design decision
      (make-id-cache
       ht
       (current-seconds)    ;; (file-modification-time (id-cache-filename))
       (id-cache-filename c))))
  (let* ((r (current-repository))
         (c (repository-id-cache r)))
    (set-repository-id-cache!
     r (write-id-cache c ht))))

(define (make-id-cache-table)
  (make-hash-table eq?))

(define (refresh-id-cache)   ;; rebuild entire ID cache from scratch
  (with-global-write-lock
   (lambda ()
     (empty-working-id-cache!)
     (let refresh-node ((n (lookup-node '())))
       (for-each (lambda (c)
                   (unless (node-definition-id? n (node-id c))   ;; dumb
                     (refresh-node c)))
                 (node-children n))
       (for-each (lambda (defid)
                   (working-id-cache-add! (append (node-path n)
                                                  (list defid))))
                 (node-definition-ids n))
       (unless (null? (node-path n))   ;; skip adding root node
         (working-id-cache-add! (node-path n))))
     (commit-working-id-cache!))))

)  ;; end module
