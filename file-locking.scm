(cond-expand
 (chicken-4) ; Module not needed nor expected for Chicken 4.
 (else
  
(module chicken-doc-file-locking (file-lock/blocking)

;; Implement fix #1565 for Chicken 5.0.0 in posixunix.scm:posix#file-lock/blocking (core rev 7729e51103).
;; This is a terrible hack, but supported releases of Chicken are rare (5.0.1 is a development release),
;; so we cannot remove it until at least 5.1.0 or a 5.0.0.x stability release.

(import scheme
        (chicken base)
        (chicken foreign)
        (chicken platform)
        (chicken fixnum)
        (prefix (chicken file posix) "posix:"))

(define file-lock/blocking)

(cond-expand
  (unix

;;; Copied from posixunix.scm (Record locking:)

#>
#include <fcntl.h>
static C_TLS struct flock C_flock;
#define C_flock_setup(t, s, n) (C_flock.l_type = C_unfix(t), C_flock.l_start = C_num_to_int(s), C_flock.l_whence = SEEK_SET, C_flock.l_len = C_num_to_int(n), C_SCHEME_UNDEFINED)
#define C_flock_lockw(p)    C_fix(fcntl(fileno(C_port_file(p)), F_SETLKW, &C_flock))
<#

(define-foreign-variable _errno int "errno")
(define-foreign-variable _eintr int "EINTR")
(define-foreign-variable _f_wrlck int "F_WRLCK")
(define-foreign-variable _f_rdlck int "F_RDLCK")
(define posix-error ##sys#posix-error)

(if (not (equal? (chicken-version) "5.0.0"))
    (set! file-lock/blocking posix:file-lock/blocking)
    (let ()
      (define (setup port args loc)
        (let-optionals* args ([start 0]
                              [len #t] )
                        (##sys#check-open-port port loc)
                        (##sys#check-exact-integer start loc)
                        (if (eq? #t len)
                            (set! len 0)
                            (##sys#check-exact-integer len loc) )
                        (##core#inline "C_flock_setup" (if (= (##sys#slot port 1) 1) _f_rdlck _f_wrlck) start len)
                        (##sys#make-structure 'lock port start len) ) )
      (define (err msg lock loc)
        (posix-error #:file-error loc msg (##sys#slot lock 1) (##sys#slot lock 2) (##sys#slot lock 3)) )
      (set! file-lock/blocking
            (lambda (port . args)
              (let loop ()
	        (let ((lock (setup port args 'file-lock/blocking)))
	          (if (fx< (##core#inline "C_flock_lockw" port) 0)
	              (cond
		       ((fx= _errno _eintr) (##sys#dispatch-interrupt loop))
		       (else (err "cannot lock file" lock 'file-lock/blocking)))
	              lock)))))))


   )  ; platform-unix
  (else
   (set! file-lock/blocking posix:file-lock/blocking)))   ; actually unimplemented on windows, but that's ok

)

  
))
