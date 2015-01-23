;; +--------------------------------------------------+
;; |  Rewrites text files with localized line breaks  |
;; |  and optionally replacing spaces for tabs.       |
;; |                                                  |
;; |  Usage:                                          |
;; |  $ fixws [-t [n]] [-h] [-r] file1 [file2 ...]    |
;; |                                                  |
;; |  Armando Blancas                        May '12  |
;; +--------------------------------------------------+

(include "plylib/ply-io.scm")

(define default-spaces 4)
(define spaces-per-tab 0)
(define file-spec "")

(define (fix-ws fname)
  (let ((text (:slurp fname))
        (newl (:nl)))
    (call-with-output-file fname
      (lambda (port)
        (println fname)
          (if (zero? spaces-per-tab)
            (:dolist (line (:lines text))
	      (print port: port line newl))
            (let ((spaces (:repeat spaces-per-tab #\space)))
              (:dolist (line (:lines text))
	        (print port: port (:replace-allstr "\t" spaces line) newl))
              (print port: port newl)))))))

(define (fix-file f)
  (if (and (:file? f) (:writable? f))
    (fix-ws f)
    (cond ((:directory? f)  (println "Directory: " f))
          ((not (:file? f)) (println "Not found: " f))
          ((:read-only? f)  (println "Read-only: " f))
	  (else             (println "Failed on: " f)))))

(define (scan args)
  (cond ((null? args)
	 (quote ()))
        ((equal? (car args) "-t")
	 (if (null? (cdr args))
	   (quote ())
	   (let ((n (string->number (cadr args))))
	     (if (number? n)
	       (begin
	         (set! spaces-per-tab n)
		 (scan (cddr args)))
	       (begin
	         (set! spaces-per-tab default-spaces)
	         (scan (cdr args)))))))
	((equal? (car args) "-r")
	 (if (not (:single? args))
	   (begin
	     (set! file-spec (cadr args))
	     (cdr args))
	   (quote ())))
	((equal? (car args) "-h")
         (quote ()))
	(else args)))

(define (main args)
  (let ((lst (scan args)))
    (if (not (null? lst))
      (if (not (:empty? file-spec))
        (:doftree (f file-spec)
          (fix-file f))
        (:dolist (f lst) 
          (fix-file f)))
      (begin
        (println "fixws -- Fixes whitespace: strips ^M and puts spaces for tabs.")
        (println "Usage:   fixws [-t [n]] [-h] [-r] file1 [file2...]")
        (println "         -t n   Puts n spaces per tab; 4 if no number is given.")
        (println "         -r     Takes a filespec in double quotes with wildcards;")
        (println "                Fixes files recursing in subdirectories.")))))

(main (cdr (command-line)))
