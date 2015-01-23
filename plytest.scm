;; +---------------------------------------------------------------------+
;; |  GAMBIT PLY                                                         |
;; |  A high-productivity function library for Gambit-C.                 |
;; |                                                                     |
;; |  Copyright 2012 Armando Blancas                                     |
;; +---------------------------------------------------------------------+

(include "plylib/ply-io.scm")

(define (show-usage)
  (println "Runs Ply tests from scheme files.")
  (println "Usage: $ plytest file ..."))

(define (main args)
  (:when (null? args)
    (show-usage))
  (:dolist (file args)
    (if (file-exists? file)
      (:run-tests file)
      (println "Not found: " file))))

(main (cdr (command-line)))
