;; +----------------------------------------------------+
;; |  Filters double-spaced lines as single spaced.     |
;; |                                                    |
;; |  Usage:                                            |
;; |  $ undouble < file                                 |
;; |                                                    |
;; |  Armando Blancas                          Jun '12  |
;; +----------------------------------------------------+

(include "plylib/ply-io.scm")

(let* ((text (:slurp-port (current-input-port)))
       (repl (:replace-all "\n\n" "\n" text)))
  (:spit-port (current-output-port) repl))
