;; +-------------------------------------------------------------+
;; | hits - Counts the hits in a Google search.                  |
;; |                                                             |
;; | Usage:                                                      |
;; | $ hits word1 word2 ...                                      |
;; +-------------------------------------------------------------+

(include "plylib/ply-io.scm")

(define pattern "About [\\d,]+")
(define server  "www.google.com/search?hl=en&q=")

(define (main args)
  (let* ((url (:str server (:str-join "+" args)))
         (lst (:extract pattern (:get-url url))))
    (:unless (null? lst)
      (println (car lst)))))

(main (cdr (command-line)))
