;; +-------------------------------------------------------------+
;; | popclock - Reports the US and World population.             |
;; |                                                             |
;; | Usage:                                                      |
;; | $ popclock                                                  |
;; +-------------------------------------------------------------+

(include "plylib/ply-sxml.scm")

(define pop-clocks "http://www.census.gov/main/www/rss/popclocks.xml")

(let* ((text (:get-url pop-clocks))
       (sxml (:str->sxml text)))
  (println (:at sxml 1 2 3 1))
  (println (:at sxml 1 2 7 1 1))
  (println (:at sxml 1 2 8 1 1)))
