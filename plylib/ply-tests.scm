;; +---------------------------------------------------------------------+
;; |  UNIT TESTS FOR GAMBIT PLY                                          |
;; |  A high-productivity function library for Gambit-C.                 |
;; |                                                                     |
;; |  Copyright 2012 Armando Blancas                                     |
;; +---------------------------------------------------------------------+

;; id

(:make-test id-1 "The identity function"
  (:eqv? (:id 5) 5))

(:make-test id-2 "The identity function"
  (let ((s "now is the time"))
    (:eqv? (:id s) s)))

(:make-test id-3 "The identity function"
  (let ((lst (:range 10)))
    (:eqv? (:id lst) lst)))

(:make-test id-4 "The identity function"
  (:not-equal? (:id 5) 6 "Negative test"))

;; !=

(:make-test not=-1 "Not equal"
  (:true? (!= 3 4)))

(:make-test not=-2 "Not equal"
  (:true? (!= 3 4 5 6)))

(:make-test not=-3 "Not equal"
  (:true? (!= 8 8 8 8 9)))

(:make-test not=-4 "Not equal"
  (:false? (!= 3 3 3) "Negative test"))

;; complement

(:make-test comp-1 "Complement function"
  (let ((is-odd? (:complement even?)))
    (:true? (is-odd? 3))))

(:make-test comp-2 "Complement function"
  (let* ((between (lambda (n x y) (and (> n x) (< n y))))
         (outside (:complement between)))
    (:true? (between 5 0 10))
    (:true? (outside 5 10 20))))

(:make-test comp-3 "Complement function"
  (let* ((between (lambda (n x y) (and (> n x) (< n y))))
         (outside (:complement between)))
    (:false? (outside 5 0 20) "Negative test")))

;; flip

(:make-test flip-1 "Flip arguments"
  (let ((rdiv (:flip quotient)))
    (:equal? (rdiv 2 4) 2)))

(:make-test flip-2 "Flip arguments"
  (:equal? ((:flip cons) '(xyz) 'abc) '(abc xyz)))

;; partial

(:make-test part-1 "Partial evaluation"
  (let ((add-5 (:partial + 5)))
    (:eqv? (add-5 3) 8)))

(:make-test part-2 "Partial evaluation"
  (let ((find-now (:partial :search "now")))
    (:eqv? (find-now "now is the time") 0)
    (:eqv? (find-now "the time is now") 12)))

;; inc

(:make-test inc-1 "Increment number"
  (:eqv? (:inc -1)   0)
  (:eqv? (:inc  0)   1)
  (:eqv? (:inc 99) 100))

(:make-test inc-2 "Increment number"
  (let ((n 5005) (m 101))
    (:eqv? (:inc n) 5006)
    (:eqv? (:inc (:inc m)) 103)))

;; dec

(:make-test dec-1 "Decrement number"
  (:eqv? (:dec   0) -1)
  (:eqv? (:dec   1)  0)
  (:eqv? (:dec 100) 99))

(:make-test dec-2 "Decrement number"
  (let ((n 5005) (m 101))
    (:eqv? (:dec n) 5004)
    (:eqv? (:dec (:dec m)) 99)))
