;; +---------------------------------------------------------------------+
;; |  GAMBIT PLY                                                         |
;; |  A high-productivity function library for Gambit-C.                 |
;; |                                                                     |
;; |  Copyright 2012 Armando Blancas                                     |
;; +---------------------------------------------------------------------+

;; Compilation settings.

(declare (standard-bindings) 
         (extended-bindings) 
         (block) 
         (fixnum) 
         (not safe)) 

;; Error messages.

(define E_NOT_AGGREGATE "Value is not string, list, vector, or pair.")
(define E_EMPTY_LIST    "The list must not be empty.")
(define E_EMPTY_VECTOR  "The vector must not be empty.")
(define E_WRONG_ARITY   "Wrong number of arguments.")
(define E_OUT_OF_RANGE  "Number out of range.")


;; +---------------------------------------------------------------------+
;; |                                                                     |
;; |                          BASIC FUNCTIONS                            |
;; |                                                                     |
;; +---------------------------------------------------------------------+

;; Defines a variable at expansion time for use in macros.
(define-macro (at-expand-time expr)
  (eval expr) expr)

 
;; The identity function.
(define (:id x) x)


;; Not equal.
(define (!= . args)
  (not (apply = args)))


;; Complement of a predicate fn.
(define (:complement fn)
  (lambda args
    (not (apply fn args))))


;; Flips the argument of a binary operator.
(define (:flip fn)
  (lambda (x y) (fn y x)))


;; Partial function application.
(define (:partial fn . args) 
  (lambda lst (apply fn (append args lst))))


;; Returns n + 1.
(at-expand-time
(define (:inc n) (+ n 1))
)

;; Returns n - 1.
(at-expand-time
(define (:dec n) (- n 1))
)

;; Tests for a singleton list.
(at-expand-time
(define (:single? lst)
  (and (not (null? lst)) (null? (cdr lst))))
)

;; Returns #\newline as a string.
(define (:nl) (string #\newline))


;; Strings the arguments.
(at-expand-time
(define (:str . args)
  (define (val->str v)
    (with-output-to-string '() (lambda () (display v))))
  (let ((lst (:flatten args)))
    (if (null? lst) "" (append-strings (map val->str lst)))))
)

;; Strings the arguments separated by sp. 
(define (:str-join sp lst)
  (:str (:interpose sp lst)))


;; Test if the string is empty.
(define (:empty? str)
  (zero? (string-length str)))


;; Creates a memoized version of f.
(define (:memoize f)
  (let ((table (make-table))
        (miss (make-table)))
    (lambda args
      (let ((ref (table-ref table args miss)))
        (if (not (eqv? ref miss))
          ref
          (let ((value (apply f args)))
            (table-set! table args value)
            value))))))


;; +---------------------------------------------------------------------+
;; |                                                                     |
;; |                          BASIC MACROS                               |
;; |                                                                     |
;; +---------------------------------------------------------------------+


;; Ignores the given forms. Use only at the top level.
(define-macro (:comment . forms) (void))


;; Expands a macro call.
(define-macro (:ppm form)
  `(pp (lambda () ,form (void))))


;; Sets x up by 1.
(define-macro (:inc! x) 
  `(set! ,x (:inc ,x)))


;; Sets x down by 1.
(define-macro (:dec! x)
  `(set! ,x (:dec ,x)))


;; Swaps the value of two variables.
(define-macro (:swap! x y)
  (let ((tmp (gensym)))
    `(let ((,tmp ,x))
       (set! ,x ,y)
       (set! ,y ,tmp))))


;; Like if, but only the true consequent and multiple forms.
(define-macro (:when test . body)
  `(if ,test (begin ,@body) (void)))


;; The opposite of when.
(define-macro (:unless test . body)
  `(if ,test (void) (begin ,@body)))


;; Evaluates a block for each element of a list.
(define-macro (:dolist bind . body)
  `(for-each
     (lambda (,(car bind)) ,@body)
     ,(cadr bind)))

;; Evaluates a block multiple times for side effects.
(define-macro (:dotimes bind . body)
  (let ((symbol (car bind))
        (limit (cadr bind)))
    `(:dolist (,symbol (:range ,limit)) ,@body)))


;; Evaluates a block while a condition holds true.
(define-macro (:while test . body)
  `(let loop ()
     (:when ,test
       ,@body 
       (loop))))


;; Threads previous values through forms.
(define-macro (-> x . forms)
  (cond ((null? forms) 
           x)
        ((:single? forms) 
           (if (pair? (car forms))
             `(,(caar forms) ,x ,@(cdar forms))
             (list (car forms) x)))
        (else 
           `(-> (-> ,x ,(car forms)) ,@(cdr forms)))))


;; Threads previous values through forms.
(define-macro (->> x . forms)
  (cond ((null? forms) 
           x)
        ((:single? forms) 
           (if (pair? (car forms))
             `(,(caar forms) ,@(cdar forms) ,x)
             (list (car forms) x)))
        (else 
           `(->> (->> ,x ,(car forms)) ,@(cdr forms)))))


;; +---------------------------------------------------------------------+
;; |                                                                     |
;; |                              LISTS                                  |
;; |                                                                     |
;; +---------------------------------------------------------------------+


;; Returns the nth element in obj.
(define (:nth obj n)
  (cond ((string? obj) (string-ref obj n))
        ((list? obj)   (list-ref obj n))
        ((vector? obj) (vector-ref n))
        ((pair? obj)   (if (zero? n) (car obj) (cdr obj)))
        (else          (error "(:nth)" E_NOT_AGGREGATE obj))))


;; Returns a nested element using a list of indexes.
(define (:at obj n . args)
  (let ((elem (:nth obj n)))
    (if (or (not (pair? elem)) (null? args))
      elem
      (apply :at elem args))))


;; Counts the elements in obj.
(define (:count obj)
  (cond ((string? obj) (string-length obj))
        ((list? obj)   (length obj))
        ((vector? obj) (vector-length obj))
        ((pair? obj)   2)
        (else          (error "(:count)" E_NOT_AGGREGATE obj))))


;; Like cons, but adds an element at the end.
(define (:snoc lst x)
  (if (null? lst)
    (cons x lst)
    (cons (car lst) (:snoc (cdr lst) x))))


;; Concatenates (appends) the results of map.
(define (:mapcat fn arg . rest)
  (apply append (map fn (cons arg rest))))


;; Left fold.
(define (:foldl fn acc lst)
  (if (null? lst)
      acc
      (:foldl fn (fn acc (car lst)) (cdr lst))))


;; Left fold one.
(define (:foldl1 fn lst)
  (if (null? lst)
    (error "(:foldl1)" E_EMPTY_LIST)
    (:foldl fn (car lst) (cdr lst))))


;; Right fold.
(define (:foldr fn acc lst)
  (if (null? lst)
      acc
      (fn (car lst) (:foldr fn acc (cdr lst)))))


;; Right fold one.
(define (:foldr1 fn lst)
  (if (null? lst)
    (error "(:foldr1)" E_EMPTY_LIST)
    (letrec ((F (lambda (l)
                  (if (null? (cdr l)) 
                    (car l)
                    (fn (car l) (F (cdr l)))))))
      (F lst))))


;; Right fold A.K.A. fold or reduce.
(define :fold   :foldr)
(define :reduce :foldr)


;; Unfold.
(define (:unfold fn init pred)
  (if (pred init)
      (cons init '())
      (cons init (:unfold fn (fn init) pred))))


;; Function composition operator.
(define (:compose . fns)
  (define (C f g)
    (lambda args
      (call-with-values (lambda () (apply g args)) f)))
  (:foldr1 C fns))


;; Repeats n times the object x.
(define (:repeat n x)
  (if (char? x) 
    (make-string n x)
    (letrec ((R (lambda (n) 
                  (if (positive? n) 
                    (cons x (R (:dec n)))
                    (quote ())))))
      (R n))))


;; List accessor shortcuts.
(define :1st car)
(define :2nd cadr)
(define :3rd caddr)
(define :4th cadddr)
(define :5th (:compose car    cddddr))
(define :6th (:compose cadr   cddddr))
(define :7th (:compose caddr  cddddr))
(define :8th (:compose cadddr cddddr))


;; Tests if val is a member of lst.
(define (:member? val lst)
  (:foldl (lambda (x y) (or x (eqv? y val))) #f lst))


;; Generates a list of numbers.
(define (:range . args)
  (case (:count args)
    ((0) (quote ()))
    ((1) (:range 0 (:1st args) 1))
    ((2) (:range (:1st args) (:2nd args) 1))
    ((3) (let* ((low  (:1st args))
                (high (:2nd args))
                (step (:3rd args))
                (comp (if (positive? step) <= >=))
                (pred (lambda (n) (comp high (+ n step)))))
         (if (pred low)
           (quote ())
           (:unfold (:partial + step) low pred))))
    (else (error "(:range)" E_WRONG_ARITY))))


;; Selects elements from a list.
(define (:filter pred lst)
  (let ((F (lambda (x y) (if (pred x) (cons x y) y))))
    (:reduce F '() lst)))


;; Removes elements from a list.
(define (:remove pred lst)
  (:filter (:complement pred) lst))


;; Selects the first n elements from a list.
(define (:take n lst)
  (if (or (zero? n) (null? lst))
    (quote ())
    (cons (car lst) (:take (:dec n) (cdr lst)))))


;; Selects elements from a list.
(define (:take-while pred lst)
  (cond ((null? lst)      (quote ()))
        ((pred (car lst)) (cons (car lst) (:take-while pred (cdr lst))))
        (else             (quote ()))))


;; Selects the last n elements from a list.
(define (:take-right n lst)
  (let loop ((lag lst) (lead (:drop n lst)))
    (if (pair? lead)
      (loop (cdr lag) (cdr lead))
      lag)))


;; Removes the first n elements from a list.
(define (:drop n lst)
  (if (or (zero? n) (null? lst))
    lst
    (:drop (:dec n) (cdr lst))))


;; Removes elements from a list.
(define (:drop-while pred lst)
  (cond ((null? lst)      (quote ()))
        ((pred (car lst)) (:drop-while pred (cdr lst)))
        (else             lst)))


;; Removes the last n elements from a list.
(define (:drop-right n lst)
  (let loop ((lag lst) (lead (:drop n lst)))
    (if (pair? lead)
      (cons (car lag) (loop (cdr lag) (cdr lead)))
      (quote ()))))


;; Creates a list from values and a new list.
(define (:list* . args)
  (:reduce cons (:take-right 1 args) (:drop-right 1 args)))


;; Returns the last element in lst.
(define (:last lst)
  (:take-right 1 lst))


;; Returns a copy of lst with the last element removed.
(define (:butlast lst)
  (:drop-right 1 lst))


;; Tests if all elements in lst satisfy the predicate pred.
(define (:all? pred lst)
  (:foldl (lambda (x y) (and x (pred y))) #t lst))


;; Tests if any element in lst satisfies the predicate pred.
(define (:any? pred lst)
  (:foldl (lambda (x y) (or x (pred y))) #f lst))


;; Interleaves the elements of lists.
(define (:interleave . args)
  (if (:any? null? args)
    (quote ())
    (append (map car args) (apply :interleave (map cdr args)))))


;; Interposes a separator between the elements of a list.
(define (:interpose sep lst)
  (cond ((null? lst) (quote ()))
        ((:single? lst) lst)
        (else
          (letrec ((I (lambda (l)
                        (if (null? (cdr l))
                            l
                            (cons (car l) (cons sep (I (cdr l))))))))
            (I lst)))))


;; Flattens a list with nested lists.
(at-expand-time
(define (:flatten lst)
  (cond ((null? lst) lst)
        ((null? (car lst)) (:flatten (cdr lst)))
        ((pair? (car lst)) (append (:flatten (car lst)) (:flatten (cdr lst))))
        (else (cons (car lst) (:flatten (cdr lst))))))
)

;; Partitions a list into sublists.
(define (:partition n step lst)
  (:unless (positive? step)
    (error "(:partition) argument step" E_OUT_OF_RANGE))
  (letrec ((P (lambda (l)
                (if (null? l)
                  (quote ())
                  (let ((p (:take n l)))
                    (if (= n (:count p))
                      (cons (:take n l) (P (:drop step l)))
                      (quote ())))))))
    (P lst)))


;; Splits a list or string at the nth element.
(define (:split-at n obj)
  (cond ((list? obj) 
         (list (:take n obj) (:drop n obj)))
        ((string? obj) 
         (list (substring obj 0 n) (substring obj n (:count obj))))))


;; Splits a list according to a predicate.
(define (:span pred lst)
  (cond ((null? lst) 
           (list '() '()))
        ((pred (car lst))
           (let* ((tmp (:span pred (cdr lst)))
                  (ys (:1st tmp))
                  (zs (:2nd tmp)))
             (list (cons (car lst) ys) zs)))
        (else
           (list '() lst))))


;; Splits a list according to the complement of a predicate.
(define (:break pred lst)
  (:span (:complement pred) lst))


;; Zips two lists into pairs from each.
(define (:zip l1 l2)
  (if (or (null? l1) (null? l2))
    (quote ())
    (cons (list (car l1) (car l2)) 
          (:zip (cdr l1) (cdr l2)))))


;; Creates an association list from a list.
(at-expand-time
(define (:alist lst)
  (:partition 2 2 (:flatten lst)))
)

;; Replaces a value in an a association list.
(at-expand-time
(define (:rplv key val alst)
  (letrec ((R (lambda (l)
                (cond ((null? l) l)
                      ((eqv? (caar l) key) (cons (list key val) (cdr l)))
                      (else (cons (car l) (R (cdr l))))))))
    (R alst)))
)

;; Removes a tuple in an a association list.
(define (:rmvv key alst)
  (letrec ((R (lambda (l)
                (cond ((null? l) l)
                      ((eqv? (caar l) key) (R (cdr l)))
                      (else (cons (car l) (R (cdr l))))))))
    (R alst)))


;; +---------------------------------------------------------------------+
;; |                                                                     |
;; |                               VECTORS                               |
;; |                                                                     |
;; +---------------------------------------------------------------------+


;; Vector left fold.
(define (:vfoldl fn init vec)
  (letrec ((len (vector-length vec))
           (F (lambda (acc idx)
                (if (= idx len)
                  acc
                  (F (fn acc (vector-ref vec idx)) (:inc idx))))))
    (F init 0)))


;; Vector left fold one.
(define (:vfoldl1 fn vec)
  (if (zero? (vector-length vec))
    (error "(:vfoldl1)" E_EMPTY_VECTOR)
    (:vfoldl fn (vector-ref vec 0) (subvector vec 1 (vector-length vec)))))


;; Vector right fold.
(define (:vfoldr fn init vec)
  (letrec ((F (lambda (acc idx)
                (if (negative? idx)
                  acc
                  (F (fn (vector-ref vec idx) acc) (:dec idx))))))
    (F init (:dec (vector-length vec)))))


;; Vector right fold one.
(define (:vfoldr1 fn vec)
  (if (zero? (vector-length vec))
    (error "(:vfoldr1)" E_EMPTY_VECTOR)
    (let ((end-idx (:dec (vector-length vec))))
      (:vfoldr fn (vector-ref vec end-idx) (subvector vec 0 end-idx)))))


;; +---------------------------------------------------------------------+
;; |                                                                     |
;; |                              SETS                                   |
;; |                                                                     |
;; +---------------------------------------------------------------------+


;; Tests if lst has no duplicate elements.
(define (:set? lst)
  (cond ((null? lst) #t)
        ((:member? (car lst) (cdr lst)) #f)
        (else (:set? (cdr lst)))))


;; Makes a set off a list.
(define (:make-set lst)
  (cond ((null? lst) (quote ()))
        ((:member? (car lst) (cdr lst)) (:make-set (cdr lst)))
        (else (cons (car lst) (:make-set (cdr lst))))))


;; Tests if sub is a subset of set.
(define (:subset? sub set)
  (if (null? sub) 
    #t
    (and (:member? (car sub) set) 
         (:subset? (cdr sub) set))))


;; Tests if any member of set1 belongs to set2.
(define (:intersect? set1 set2)
  (if (null? set1)
    #f
    (or (:member? (car set1) set2)
        (:intersect? (cdr set1) set2))))


;; Returns the intersection of two sets.
(define (:intersect set1 set2)
  (cond ((null? set1) (quote ()))
        ((:member? (car set1) set2) (cons (car set1) (:intersect (cdr set1) set2)))
        (else (:intersect (cdr set1) set2))))


;; Returns the union of two sets.
(define (:union set1 set2)
  (cond ((null? set1) set2)
        ((:member? (car set1) set2) (:union (cdr set1) set2))
        (else (cons (car set1) (:union (cdr set1) set2)))))


;; Returns the elements in set1 that do not belong to set2.
(define (:diff set1 set2)
  (cond ((null? set1) (quote ()))
        ((:member? (car set1) set2) (:diff (cdr set1) set2))
        (else (cons (car set1) (:diff (cdr set1) set2)))))


;; +---------------------------------------------------------------------+
;; |                                                                     |
;; |                            TESTING                                  |
;; |                                                                     |
;; +---------------------------------------------------------------------+

;; Private declarations.

(define-structure Test##_ name docstr fn)
(define-structure Tally##_ total passed failed)

(define :test-name##_)
(define :test-doc##_)
(define :test-flag##_)

(define :total##_)
(define :passed##_)
(define :failed##_)

;; Makes a test record.
(define-macro (:make-test name . args)
  (let* ((span (:span string? args))
         (docstr (if (null? (car span)) "" (caar span)))
         (forms (cadr span)))
    `(make-Test##_ ',name ,docstr (lambda () ,@forms))))


;; Asserts equality.
(define-macro (:equal##_ op form expected . msg)
  (let* ((fsym (gensym))
         (asym (gensym))
         (esym (gensym))
         (bind `((,fsym ,(object->string form))
                 (,asym ,form)
                 (,esym ,expected)))
         (diag `(:str :test-name##_ " " :test-doc##_ ,(:nl)
                      "Evaluated: " ,fsym " ==> " ,asym ,(:nl)
                      "Expected:  " ,esym))
         (text (if (null? msg) diag (:snoc diag (:str (:nl) (car msg))))))
    `(let ,bind
       (:when (not (,op ,asym ,esym))
         (raise ,text)))))

(define-macro (:eqv? form expected . msg)
  `(:equal##_ eqv? ,form ,expected ,@msg))

(define-macro (:equal? form expected . msg)
  `(:equal##_ equal? ,form ,expected ,@msg))


;; Asserts inequality.
(define-macro (:not-equal##_ op form expected . msg)
  (let* ((fsym (gensym))
         (asym (gensym))
         (esym (gensym))
         (bind `((,fsym ,(object->string form))
                 (,asym ,form)
                 (,esym ,expected)))
         (diag `(:str :test-name##_ " " :test-doc##_ ,(:nl)
                      "Incorrect: " ,fsym " ==> " ,asym))
         (text (if (null? msg) diag (:snoc diag (:str (:nl) (car msg))))))
    `(let ,bind
       (:when (,op ,asym ,esym)
         (raise ,text)))))

(define-macro (:not-eqv? form expected . msg)
  `(:not-equal##_ eqv? ,form ,expected ,@msg))

(define-macro (:not-equal? form expected . msg)
  `(:not-equal##_ equal? ,form ,expected ,@msg))


;; Asserts that form evaluates to #t.
(define-macro (:true? form . msg)
  (let* ((fsym (gensym))
         (asym (gensym))
         (bind `((,fsym ,(object->string form))
                 (,asym ,form)))
         (diag `(:str :test-name##_ " " :test-doc##_ ,(:nl)
                      "Evaluated: " ,fsym " ==> " ,asym ,(:nl)
                      "Expected:  #t"))
         (text (if (null? msg) diag (:snoc diag (:str (:nl) (car msg))))))
    `(let ,bind
       (:when (not ,asym)
         (raise ,text)))))


;; Asserts that form evaluates to #f.
(define-macro (:false? form . msg)
  (let* ((fsym (gensym))
         (asym (gensym))
         (bind `((,fsym ,(object->string form))
                 (,asym ,form)))
         (diag `(:str :test-name##_ " " :test-doc##_ ,(:nl)
                      "Evaluated: " ,fsym " ==> " ,asym ,(:nl)
                      "Expected:  #f"))
         (text (if (null? msg) diag (:snoc diag (:str (:nl) (car msg))))))
    `(let ,bind
       (:when ,asym
         (raise ,text)))))


;; Runs a test created with (:make-test).
(define (:run-test test)
  (with-exception-catcher
    (lambda (e)
      (println e)
      (set! :test-flag##_ #f))
    (lambda ()
      (set! :test-name##_ (symbol->string (Test##_-name test)))
      (set! :test-doc##_ (Test##_-docstr test))
      ((Test##_-fn test)))))
