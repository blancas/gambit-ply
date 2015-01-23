;; +-------------------------------------------------------------+
;; | catpom - Prints a catalog of a POM's artifacts.             |
;; |                                                             |
;; | Run this command on the same directory as the POM.          |
;; |                                                             |
;; | Usage:                                                      |
;; | $ catpom                                                    |
;; +-------------------------------------------------------------+

(include "plylib/ply-sxml.scm")

(define POM "pom.xml")

(define (get-main sxml)
  (let* ((main (:xpath-get "//project/properties/main/text()" sxml)))
    (if (null? main) "" (car main))))

(define (get-tag tag sxml)
  (let ((pair (:xpath-get (:str "/project/" tag) sxml)))
    (if (not (null? pair))
      (car pair)
      (car (:xpath-get (:str "/project/parent/" tag) sxml)))))

(define (get-artifact sxml repo)
  (let ((gid (get-tag "groupId" sxml))
        (aid (get-tag "artifactId" sxml))
        (vid (get-tag "version" sxml)))
    (make-path (list gid aid vid) repo)))

(define (get-deps file)
  (let ((sxml (:file->sxml file)))
    (:xpath-get "//project/dependencies/*" sxml)))

(define (get-super path)
  (if (file-exists? path)
    (append (get-super (:str "../" path)) (get-deps path))
    (quote ())))

(define (make-path dep repo)
  (let ((gid (:str-join "/" (:split "\\." (:2nd (assq 'groupId dep)))))
        (aid (:2nd (assq 'artifactId dep)))
        (vid (:2nd (assq 'version dep))))
    (:str repo "/" gid "/" aid "/" vid "/" aid "-" vid ".jar")))

(define (jar-paths deps repo)
  (if (null? deps)
    (quote ())
    (cons (make-path (cdar deps) repo) (jar-paths (cdr deps) repo))))

(let* ((sxml (:file->sxml POM))
       (repo (:str (getenv "HOME") "/.m2/repository")))
  (println (get-artifact sxml repo))
  (println (get-main sxml))
  (newline)
  (:dolist (j (jar-paths (get-super POM) repo)) (println j)))
