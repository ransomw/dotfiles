;; link to ~/.guile

(define (my-add x y) (+ x y))


(use-modules (oop goops))

(default-duplicate-binding-handler
  '(merge-generics replace warn-override-core warn last))

(add-to-load-path "~/ws/guile")

(add-to-load-path "/opt/guile-charting/share/guile/site/3.0")
