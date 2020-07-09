;;;; lisp-critic.asd

(cl:in-package :asdf)

(defsystem :lisp-critic
  :version "1.1"
  :description "LISP-CRITIC - A Lisp code critiquing package."
  :long-description "The Lisp Critic scans your code for instances of bad Lisp programming practice. The Lisp Critic works for all Lisp code, even if there are no test cases. Use the Critic with all your code, whether it's an exercise, an assignment, or something you invented on your own.
see more - https://courses.cs.northwestern.edu/325/exercises/critic.php#critic"
  :author "Chris Riesbeck"
  :maintainer "CHIBA Masaomi"
  :license "MIT Licence"
  :serial t
  :depends-on (#:ckr-tables)
  :components ((:file "extend-match")
               (:file "write-wrap")
               (:file "lisp-critic")
               (:file "lisp-rules")))

