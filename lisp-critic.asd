;;;; lisp-critic.asd

(cl:in-package :asdf)

(defsystem :lisp-critic
  :serial t
  :components ((:file "tables")
               (:file "extend-match")
               (:file "write-wrap")
               (:file "lisp-critic")
               (:file "lisp-rules")))


