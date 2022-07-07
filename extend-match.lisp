;;;-*- Mode: Lisp; Package: EXTEND-MATCH -*-

;;; Updates:
;;;
;;; 10/11/2011 Added a warning about ?name as a pattern [CKR]
;;; 1/24/2006 Changed EQL to EQUAL in PAT-MATCH to handle strings [CKR]
;;; 1/21/2005 Changed to store extensions by name string, not symbols [CKR]
;;; 1/21/2005 Changed package to be Franz "modern" compatible [CKR]
;;; 3/5/2001 Add listp check to match-segment-extension. [CKR]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensible Pattern Matcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; See
;;;
;;; https://courses.cs.northwestern.edu/325/readings/extend-pat.php
;;;
;;; for information.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defpackage #:extend-match
  (:use #:common-lisp)
  (:export #:pat-match #:bind-variable #:add-extension #:instantiate-pattern)
  )

(in-package #:extend-match)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Design note:
;;;
;;; Technically, we could treat any list of the form (name ...)
;;; where name is in a pattern extension table as a pattern extension.
;;; This however would mean that
;;;
;;;   - misspelling the name of a pattern extension would cause
;;;     a match failure, but no error message, and
;;;   - unintentionally using the name of a pattern extension
;;;     would cause unexpected match results
;;;   - later definition of extensions could break previously
;;;     working patterns
;;;
;;; Therefore, all pattern extension names must start with
;;; a question mark (?) and anything starting with a question
;;; mark is presumed to be a pattern extension.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *known-pattern-types* '(:single :segment :none))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable name stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant var-prefix
  (if (boundp 'var-prefix)
      (symbol-value 'var-prefix)
      "?"))

(defun pat-var-p (form)
  (or (single-pat-var-p form)
      (segment-pat-var-p form)))

(defun single-pat-var-p (form)
  (and (consp form)
       (eql (pat-function (first form)) 'match-variable)))

(defun segment-pat-var-p (form)
  (and (consp form)
       (eql (pat-function (first form)) 'match-segment-variable)))

(defun pat-var-name (pat) (second pat))

(defun var-type-name-p (pat)
  (and (symbolp pat)
       (prefix-p var-prefix (symbol-name pat))))

(defun prefix-p (seq1 seq2 &key (test #'eql))
  (and (<= (length seq1) (length seq2))
       (every test seq1 seq2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extension tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pattern-types* (make-hash-table :test 'equal))
(defvar *pattern-functions* (make-hash-table :test 'equal))

(defun pat-function (name)
  (and (symbolp name)
       (gethash (symbol-name name) *pattern-functions*)))

(defsetf pat-function (name) (value)
  `(setf (gethash (symbol-name ,name) *pattern-functions*)
         ,value))

(defun pat-type (name)
  (and (symbolp name)
       (gethash (symbol-name name) *pattern-types*)))

(defsetf pat-type (name) (value)
  `(setf (gethash (symbol-name ,name) *pattern-types*)
         ,value))

(defun add-extension (name type function)
  (unless (var-type-name-p name)
    (error "Not a valid variable type name: ~S" name))
  (unless (member type *known-pattern-types*)
    (error "Not a valid extension type: ~S" type))

  (let ((old-type (pat-type name)))
    (unless (or (null old-type)
                (eql old-type type))
      (warn "Redefining ~A pattern extension ~S as ~A"
             old-type name type)))
  (setf (pat-type name) type)
  (setf (pat-function name) function))

(defun pat-extension-p (pat)
  (and (consp pat)
       (var-type-name-p (first pat))))

(defun segment-pat-extension-p (pat)
  (and (consp pat)
       (eql (pat-type (first pat)) :segment)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The matcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pat-match (pat form &optional (blists '(nil)))
  (cond ((null blists) nil)
        ((pat-extension-p pat)
         (match-extension pat form blists))
        ((var-type-name-p pat)
         (warn "Undefined pattern form ~S" pat))
        ((equal pat form) blists)
        ((atom pat) nil)
        ((segment-pat-extension-p (first pat))
         (match-segment-extension pat form blists))
        ((atom form) nil)
        (t (pat-match (cdr pat) (cdr form)
                      (pat-match (car pat) (car form) blists)))))

(defun match-extension (pat form blists)
  (let ((fn (pat-function (first pat))))
      (when (null fn)
        (warn "Undefined pattern extension ~S" pat))
      (case (pat-type (first pat))
        (:single (funcall fn (rest pat) form blists))
        (:none (funcall fn (rest pat) blists)))))

(defun match-segment-extension (pats form blists)
  (and (listp form)
       (let ((pat (first pats))
             (rest-pats (rest pats)))
         (let ((fn (pat-function (first pat))))
           (when (null fn)
             (warn "Undefined pattern extension ~S" pat))
           (funcall fn (rest pat) rest-pats form blists)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instantiating patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; More complex than in pat-match.lisp, because we have two
;;; kinds of variables and variables are not simple symbols.

(defun instantiate-pattern (form blist)
  (cond ((pat-var-p form) (instantiate-var form blist))
        ((atom form) form)
        (t (instantiate-cons form blist))))

(defun instantiate-cons (form blist)
  (let ((inst-first (instantiate-pattern (first form) blist))
        (inst-rest (instantiate-pattern (rest form) blist)))
    (cond ((segment-pat-var-p (first form))
           (append inst-first inst-rest))
          (t (cons inst-first inst-rest)))))

(defun instantiate-var (form blist)
  (let ((binding (get-binding (pat-var-name form) blist)))
    (cond ((null binding) form)
          (t (binding-value binding)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binding stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Exactly the same as in pat-match.lisp

(defun bind-variable (name input blists)
  (cond ((null blists) nil)
        ((null name) blists)
        ((null (rest blists))
         (extend-bindings name input blists))
        (t (mapcan #'(lambda (blist)
                       (copy-list
                        (extend-bindings name input (list blist))))
                   blists))))

(defun extend-bindings (name input blists)
  (let ((binding (get-binding name (first blists))))
    (cond ((null binding)
           (add-binding name input blists))
          ((equal (binding-value binding) input)
           blists)
          (t nil))))

(defun get-binding (name bindings)
  (assoc name bindings))

(defun add-binding (name input blists)
  (list (acons name input (first blists))))

(defun binding-variable (binding) (first binding))

(defun binding-value (binding) (rest binding))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pattern extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Usage: (? [name]).  (?) is for anonymous matching

(add-extension '? :single 'match-variable)

(defun match-variable (args input blists)
  (destructuring-bind (&optional name) args
    (bind-variable name input blists)))


;;; Usage: (?* [name]).  (?*) is for anonymous matching

(add-extension '?* :segment 'match-segment-variable)

(defun match-segment-variable (args pats input blists)
  (destructuring-bind (&optional name) args
    (do* ((tail input (rest tail))
          (new-blists (match-tail tail name pats input blists)
                      (append (match-tail tail name pats input blists)
                              new-blists)))
         ((null tail) new-blists))))

(defun match-tail (tail name pats input blists)
  (let ((blists (pat-match pats tail blists)))
    (if (null blists)
      nil
      (bind-variable name (ldiff input tail) blists))))


;;; Usage: (?and pat1 pat2 ...)
;;;
;;; Matches if all the patterns match with a common
;;; set of bindings. Equivalent to (pat-match pat1
;;; input (pat-match pat2 input ...))

(add-extension '?and :single 'match-and)

(defun match-and (args input blists)
  (cond ((null blists) nil)
        ((null args) blists)
        (t (match-and (rest args) input
                      (pat-match (first args) input blists)))))

;;; Usage: (?is predicate)
;;;
;;; Matches if (funcall predicate input) returns true.

(add-extension '?is :single 'match-predicate)

(defun match-predicate (args input blists)
  (destructuring-bind (pred) args
    (and (not (null blists))
         (funcall pred input)
         blists)))

;;; Usage: (?match pat1 pat2) -- pat1 will be matched against
;;; the instantiation of pat2

(add-extension '?match :none 'match-match)

(defun match-match (args blists)
  (destructuring-bind (pat input-pat) args
    (loop for blist in blists
          append (pat-match pat (instantiate-pattern input-pat blist)
                            blists))))


;;; Usage: (?not pat)
;;;
;;; Matches if (pat-match pat input) returns false.

(add-extension '?not :single 'match-not)

(defun match-not (args input blists)
  (destructuring-bind (pat) args
    (cond ((null blists) nil)
          ((pat-match pat input blists) nil)
          (t blists))))


;;; Usage: (?or pat1 pat2 ...)
;;;
;;; This returns all successful matches with any
;;; of the patterns. E.g.,
;;;
;;; ? (pat-match '(?or (?is numberp) (? x)) 1)
;;; (NIL ((X 1)))
;;;
;;; because (?is numberp) matches and returns the
;;; empty binding list, and (? x) also matches.

(add-extension '?or :single 'match-or)

(defun match-or (args input blists)
  (cond ((null blists) nil)
        ((null args) nil)
        (t (append (pat-match (first args) input blists)
                   (match-or (rest args) input blists)))))


(provide "extend-match")
