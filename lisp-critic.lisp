;;;-*- Mode: Lisp; Package: LISP-CRITIC -*-

#|
Copyright (C) 1997-2005 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
|#

;;; A Lisp code critiquing package.
;;; Author: Chris Riesbeck
;;;
;;; Update history:
;;;
;;; 12/2/19 added NAME-STARTS-WITH [CKR]
;;; 09/18/05 replaced Academic Free License with MIT Licence [CKR]
;;; 08/30/05 added license notice [CKR]
;;; 3/10/05 fixed REMOVE-LISP-PATTERN to remove responses [CKR]
;;; 1/29/2005 exported more functions to support web editor [CKR]
;;; 1/27/2005 removed internal global *PATTERN-NAMES* [CKR]
;;; 1/26/2005 exported ADD-LISP-PATTERN, REMOVE-LISP-PATTERN [CKR]
;;; 1/22/2005 fixed stupid bug in *TOP-LEVEL* handling [CKR]
;;; 1/21/2005 re-implemented special ?TOP-LEVEL pattern [CKR]
;;; 1/21/05 Removed unnecessary pattern extension exports [CKR]
;;; 1/3/03 made DEFPACKAGE compatible with Allegro Modern [CKR]
;;; 1/3/03 removed duplicates in SETS-FREE-VARS [CKR]
;;; 1/3/03 merged DEFINE-LISP-PATTERN and DEFINE-RESPONSE [CKR]
;;; 12/28/01 fixed bugs caused by dotted pairs in code [CKR]
;;; 2/1/01 added NAME-ENDS-WITH pattern [CKR]
;;; 1/15/01 added code to catch (critique 'name) [CKR]
;;; 12/17/98 sped up printing [CKR]
;;; 10/6/98 added a check for no rules [CKR]
;;; 11/18/97 upped length threshold by 5 [CKR]
;;; 11/18/97 added N parameter for ?REPEAT [CKR]
;;; 10/21/97 added LABELS and FLET to CODE-VARS [CKR]
;;; 10/2/97 simplified a FORMAT string for XlispStat [CKR]
;;; 10/2/97 exported ?and etc from extend-match [CKR]


#|
1. Load TABLES, WRITE-WRAP, EXTEND-MATCH, LISP-CRITIC and
   BAD-LISP.RULES

2. Type (USE-PACKAGE :LISP-CRITIC) -- DON'T FORGET THIS!

3. Critique your code with CRITIQUE-DEFINITION.

Example call:

 (critique
    (defun count-a (lst)
      (setq n 0)
      (dolist (x lst)
        (if (equal x 'a)
          (setq n (+ n 1))))
      n))

Example output:

----------------------------------------------------------------------

SETS-GLOBALS: GLOBALS!! Don't use global variables, i.e., N N
----------------------------------------------------------------------

DOLIST-SETF: Don't use SETQ inside DOLIST to accumulate values for N.
Use DO. Make N a DO variable and don't use SETQ etc at all.
----------------------------------------------------------------------

USE-EQL: Unless something special is going on, use EQL, not EQUAL.
----------------------------------------------------------------------

X-PLUS-1: Don't use (+ N 1), use (1+ N) for its value or (INCF N) to
change N, whichever is appropriate here.
----------------------------------------------------------------------



If you get a CRITIQUE-DEFINITION undefined error, it's because you
forgot the USE-PACKAGE. Do this to fix things:

  (UNINTERN 'CRITIQUE-DEFINITION)

  (USE-PACKAGE :LISP-CRITIC)

|#

(cl:defpackage #:lisp-critic
  (:use  #:common-lisp #:tables #:extend-match #:write-wrap)
  (:import-from #:extend-match #:match-and)
  (:export #:critique #:critique-file #:critique-definition
           #:apply-critique-rule
           #:lisp-critic-version
           #:add-lisp-pattern #:define-lisp-pattern #:remove-lisp-pattern
           #:get-pattern #:get-response #:get-pattern-names
           #:response-args #:response-format-string
           #:clear-critique-db)
  )

(in-package #:lisp-critic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables and tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftable get-pattern)
(deftable get-response)

(defparameter *length-threshold* 55)

(deftable get-local-vars-fn)
(deftable get-assigned-vars-fn)

(defvar *critic-version* 1.1)

;;; used by ?TOP-LEVEL ,set by FIND-CRITIQUE
(defvar *top-level* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (LISP-CRITIC-VERSION [n]) => version or boolean
;;;   If no argument is given, returns current critic version
;;;   If a number n is given, returns true if critic is at least
;;;      version n or higher.

(defun lisp-critic-version (&optional n)
  (if (null n)
    *critic-version*
    (>= *critic-version* n)))


(defun clear-critique-db ()
  (clear-table (get-pattern))
  (clear-table (get-response))
  nil)

(defparameter *output-width* 70)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (critique
            (:type list)
            (:constructor new-critique (name blist code)))
  name blist code)

(defstruct (response
            (:type list)
            (:constructor new-response (format-string args)))
  format-string args)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defining Lisp patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro define-lisp-pattern (name pattern format-string &rest args)
  (unless (symbolp name)
    (error "Non-symbolic Lisp pattern name ~S" name))
  `(add-lisp-pattern ',name ',pattern ,format-string ',args))

(defun add-lisp-pattern (name pat format-string args)
  (setf (get-pattern name) pat)
  (setf (get-response name) (new-response format-string args))
  name)

(defun get-pattern-names ()
  (let ((l nil))
    (map-table #'(lambda (name pat)
                   (declare (ignore pat))
                   (push name l))
               (get-pattern))
    (sort l #'string<)))

(defun remove-lisp-pattern (name)
  (remove-key name (get-pattern))
  (remove-key name (get-response)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CRITIQUE, -DEFINITION, CRITIQUE-FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro critique (form)
  `(critique-definition ',form))

;;; A common bug is (critique 'foo) which becomes
;;; (critique-definition (quote (quote foo)) -- so we check
;;; for that specially.

(defun critique-definition
      (defn &optional (out *standard-output*) (names (get-pattern-names)))
    (cond ((or (atom defn)
               (and (eql (car defn) 'quote)
                    (or (atom (cadr defn))
                        (and  (eql (caadr defn) 'quote)
                              (atom (cadadr defn))))))
           (format t "~&Can't critique ~S -- I need the actual definition~%"
                   defn))
          ((null names)
           (format t "~&You forgot to load bad-lisp.rules~%"))
          (t
           (print-critique-responses (generate-critiques defn names) out)))
    (values))

(defun critique-file
       (file &optional (out *standard-output*) (names (get-pattern-names)))
  (with-open-file (in file)
    (let ((eof (list nil)))
      (do ((code (read in nil eof) (read in nil eof)))
          ((eq code eof) (values))
        (print-separator out #\*)
        (let ((*print-right-margin* *output-width*))
          (pprint code out))
        (critique-definition code out names)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-critiques (code names)
  (loop for name in names
        append (apply-critique-rule name code)))

(defun apply-critique-rule (name code)
  (find-critiques name (get-pattern name) code :blists '(nil) :top-level t))

(defun print-critique-responses (critiques
                                 &optional (stream *standard-output*))
  (let ((*print-pretty* nil))
    (when critiques
      (print-separator stream))
    (dolist (critique critiques)
      (print-critique-response critique stream))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIND-CRITIQUES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-critiques (name pat code &key (blists '(nil)) ((:top-level *top-level*) *top-level*))
  (let ((new-blists (critique-match pat code blists)))
    (cond ((not (null new-blists))
           (make-critiques name new-blists code))
      ((atom code) nil)
       (t
       (or (find-critiques name pat (car code) :blists blists)
           (find-critiques name pat (cdr code) :blists blists))))))


(defun critique-match (pat code blists)
  (pat-match pat code blists))

(defun make-critiques (name blists code)
  (mapcar #'(lambda (blist) (new-critique name blist code))
          blists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Critique message printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-critique-response (critique
                                &optional (stream *standard-output*))
  (let ((name (critique-name critique))
        (blist (critique-blist critique))
        (code (critique-code critique)))
    (let ((response (get-response name)))
      (cond ((null response)
             (let ((*print-lines* 2) (*print-pretty* t)
                   (*print-right-margin* *output-width*))
               (format stream "~&~A: Code: ~W" name code)))
            (t
             (write-wrap stream
                         (make-response-string name response blist)
                         *output-width*)))
      (print-separator stream))))

(defun make-response-string (name response blist)
  (declare (ignore name))
  (format nil "~&~?"
          (response-format-string response)
          (instantiate-pattern (response-args response)
                               blist)))

;;; the following can be done with
;;;
;;;   (format stream "~&~V,,,V<~:*~A~>~%" *output-width* ch)
;;;
;;; but XlispStat 3.50 doesn't handle that and everyone has
;;; to run to Steele to see what it does.


(defun print-separator (&optional (stream  *standard-output*)
                                  (ch #\-))
  (format stream "~&~A~%"
    (make-string *output-width* :initial-element ch)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matcher extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; General extensions -- useful lots of places

;;; (?CONTAINS pat) -- matches anything containing something matching
;;;    pat

(add-extension '?contains :single 'match-contains)

(defun match-contains (args input blists)
  (destructuring-bind (pat) args
    (find-match pat input blists)))

(defun find-match (pat input blists)
  (or (pat-match pat input blists)
      (and (consp input)
           (or (find-match pat (first input) blists)
               (find-match pat (rest input) blists)))))


;;; (?REPEAT pat [n]) -- matches N or more occurrences of pat;
;;;   N defaults to 1

(add-extension '?repeat :segment 'match-repeat)

(defun match-repeat (args pats input blists)
  (and (not (null input))
       (destructuring-bind (pat &optional (n 1)) args
         (match-repeat-pat n pat pats input blists))))

(defun match-repeat-pat (n pat pats input blists)
  (unless (null input)
    (let ((blists (pat-match pat (first input) blists)))
      (cond ((null blists) nil)
            ((> n 1)
             (match-repeat-pat (1- n) pat pats (rest input) blists))
            (t (append (pat-match pats (rest input) blists)
                       (match-repeat-pat n pat pats (rest input) blists)
                       ))))))


;;; (?OPTIONAL pat) -- matches zero or one occurrences of pat

(add-extension '?optional :segment 'match-optional)

(defun match-optional (args pats input blists)
  (let ((skip-blists (pat-match pats input blists))
        (no-skip-blists
         (and (not (null input))
              (pat-match pats (rest input)
                         (pat-match (first args) (first input) blists)))))
    (cond ((null skip-blists) no-skip-blists)
          ((null no-skip-blists) skip-blists)
          (t (append skip-blists no-skip-blists)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensions useful for critiquing Lisp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (?NAME-CONTAINS string) -- matches a symbol containing
;;; the given string (case is ignored)

(add-extension '?name-contains :single 'match-name-contains)

(defun match-name-contains (args input blists)
  (destructuring-bind (substring) args
    (and (symbolp input)
         (search substring (symbol-name input)
                 :test #'char-equal)
         blists)))

;;; (?NAME-ENDS-WITH string) -- matches a symbol ending with
;;; the given string (case is ignored)

(add-extension '?name-ends-with :single 'match-name-ends-with)

(defun match-name-ends-with (args input blists)
  (destructuring-bind (substring) args
    (and (symbolp input)
         (string-ends-with (symbol-name input) substring)
         blists)))

(defun string-ends-with (str substr)
  (let ((strlen (length str))
        (substrlen (length substr)))
    (and (> strlen substrlen)
         (string-equal str substr :start1 (- strlen substrlen)))))

;;; (?NAME-STARTS-WITH string) -- matches a symbol starting with
;;; the given string (case is ignored)

(add-extension '?name-starts-with :single 'match-name-starts-with)

(defun match-name-starts-with (args input blists)
  (destructuring-bind (substring) args
    (and (symbolp input)
         (string-starts-with-p (symbol-name input) substring)
         blists)))

(defun string-starts-with-p (str substr)
  (let ((strlen (length str))
        (substrlen (length substr)))
    (and (>= strlen substrlen)
         (string-equal str substr :end1 substrlen))))

(add-extension '?user-defined-name-starts-with :single 'match-user-defined-name-starts-with)

(defun standard-symbolp (sym)
  (and (symbolp sym)
       (eql (load-time-value (find-package "CL"))
            (symbol-package sym))))

(defun match-user-defined-name-starts-with (args input blists)
  (destructuring-bind (substring) args
    (and (symbolp input)
         (not (standard-symbolp input))
         (string-starts-with-p (symbol-name input) substring)
         blists)))

;;; (?EQL-PRED [name]) -- matches a Lisp equality predicate
;;;   (except =) and binds name to it, if given

(add-extension '?eql-pred :single 'match-eql-pred)

(defun match-eql-pred (args input blists)
  (destructuring-bind (&optional name) args
    (and (member input '(eq eql equal equalp))
         (bind-variable name input blists))))


;;; (?TOO-LONG [name]) -- matches if code is too long
;;;   (LIST-COUNT > *LENGTH-THRESHOLD*) and binds name
;;    to LIST-COUNT, if given

(add-extension '?too-long :single 'match-too-long)

(defun match-too-long (args input blists)
  (destructuring-bind (&optional name) args
    (let ((badness (get-length-badness input)))
      (when (> badness 0)
        (bind-variable name
                       (get-badness-phrase badness)
                       blists)))))


(defun get-length-badness (code)
  (let ((code-length (list-count code)))
    (/ (- code-length *length-threshold*)
       *length-threshold*)))

#| doesn't handle dotted pairs

(defun list-count (form)
  (cond ((atom form) 0)
        (t (reduce #'+ form
                   :key #'list-count
                   :initial-value (length form)))))
|#

(defun list-count (form)
  (cond ((null form) 0)
        ((atom form) 1)
        (t (+ (list-count (car form))
              (list-count (cdr form))))))

(defun get-badness-phrase (badness)
  (cond ((<= badness 1/4) "a little")
        ((<= badness 1/2) "somewhat")
        ((<= badness 3/4) "")
        (t "way")))


;;; (?SETS-FREE-VARS [name]) -- matches any Lisp code containing
;;;    assignments to free variables -- binds name to a list of the
;;;    free variables if given
;;;
;;; For all that there's lot of code here, it's still very crude.
;;; Most of the code is to handle all the ways Common Lisp can
;;; assign and create variables.

(add-extension '?sets-free-vars :single 'match-sets-free-vars)

(defun match-sets-free-vars (args input blists)
  (destructuring-bind (&optional name) args
    (let ((vars (remove-duplicates (find-assigned-free-vars input))))
      (if (null vars) nil
          (bind-variable name vars blists)))))

;;; Usage: (?top-level pat1 pat2 ...)
;;;
;;; Matches if (pat-match pat input) is true at the top-level
;;; of input, i.e, no nesting.

(add-extension '?top-level :single 'match-top-level)

(defun match-top-level (args input blists)
  (and *top-level*
       (match-and args input blists)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting assigned free variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Very quick and dirty. Doesn't know real
;;; scope rules, assumes anything nested is scoped, e.g.,
;;;
;;;   (do ((x (setq x 2) ...)) ...)
;;;
;;; is not considered a free variable assignment.

(defun find-assigned-free-vars (code &optional env-stack)
  (or (code-assigned-free-vars code env-stack)
      (and (consp code)
           (let ((new-stack (cons code env-stack)))
             (loop for l = code then (cdr l)
                   until (atom l)
                   append (find-assigned-free-vars (car l) new-stack))))))

(defun code-assigned-free-vars (code &optional env-stack)
  (let ((vars (code-assigned-vars code)))
    (cond ((null vars) nil)
          (t (get-free-vars vars env-stack)))))

(defun get-free-vars (vars env-stack)
  (cond ((null env-stack) vars)
        ((null vars) nil)
        (t (get-free-vars (remove-local-vars vars (first env-stack))
                          (rest env-stack)))))

(defun remove-local-vars (vars code-env)
  (let ((local-vars (code-vars code-env)))
    (cond ((null local-vars) vars)
          (t (set-difference vars local-vars)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting assigned variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun code-assigned-vars (code)
  (unless (atom code)
    (let ((fn (get-assigned-vars-fn (first code))))
      (cond ((null fn) nil)
            (t (remove-if-not #'symbolp (funcall fn code)))))))

(dolist (fn '(psetf psetq rotatef setf setq shiftf))
  (setf (get-assigned-vars-fn fn)
        #'(lambda (code)
            (do ((tail (cdr code) (cddr tail))
                 (vars nil (cons (first tail) vars)))
                ((null tail) vars)))))

(dolist (fn '(decf incf pop))
  (setf (get-assigned-vars-fn fn)
        #'(lambda (code) (list (second code)))))

(dolist (fn '(push pushnew))
  (setf (get-assigned-vars-fn fn)
        #'(lambda (code) (list (third code)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting new local variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun code-vars (code)
  (unless (atom code)
    (let ((fn (get-local-vars-fn (first code))))
      (cond ((null fn) nil)
            (t (funcall fn code))))))

(defun get-vars (vars-list)
  (loop for var-form in vars-list
        for var = (get-var var-form)
        unless (member var lambda-list-keywords)
          collect var))

(defun get-var (var-form)
  (cond ((atom var-form) var-form)
        (t (get-var (car var-form)))))


(dolist (fn '(defmacro defun))
  (setf (get-local-vars-fn fn)
        #'(lambda (code) (get-vars (third code)))))

(dolist (fn '(destructuring-bind do do* lambda let let*
              multiple-value-bind))
  (setf (get-local-vars-fn fn)
        #'(lambda (code) (get-vars (second code)))))

(dolist (fn '(dolist dotimes with-open-file with-open-stream))
  (setf (get-local-vars-fn fn)
        #'(lambda (code) (list (get-var (second code))))))

(dolist (fn '(flet labels))
  (setf (get-local-vars-fn fn)
        #'(lambda (code)
            (loop for def in (second code)
                  append (second def)))))

(setf (get-local-vars-fn 'loop) 'get-loop-vars)

(defun get-loop-vars (code)
  (cond ((atom code) nil)
        (t (let ((tail (member-if #'loop-binder-p code)))
             (cond ((null tail) nil)
                   (t (cons (get-var (second tail))
                            (get-loop-vars (cddr tail)))))))))

(defun loop-binder-p (x)
  (and (symbolp x)
       (member x '(for with and) :test #'string=)))



(provide "lisp-critic")

#|
CHANGE LOG

12/17/98 sped up --- printing [CKR]
Problem: the --- lines printed very slowly in ACL 3 Lite
Cause: - were printed one character at a time
Change: made a --- string first then printed

7/10/97 simplified named patterns [CKR]
Problem: function names like (pattern-pattern ...)
Cause: named patterns kept in list
Change: replaced with get-pattern table and separate
        name list to maintain order of patterns (though
        order's not important right now)
|#
