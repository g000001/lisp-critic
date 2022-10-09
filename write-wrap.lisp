;;;-*- Mode: Lisp; Package: WRITE-WRAP -*-

#|
Copyright (c) 2003 Christopher K. Riesbeck

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

;;; Updates:
;;; 1/3/03 made DEFPACKAGE compatible with Allegro Modern [CKR]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defpackage #:write-wrap
  (:use #:common-lisp)
  (:export #:write-wrap)
  )

(in-package #:write-wrap)

;;; (WRITE-WRAP stream string width &optional indent first-indent)
;;;   writes string to stream, split into width-size lengths, breaking
;;;   at returns and spaces in the string, if possible, indenting every
;;;   line indent spaces (default = 0), except the first line which is
;;;   indented first-indent spaces (default = indent).
;;;
;;; Note: to generate a string simply use with-output-to-string
;;;   (WITH-OUTPUT-TO-STRING (s) (WRITE-WRAP s ...))
;;;
;;; Had to turn off *PRINT-PRETTY* because Franz turns it on
;;; by default and if you turn it off globally, it breaks the IDE
;;; in 6.0!

(defun write-wrap (stream strng width
                   &key indent (first-indent indent))
  (let ((*print-pretty* nil))
    (do* ((end (length strng))
          (indent-string (indent-string indent))
          (first-indent-string (indent-string first-indent))
          (start 0 (1+ next))
          (next (break-pos strng start end width)
                (break-pos strng start end width))
          (margin first-indent-string indent-string))
         ((null next))
      (when margin (write-string margin stream))
      (write-string strng stream :start start :end next)
      (terpri stream))))

(defun indent-string (size)
  "When size is a positive fixnum, indent-string returns a simple
   string of length size whose elements have been initialized to space."
  (when (and size (> size 0))
    (make-string size :initial-element #\space)))

;;; (whitespace-p char) is true if ch is whitespace.

(defun whitespace-p (ch)
 (member ch '(#\linefeed #\newline #\return #\space #\tab)))

;;; (break-pos string start end width) returns the position to break string
;;;   at, guaranteed to be no more than width characters.  If there's a`
;;;   return, its position is used, else the last space before the width
;;;   cutoff, else width.  If the end comes before width, then end is 
;;;   returned.

(defun break-pos (strng start end width)
 (unless (or (null start) (>= start end))
   (let ((limit (min (+ start width) end)))
     (or (position #\newline strng :start start :end limit)
	 (and (= end limit) end)
	 (position #\space strng :start start :end limit :from-end t)
	 limit))))		;;insert warning here, if desired


#|
;;; (non-whitespace-pos string &optional start) returns the position of
;;;   the first non-whitespace character in string, after start, if any.

;;; Not used now but was used before to set and update START in WRITE-WRAP
;;; to skip spaces. The current WRITE-WRAP keeps user spacing, except
;;; when replacing a space with a line break.

(defun non-whitespace-pos (strng &optional (start 0))
  (position-if-not #'whitespace-p strng :start start))

|#

(provide "write-wrap")
