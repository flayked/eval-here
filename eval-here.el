;;; eval-here.el --- Lexical environment eval  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Satoshi Nakamoto
;; Copyright (C) 1993, 2001-2020 Free Software Foundation, Inc.
;; (Free Software Foundation, Inc. is the copyright holder of
;; the original version of the cl--sm-macroexpand function,
;; which I have copied here and modified.)

;; Author: Satoshi Nakamoto
;; Author: Dave Gillespie <daveg@synaptics.com>
;; Keywords: extensions

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This version of eval-here seems to work on emacs-27.0.91.

;; This is an ugly hack to get something resembling the pandoric-eval
;; macro from Let Over Lambda.

;; https://letoverlambda.com/textmode.cl/guest/chap6.html

;; The function-like macro eval-here behaves like eval, except that lexical,
;; cl-symbol-macrolet, and cl-macrolet bindings will be available
;; to use within the evaluated form.

;;; Code:

(require 'cl)
;; Used for gv-ref and gv-deref.
(require 'gv)
;; Used for seq-contains.
(require 'seq)

;; Kludge to ensure that autoload macros are loaded before we
;; start advising them.
(cl-symbol-macrolet nil (+ 3 2))
(cl-macrolet nil (+ 3 2))

(defun eval-here--myfun (second third venv)
  (let ((newthird (car (cdr third)))
	(newsecond (cdr second)))
    (dolist (x venv)
      (unless (seq-contains newthird (car x))
	(push (car x) newthird)
	(push `(gv-ref ,(car x)) newsecond)))
    (cons (cons 'list newsecond) (cons 'quote (cons newthird nil)))))

(defun cl--sm-macroexpand (orig-fun exp &optional env)
  "Special macro expander advice used inside `cl-symbol-macrolet'.
This function extends `macroexpand' during macro expansion
of `cl-symbol-macrolet' to additionally expand symbol macros."
  (let ((macroexpand-all-environment env)
        (venv (alist-get :cl-symbol-macros env)))
    (while
        (progn
          (setq exp (funcall orig-fun exp env))
          (pcase exp
            ((pred symbolp)
             ;; Perform symbol-macro expansion.
             (let ((symval (assq exp venv)))
               (when symval
                 (setq exp (cadr symval)))))
	    (`(eval--here ,secondarg ,thirdarg . ,restargs)
	     (let ((outputs (eval-here--myfun secondarg thirdarg venv)))
	       (let ((outsec (car outputs))
		     (outthi (cdr outputs)))
		 (setq exp `(eval--here ,outsec ,outthi ,@restargs))))
	     nil)
            (`(setq . ,args)
             ;; Convert setq to setf if required by symbol-macro expansion.
             (let ((convert nil)
                   (rargs nil))
               (while args
                 (let ((place (pop args)))
                   ;; Here, we know `place' should be a symbol.
                   (while
                       (let ((symval (assq place venv)))
                         (when symval
                           (setq place (cadr symval))
                           (if (symbolp place)
                               t        ;Repeat.
                             (setq convert t)
                             nil))))
                   (push place rargs)
                   (push (pop args) rargs)))
               (setq exp (cons (if convert 'setf 'setq)
                               (nreverse rargs)))
               convert))
            ;; CL's symbol-macrolet used to treat re-bindings as candidates for
            ;; expansion (turning the let into a letf if needed), contrary to
            ;; Common-Lisp where such re-bindings hide the symbol-macro.
            ;; Not sure if there actually is code out there which depends
            ;; on this behavior (haven't found any yet).
            ;; Such code should explicitly use `cl-letf' instead, I think.
            ;;
            ;; (`(,(or `let `let*) . ,(or `(,bindings . ,body) dontcare))
            ;;  (let ((letf nil) (found nil) (nbs ()))
            ;;    (dolist (binding bindings)
            ;;      (let* ((var (if (symbolp binding) binding (car binding)))
            ;;             (sm (assq var venv)))
            ;;        (push (if (not (cdr sm))
            ;;                  binding
            ;;                (let ((nexp (cadr sm)))
            ;;                  (setq found t)
            ;;                  (unless (symbolp nexp) (setq letf t))
            ;;                  (cons nexp (cdr-safe binding))))
            ;;              nbs)))
            ;;    (when found
            ;;      (setq exp `(,(if letf
            ;;                       (if (eq (car exp) 'let) 'cl-letf 'cl-letf*)
            ;;                     (car exp))
            ;;                  ,(nreverse nbs)
            ;;                  ,@body)))))
            ;;
            ;; We implement the Common-Lisp behavior, instead (see bug#26073):
            ;; The behavior of CL made sense in a dynamically scoped
            ;; language, but nowadays, lexical scoping semantics is more often
            ;; expected.
            (`(,(or 'let 'let*) . ,(or `(,bindings . ,body) dontcare))
             (let ((nbs ()) (found nil))
               (dolist (binding bindings)
                 (let* ((var (if (symbolp binding) binding (car binding)))
                        (val (and found (consp binding) (eq 'let* (car exp))
                                  (list (macroexpand-all (cadr binding)
                                                         env)))))
                   (push (if (assq var venv)
                             ;; This binding should hide "its" surrounding
                             ;; symbol-macro, but given the way macroexpand-all
                             ;; works (i.e. the `env' we receive as input will
                             ;; be (re)applied to the code we return), we can't
                             ;; prevent application of `env' to the
                             ;; sub-expressions, so we need to α-rename this
                             ;; variable instead.
                             (let ((nvar (make-symbol (symbol-name var))))
                               (setq found t)
                               (push (list var nvar) venv)
                               (push (cons :cl-symbol-macros venv) env)
                               (cons nvar (or val (cdr-safe binding))))
                           (if val (cons var val) binding))
                         nbs)))
               (when found
                 (setq exp `(,(car exp)
                             ,(nreverse nbs)
                             ,@(macroexp-unprogn
                                (macroexpand-all (macroexp-progn body)
                                                 env)))))
               nil))
            ;; Do the same as for `let' but for variables introduced
            ;; via other means, such as `lambda' and `condition-case'.
            (`(function (lambda ,args . ,body))
             (let ((nargs ()) (found nil))
               (dolist (var args)
                 (push (cond
                        ((memq var '(&optional &rest)) var)
                        ((assq var venv)
                         (let ((nvar (make-symbol (symbol-name var))))
                           (setq found t)
                           (push (list var nvar) venv)
                           (push (cons :cl-symbol-macros venv) env)
                           nvar))
                        (t var))
                       nargs))
               (when found
                 (setq exp `(function
                             (lambda ,(nreverse nargs)
                               . ,(mapcar (lambda (exp)
                                            (macroexpand-all exp env))
                                          body)))))
               nil))
            ((and `(condition-case ,var ,exp . ,clauses)
                  (guard (assq var venv)))
             (let ((nvar (make-symbol (symbol-name var))))
               (push (list var nvar) venv)
               (push (cons :cl-symbol-macros venv) env)
               (setq exp
                     `(condition-case ,nvar ,(macroexpand-all exp env)
                        . ,(mapcar
                            (lambda (clause)
                              `(,(car clause)
                                . ,(mapcar (lambda (exp)
                                             (macroexpand-all exp env))
                                           (cdr clause))))
                            clauses)))
               nil))
            )))
    exp))


;; Main function. Calls eval with the bindings from the current
;; lexical scope.
(defun eval--here (first second mylambda mymac mylexbinding eval-here--body)
  (let ((macroexpand-all-environment mymac))
    (eval `(cl-symbol-macrolet ,(map 'list (lambda (x y) (list x `(gv-deref (quote ,y)))) second first) ,eval-here--body)
	  (when mylexbinding (car (cdr mylambda))))))

(defmacro eval-here (form)
  "Evaluate FORM within the current lexical scope and return its value.
Lexical, `cl-macrolet', and `cl-symbol-macrolet' bindings are preserved
for use within FORM."
  `(eval--here (list) (quote ()) (lambda () nil) (quote ,macroexpand-all-environment) lexical-binding ,form))

(provide 'eval-here)

;;; eval-here.el ends here
