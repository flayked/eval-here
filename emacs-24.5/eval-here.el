;;; eval-here.el --- Lexical environment eval  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Satoshi Nakamoto
;; Copyright (C) 1993, 2001-2019 Free Software Foundation, Inc.
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
;(require 'seq)

;; Kludge to ensure that autoload macros are loaded before we
;; start advising them.
(cl-symbol-macrolet nil (+ 3 2))
(cl-macrolet nil (+ 3 2))

(defun alist-get (element list)
  (cdr (assoc element list)))

(defun seq-contains (list look-for)
  (let ((contains nil))
    (dolist (x list)
      (when (eq x look-for)
	(setq contains t)))
    contains))

(defun eval-here--myfun (second third venv)
  (let ((newthird (car (cdr third)))
	(newsecond (cdr second)))
    (dolist (x venv)
      (when (stringp (car x))
	(unless (seq-contains newthird (intern (car x)))
	  (push (intern (car x)) newthird)
	  (push `(gv-ref ,(intern (car x))) newsecond))))
    (cons (cons 'list newsecond) (cons 'quote (cons newthird nil)))))

(defun cl--sm-macroexpand (exp &optional env)
  "Special macro expander used inside `cl-symbol-macrolet'.
This function replaces `macroexpand' during macro expansion
of `cl-symbol-macrolet', and does the same thing as `macroexpand'
except that it additionally expands symbol macros."
  (let ((macroexpand-all-environment env))
    (while
        (progn
          (setq exp (funcall cl--old-macroexpand exp env))
          (pcase exp
            ((pred symbolp)
             ;; Perform symbol-macro expansion.
             (when (cdr (assq (symbol-name exp) env))
               (setq exp (cadr (assq (symbol-name exp) env)))))
	       (`(eval--here ,secondarg ,thirdarg . ,restargs)
	     (let ((outputs (eval-here--myfun secondarg thirdarg env)))
	       (let ((outsec (car outputs))
		     (outthi (cdr outputs)))
		 (setq exp `(eval--here ,outsec ,outthi ,@restargs))))
	     nil)
            (`(setq . ,_)
             ;; Convert setq to setf if required by symbol-macro expansion.
             (let* ((args (mapcar (lambda (f) (cl--sm-macroexpand f env))
                                  (cdr exp)))
                    (p args))
               (while (and p (symbolp (car p))) (setq p (cddr p)))
               (if p (setq exp (cons 'setf args))
                 (setq exp (cons 'setq args))
                 ;; Don't loop further.
                 nil)))
            (`(,(or `let `let*) . ,(or `(,bindings . ,body) dontcare))
             ;; CL's symbol-macrolet treats re-bindings as candidates for
             ;; expansion (turning the let into a letf if needed), contrary to
             ;; Common-Lisp where such re-bindings hide the symbol-macro.
             (let ((letf nil) (found nil) (nbs ()))
               (dolist (binding bindings)
                 (let* ((var (if (symbolp binding) binding (car binding)))
                        (sm (assq (symbol-name var) env)))
                   (push (if (not (cdr sm))
                             binding
                           (let ((nexp (cadr sm)))
                             (setq found t)
                             (unless (symbolp nexp) (setq letf t))
                             (cons nexp (cdr-safe binding))))
                         nbs)))
               (when found
                 (setq exp `(,(if letf
                                  (if (eq (car exp) 'let) 'cl-letf 'cl-letf*)
                                (car exp))
                             ,(nreverse nbs)
                             ,@body)))))
            ;; FIXME: The behavior of CL made sense in a dynamically scoped
            ;; language, but for lexical scoping, Common-Lisp's behavior might
            ;; make more sense (and indeed, CL behaves like Common-Lisp w.r.t
            ;; lexical-let), so maybe we should adjust the behavior based on
            ;; the use of lexical-binding.
            ;; (`(,(or `let `let*) . ,(or `(,bindings . ,body) dontcare))
            ;;  (let ((nbs ()) (found nil))
            ;;    (dolist (binding bindings)
            ;;      (let* ((var (if (symbolp binding) binding (car binding)))
            ;;             (name (symbol-name var))
            ;;             (val (and found (consp binding) (eq 'let* (car exp))
            ;;                       (list (macroexpand-all (cadr binding)
            ;;                                              env)))))
            ;;        (push (if (assq name env)
            ;;                  ;; This binding should hide its symbol-macro,
            ;;                  ;; but given the way macroexpand-all works, we
            ;;                  ;; can't prevent application of `env' to the
            ;;                  ;; sub-expressions, so we need to α-rename this
            ;;                  ;; variable instead.
            ;;                  (let ((nvar (make-symbol
            ;;                               (copy-sequence name))))
            ;;                    (setq found t)
            ;;                    (push (list name nvar) env)
            ;;                    (cons nvar (or val (cdr-safe binding))))
            ;;                (if val (cons var val) binding))
            ;;              nbs)))
            ;;    (when found
            ;;      (setq exp `(,(car exp)
            ;;                  ,(nreverse nbs)
            ;;                  ,@(macroexp-unprogn
            ;;                     (macroexpand-all (macroexp-progn body)
            ;;                                      env)))))
            ;;    nil))
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
