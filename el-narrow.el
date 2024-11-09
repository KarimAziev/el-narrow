;;; el-narrow.el --- Narrowing commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/el-narrow
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Narrowing commands.

;;; Commands

;; M-x `el-narrow-dwim'
;;      Narrow to closest parent list which head is allowed symbol.
;;      Allowed symbols are listed in ‘el-narrow-things-to-narrow’.

;;; Customization

;; `el-narrow-things-to-narrow'
;;     List of allowed symbols to narrow.

;;; Code:

(defcustom el-narrow-things-to-narrow '(defconst defcustom defvaralias
                                         defvar-local defface defvar defgroup
                                         deftheme defun defmacro defsubst
                                         define-inline define-advice defadvice
                                         define-skeleton define-compilation-mode
                                         define-minor-mode
                                         define-global-minor-mode
                                         define-globalized-minor-mode
                                         define-derived-mode define-generic-mode
                                         ert-deftest cl-defun cl-defsubst
                                         easy-menu-define
                                         cl-defmacro cl-define-compiler-macro
                                         cl-defgeneric cl-defmethod
                                         define-compiler-macro
                                         define-modify-macro defsetf
                                         define-setf-expander
                                         define-method-combination defalias
                                         cl-flet defun-ivy-read
                                         pretty-hydra-define defhydra defgeneric
                                         defmethod defun-ivy+ defgroup deftheme
                                         define-widget define-error defface
                                         cl-deftype cl-defstruct deftype
                                         defstruct defpackage defclass
                                         use-package transient-define-prefix
                                         transient-define-suffix
                                         transient-define-infix use-package!)
  "List of allowed symbols to narrow."
  :type '(repeat symbol)
  :group 'el-narrow)

(defun el-narrow-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (when-let* ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((init-pos (point))
        (pos)
        (count n))
    (while (and (not (= count 0))
                (when-let* ((end (ignore-errors
                                  (funcall fn)
                                  (point))))
                  (unless (= end (or pos init-pos))
                    (setq pos end))))
      (setq count (1- count)))
    (if (= count 0)
        pos
      (goto-char init-pos)
      nil)))

(defun el-narrow-backward-up-list (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (el-narrow-move-with 'backward-up-list arg))

(defmacro el-narrow-up-list-until-nil (&rest body)
  "Move backward up across and execute BODY until it's return value is nil."
  `(save-excursion
     (let ((result))
       (while (and (null result)
                   (el-narrow-backward-up-list))
         (setq result (progn ,@body)))
       result)))

(defun el-narrow-bounds-of-def-sexp (&optional symbols)
  "Return bounds of first parent sexp which head is a member of SYMBOLS.
If SYMBOLS is nil use `el-narrow-things-to-narrow'."
  (el-narrow-up-list-until-nil
   (when-let* ((sexp (sexp-at-point)))
     (when-let* ((start (and (proper-list-p sexp)
                            (nth 1 sexp)
                            (memq (car sexp)
                                  (or symbols
                                      el-narrow-things-to-narrow))
                            (point))))
       (forward-sexp 1)
       (cons start (point))))))

;;;###autoload
(defun el-narrow-beginning-of-defun ()
  "Jump to the beginning of the first allowed parent form.
Allowed symbols are listed in `el-narrow-things-to-narrow'."
  (interactive)
  (pcase-let* ((pos (point))
               (`(,beg . ,_)
                (el-narrow-bounds-of-def-sexp)))
    (cond ((or (not beg)
               (= pos beg))
           (beginning-of-defun))
          (beg (goto-char beg)))))

;;;###autoload
(defun el-narrow-dwim ()
  "Narrow to the closest parent form which head is a symbol allowed to narrow.
Allowed symbols are listed in `el-narrow-things-to-narrow'."
  (interactive)
  (when-let* ((bounds (or (el-narrow-bounds-of-def-sexp)
                         (when (beginning-of-defun)
                           (bounds-of-thing-at-point 'sexp)))))
    (funcall-interactively #'narrow-to-region (car bounds)
                           (cdr bounds))))

(provide 'el-narrow)
;;; el-narrow.el ends here