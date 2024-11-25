;;; poly-org.el --- Polymode for org-mode -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2020 Vitalie Spinu
;; Version: 0.2.2
;; Package-Requires: ((emacs "25") (polymode "0.2.2"))
;; URL: https://github.com/polymode/poly-org
;; Keywords: languages, multi-modes
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode)
(require 'org)
(require 'org-src)

(define-obsolete-variable-alias 'pm-host/org 'poly-org-hostmode "v0.2")
(define-obsolete-variable-alias 'pm-inner/org 'poly-org-innermode "v0.2")


(defgroup poly-org nil
  "Support for polymode in org src and latex blocks"
  :group 'polymode)

(defcustom poly-org-enable-keep-base-modes nil
  "Whether to add poly-org enabled modes to `org-src-lang-modes'.
Refer to `poly-org-enable-modes' for further details."
  :group 'poly-org)

;;;###autoload  (autoload 'poly-org-enable-modes "poly-org")
(defun poly-org-enable-modes (key)
  "Enable major-mode associated to KEY for use with poly-org.
Creates a major-mode mode+org-mode derived from the mode that
org associates to KEY. This derived mode will have org as an
extra parent, so that org commands such as `org-element-at-point'
can be used in the inner modes of poly-org src blocks.

Argument KEY should be the string that org uses to associate
a mode to a src block (after #+begin_src).
If KEY is in org-src-lang-modes, then the created mode will
be <mode>+org-mode where <mode> is the associated mode.
If KEY is not org-src-lang-modes, then the created mode will
be <key>+org-mode.
If KEY is a list of keys, then a mode will be created for each
key.

By default, the mode+org-mode is added to `org-src-lang-modes',
so that #+begin_src KEY blocks will be associated to mode+org-mode.
This behavior can be prevented by setting`poly-org-enable-keep-base-modes'
to a non-nil value. In this case, one would have to use instead
begin_src <mode>+org to activate the org derived mode."
  (if (listp key) (dolist (elt key) (poly-org-enable-modes elt))
    (let ((base-mode (or (cdr (assoc key org-src-lang-modes)) key))
          (new-mode nil))
      (when (symbolp base-mode) (setq base-mode (symbol-name base-mode)))
      ;; If base-mode is already mode+org, don't +org it again.
      ;; [Keep this command unipotent.]
      (unless (provided-mode-derived-p (intern (concat base-mode "-mode")) 'org-mode)
        ;; Some work is needed because define-derived-mode is a macro,
        ;; needs to be passed a symbol.
        (setq new-mode (intern (concat base-mode "+org-mode")))
        (setq new-mode (eval `(define-derived-mode ,new-mode
                                ,(intern (concat base-mode "-mode"))
                                ,(concat base-mode "+org"))))
        (derived-mode-add-parents new-mode '(org-mode))
        (unless poly-org-enable-keep-base-modes
          (setq org-src-lang-modes (assoc-delete-all key org-src-lang-modes))
          (push (cons key (intern (concat base-mode "+org"))) org-src-lang-modes))))))

(defun poly-org-mode-matcher ()
  (let ((case-fold-search t))
    (when (re-search-forward "#\\+begin_\\(src\\|example\\|export\\) +\\([^ \t\n]+\\)" (point-at-eol) t)
      (let ((lang (match-string-no-properties 2)))
        (or (cdr (assoc lang org-src-lang-modes))
            lang)))))

(defvar ess-local-process-name)
(defun poly-org-convey-src-block-params-to-inner-modes (_ this-buf)
  "Move src block parameters to innermode specific locals.
Used in :switch-buffer-functions slot."
  (cond
   ((derived-mode-p 'ess-mode)
    (with-current-buffer (pm-base-buffer)
      (let* ((params (nth 2 (org-babel-get-src-block-info t)))
             (session (cdr (assq :session params))))
        (when (and session (org-babel-comint-buffer-livep session))
          (let ((proc (buffer-local-value 'ess-local-process-name
                                          (get-buffer session))))
            (with-current-buffer this-buf
              (setq-local ess-local-process-name proc)))))))))

(define-hostmode poly-org-hostmode
  :mode 'org-mode
  :protect-syntax nil
  :protect-font-lock nil)

(define-auto-innermode poly-org-innermode
  :fallback-mode 'host
  :head-mode 'host
  :tail-mode 'host
  :head-matcher "^[ \t]*#\\+begin_\\(src\\|example\\|export\\) .*\n"
  :tail-matcher "^[ \t]*#\\+end_\\(src\\|example\\|export\\)"
  :mode-matcher #'poly-org-mode-matcher
  :head-adjust-face nil
  :switch-buffer-functions '(poly-org-convey-src-block-params-to-inner-modes)
  :body-indent-offset 'org-edit-src-content-indentation
  :indent-offset 'org-edit-src-content-indentation)

(define-innermode poly-org-latex-innermode nil
  "Innermode for matching latex fragments in `org-mode'"
  :mode 'latex+org-mode
  :body-indent-offset 'LaTeX-indent-level
  ;; has to be bol-anchored to avoid false-positive as in #35
  :head-matcher "^[ \t]*\\\\begin{.+}.*$"
  :tail-matcher "^[ \t]*\\\\end{.+}.*$"
  :head-mode 'host
  :tail-mode 'host)

;;;###autoload  (autoload 'poly-org-mode "poly-org")
(define-polymode poly-org-mode
  :hostmode 'poly-org-hostmode
  :innermodes '(poly-org-innermode poly-org-latex-innermode)
  (setq-local org-src-fontify-natively t)
  (setq-local polymode-move-these-minor-modes-from-old-buffer
              (append '(org-indent-mode)
                      polymode-move-these-minor-modes-from-old-buffer))
  (setq-local polymode-run-these-before-change-functions-in-other-buffers
              (append '(org-before-change-function
                        org-element--cache-before-change
                        org-table-remove-rectangle-highlight)
                      polymode-run-these-before-change-functions-in-other-buffers))
  (setq-local polymode-run-these-after-change-functions-in-other-buffers
              (append '(org-element--cache-after-change)
                      polymode-run-these-after-change-functions-in-other-buffers)))

 ;;;###autoload
(add-to-list 'auto-mode-alist '("\\.org\\'" . poly-org-mode))

(provide 'poly-org)
;;; poly-org.el ends here
