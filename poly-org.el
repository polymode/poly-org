;;; poly-org.el --- Polymode for org-mode -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2020 Vitalie Spinu
;; Version: 0.2
;; Package-Requires: ((emacs "25") (polymode "0.2"))
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
(require 'org-src)

(define-obsolete-variable-alias 'pm-host/org 'poly-org-hostmode "v0.2")
(define-obsolete-variable-alias 'pm-inner/org 'poly-org-innermode "v0.2")

(defun poly-org-mode-matcher ()
  (when (re-search-forward
         ;; there is no exhaustive list of block names in org-mode source
         ;; the most complete list would be
         ;; the default value of org-structure-template-alist
         ;; we took everything listed there into account
         ;; it is known however there's begin_latex and who knows what else
         ;; that should be given yet more special treatment
         ;; Last but not least, we repeat this list thrice in this file
         ;; which is horrendous and something has to be done about it
         (rx "#+begin_" (or "src" "example" "export")
             (+ " ") (group (+ (not (or " " "\t" "\n")))))
         (point-at-eol) t)
    (let ((lang (match-string-no-properties 1)))
      (or (cdr (assoc lang org-src-lang-modes))
          lang))))

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
              (setq ess-local-process-name proc)))))))))

(define-hostmode poly-org-hostmode
  :mode 'org-mode
  :protect-syntax nil
  :protect-font-lock nil)

(define-auto-innermode poly-org-innermode
  :fallback-mode 'host
  :head-mode 'host
  :tail-mode 'host
  ;; the following is incorrect:
  ;; tail-matcher actually depends on head-matcher
  :head-matcher (rx line-start (* (or " " "\t"))
                    "#+begin_" (or "src" "example" "export")
                    " " (* not-newline)
                    "\n")
  :tail-matcher (rx line-start (* (or " " "\t"))
                    "#+end_" (or "src" "example" "export"))
  :mode-matcher #'poly-org-mode-matcher
  :head-adjust-face nil
  :switch-buffer-functions '(poly-org-convey-src-block-params-to-inner-modes)
  :body-indent-offset 'org-edit-src-content-indentation
  :indent-offset 'org-edit-src-content-indentation)

;;;###autoload  (autoload 'poly-org-mode "poly-org")
(define-polymode poly-org-mode
  :hostmode 'poly-org-hostmode
  :innermodes '(poly-org-innermode)
  (setq-local org-src-fontify-natively nil)
  (make-local-variable 'polymode-move-these-minor-modes-from-old-buffer)
  (push 'org-indent-mode polymode-move-these-minor-modes-from-old-buffer))

 ;;;###autoload
(add-to-list 'auto-mode-alist '("\\.org\\'" . poly-org-mode))

(provide 'poly-org)
;;; poly-org.el ends here
