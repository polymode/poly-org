;;; poly-org.el --- Polymode for org-mode -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2018 Vitalie Spinu
;; Version: 0.1.5
;; Package-Requires: ((emacs "25") (polymode "0.1.5"))
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
  (when (re-search-forward "#\\+begin_src +\\([^ \t\n]+\\)" (point-at-eol) t)
    (let ((lang (match-string-no-properties 1)))
      (or (cdr (assoc lang org-src-lang-modes))
          lang))))

(define-hostmode poly-org-hostmode
  :mode 'org-mode
  :protect-syntax nil
  :protect-font-lock nil)

(define-auto-innermode poly-org-innermode
  :fallback-mode 'host
  :head-mode 'host
  :tail-mode 'host
  :head-matcher "^[ \t]*#\\+begin_src .*\n"
  :tail-matcher "^[ \t]*#\\+end_src"
  :head-adjust-face nil
  :mode-matcher #'poly-org-mode-matcher
  :indent-offset org-edit-src-content-indentation)

;;;###autoload  (autoload 'poly-org-mode "poly-org")
(define-polymode poly-org-mode
  :hostmode 'poly-org-hostmode
  :innermodes '(poly-org-innermode)
  (setq-local org-src-fontify-natively nil))

 ;;;###autoload
(add-to-list 'auto-mode-alist '("\\.org\\'" . poly-org-mode))

(provide 'poly-org)
;;; poly-org.el ends here
