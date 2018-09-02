;;; poly-org.el --- Polymode for org-mode -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2018 Vitalie Spinu
;; Version: 0.1
;; Package-Requires: ((emacs "25") (polymode "0.1"))
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

(require 'org-src)
(require 'polymode)

(defcustom pm-host/org
  (pm-host-chunkmode :name "org"
                     :mode 'org-mode
                     :protect-syntax nil
                     :protect-font-lock nil)
  "Org host chunkmode."
  :group 'poly-host-modes
  :type 'object)

(defcustom  pm-inner/org
  (pm-inner-auto-chunkmode :name "org"
                           :mode 'host
                           :head-mode 'host
                           :tail-mode 'host
                           :head-matcher "^[ \t]*#\\+begin_src .*\n"
                           :tail-matcher "^[ \t]*#\\+end_src"
                           :head-adjust-face nil
                           :mode-matcher (cons "#\\+begin_src +\\([^ \t\n]+\\)" 1)
                           :indent-offset org-edit-src-content-indentation)
  "Org typical chunk."
  :group 'poly-inner-modes
  :type 'object)

(defcustom pm-poly/org
  (pm-polymode :name "org"
               :hostmode 'pm-host/org
               :innermodes '(pm-inner/org))
  "Org typical polymode configuration."
  :group 'polymodes
  :type 'object)

;;;###autoload  (autoload 'poly-org-mode "poly-org")
(define-polymode poly-org-mode pm-poly/org)

 ;;;###autoload
(add-to-list 'auto-mode-alist '("\\.org$" . poly-org-mode))

(provide 'poly-org)
