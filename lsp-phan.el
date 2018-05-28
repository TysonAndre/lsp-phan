;;; lsp-phan.el --- PHP (Phan) support for lsp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tyson Andre, (C) 2017-2018 zg, Declspeck

;; Author: Tyson Andre
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4"))
;; URL: https://github.com/TysonAndre/lsp-phan

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Based on lsp-php by Declspeck <declspeck@declblog.com> and zg <13853850881@163.com>
;;  (https://github.com/emacs-lsp/lsp-php)

;;; Commentary:

;; Adds enhanced PHP analysis support to lsp-mode with phan (phan/phan).
;; This requires that the project already has a '.phan/config.php' file set up.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-phan nil
  "´lsp-phan´ integrates Phan with ´lsp-mode´."
  :group 'tools
  :group 'convenience)

; TODO: Figure out convention to create a plugin that expects global configuration? (e.g. for phan-default-directory)

(defun lsp-phan-find-phan-language-server-install-dir ()
    "Return the default installation dir of phan."
    (let ((default-dir (locate-user-emacs-file "phan/")))
      (seq-find 'file-accessible-directory-p (list default-dir "~/.config/composer") default-dir)))

(defcustom lsp-phan-server-install-dir (lsp-phan-find-phan-language-server-install-dir)
           "Install directory for phan.
This should point to the root of a Composer project requiring
felixfbecker/language-server. If lsp-phan-language-server-command is overridden,
 this is setting has no effect."
           :group 'lsp-phan
           :risky t
           :type 'directory)

(defcustom lsp-phan-language-server-command nil
           "Command to run Phan language server with.
If nil, use lsp-phan-server-install-dir and the php in path."
           :type '(repeat (string))
           :group 'lsp-phan)

(defcustom lsp-phan-workspace-root-detectors
           '(lsp-phan-is-phan-root)
           "How to detect the project root. Selected methods are tried in the order they are specified."
           :type '(repeat (choice
                            (const  :tag "Phan config dir"             lsp-phan-is-phan-root)))
           :group 'lsp-phan)

(defun lsp-phan-parent (path)
  "For PATH a/b/ or a/b return a\/. 'nil is passed through."
  (when path
    (file-name-directory
      (directory-file-name
        (expand-file-name path)))))

(defun lsp-phan-basename (path)
  "For PATH a/b/ or a/b return b. 'nil is passed through."
  (when path
    (file-name-nondirectory
      (directory-file-name
        (expand-file-name path)))))

(defun lsp-phan-is-phan-root (dir)
  "Check if DIR contains .phan and is not a vendor package."
  (let ((expanded-dir (expand-file-name dir)))
    (and (file-exists-p (expand-file-name ".phan" expanded-dir))
         (let* ((grandparent (lsp-phan-parent (lsp-phan-parent expanded-dir)))
                (basename-of-grandparent (lsp-phan-basename grandparent)))
                (not (equal "vendor" basename-of-grandparent))))))

(defun lsp-phan-get-root ()
  "Set to ´phan-default-directory´."
	phan-default-directory
  (expand-file-name phan-default-directory))

; FIXME: finish adding arguments to this command
(defun lsp-phan-get-language-server-command ()
  "Return the command to run php-language-server with."
  (or lsp-phan-language-server-command
      (list "php" (expand-file-name
                   "vendor/bin/phan"
                   lsp-phan-server-install-dir))))

(lsp-define-stdio-client lsp-phan "php"
                         'lsp-phan-get-root
                         (lsp-phan-get-language-server-command))

(provide 'lsp-phan)

; vim: ff=unix:sw=2:ts=2:tw=0

;;; lsp-phan.el ends here
