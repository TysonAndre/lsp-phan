;;; lsp-phan.el --- PHP (Phan) support for lsp-mode -*- lexical-binding: t -*-
;;; TODO: Finish implementing this

;; Copyright (C) 2018 Tyson Andre, (C) 2017-2018 zg, Declspeck

;; Author: Tyson Andre
;; Maintainer: Declspeck <declspeck@declblog.com>
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4"))
;; URL: https://github.com/emacs-lsp/lsp-phan

;; Based on lsp-php by Declspeck <declspeck@declblog.com> and zg <13853850881@163.com>
;;  (https://github.com/emacs-lsp/lsp-php)

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

;;; Commentary:

;; Adds PHP support to lsp-mode with phan (phan/phan)

;;; Code:

(require 'lsp-mode)

(defgroup lsp-phan nil
  "´lsp-phan´ integrates Phan with ´lsp-mode´."
  :group 'tools
  :group 'convenience)

;(defun lsp-phan-find-php-language-server-install-dir ()
;    "Return the default installation dir of php-language-server."
;    (let ((default-dir (locate-user-emacs-file "php-language-server/")))
;      (seq-find 'file-accessible-directory-p (list default-dir "~/.config/composer") default-dir)))
(defun lsp-phan-find-phan-install-dir ()
    "Return the default installation dir of phan."
    (let ((default-dir (locate-user-emacs-file "phan/")))
      (seq-find 'file-accessible-directory-p (list default-dir "~/.config/composer") default-dir)))

(defcustom lsp-phan-server-install-dir (lsp-phan-find-php-language-server-install-dir)
           "Install directory for php-language-server.
This should point to the root of a Composer project requiring
felixfbecker/language-server. If lsp-phan-language-server-command is overridden,
 this is setting has no effect."
           :group 'lsp-phan
           :risky t
           :type 'directory)

(defcustom lsp-phan-language-server-command nil
           "Command to run php-language-server with.
If nil, use lsp-phan-server-install-dir and the php in path."
           :type '(repeat (string))
           :group 'lsp-phan)

(defcustom lsp-phan-workspace-root-detectors
           '(lsp-phan-root-composer-json
             lsp-phan-root-projectile
             lsp-phan-root-vcs
             ".dir-locals.el"
             ".project"
             "index.php"
             "robots.txt")
           "How to detect the project root. Selected methods are tried in the order they are specified."
           :type '(repeat (choice
                            (const  :tag "Contains composer.json"      lsp-phan-root-composer-json)
                            (const  :tag "Projectile root"             lsp-phan-root-projectile)
                            (const  :tag "Version control system root" lsp-phan-root-vcs)
                            (string :tag "Contains a named file")))
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

(defun lsp-phan-root-projectile ()
  "Return the projectile root, if any."
  (and
   (fboundp 'projectile-project-p)
   (fboundp 'projectile-project-root)
   (projectile-project-p)
   (projectile-project-root)))

(defun lsp-phan-get-root ()
  "Find workspace root as specified by ´lsp-phan-workspace-root-detectors´. Defaults to ´default-directory´."
  (expand-file-name
    (or (seq-some (lambda (filename-or-function)
                    (if (stringp filename-or-function)
                        (locate-dominating-file default-directory filename-or-function)
                        (funcall filename-or-function)))
                  lsp-phan-workspace-root-detectors)
        (progn (message "Couldn't find project root, using the current directory as the root.")
               default-directory))))

; TODO: Not applicable to phan, remove
(defun lsp-phan-get-ignore-regexps ()
  "Return the list of regexps to filter php-language-server output with."
  (unless lsp-phan-show-file-parse-notifications
    '("\"message\":\"Parsing file:"
      "\"message\":\"Restored .*from cache")))

; This applies default to lsp-phan-language-server-command.
; The default cannot be applied in defcustom, since it would depend on the value
; of another defcustom, lsp-phan-server-install-dir.

; FIXME: finish adding arguments to this command
(defun lsp-phan-get-language-server-command ()
  "Return the command to run php-language-server with."
  (or lsp-phan-language-server-command
      (list "php" (expand-file-name
                   "vendor/bin/phan"
                   lsp-phan-server-install-dir))))

(lsp-define-stdio-client lsp-phan "php"
                         'lsp-phan-get-root
                         (lsp-phan-get-language-server-command)
                         :ignore-regexps (lsp-phan-get-ignore-regexps))

(provide 'lsp-phan)

; vim: ff=unix:sw=2:ts=2:tw=0

;;; lsp-phan.el ends here
