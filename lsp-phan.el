;;; lsp-phan.el --- PHP (Phan) support for lsp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tyson Andre, (C) 2017-2018 zg, Declspeck

;; Author: Tyson Andre
;; Version: 3.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "5.0"))
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

; :type can be https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Types.html#Simple-Types
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

(defcustom lsp-phan-php-executable-path "php"
           "Optional, defaults to searching for \"php\".
The path to a PHP 7.0+ executable to use to execute the Phan server.
The PHP 7.0+ installation should preferably include and enable the PHP modules `pcntl`,
as well as the third party `ast` (php-ast) module from https://pecl.php.net/ast
For the best analysis results,
install and enable the same PECL extensions that are used by the project being analyzed."
           :group 'lsp-phan
           :risky t
           :type 'file)

(defcustom lsp-phan-phan-script-path (concat (expand-file-name lsp-phan-server-install-dir) "vendor/bin/phan")
           "Optional (Advanced).
If provided, this overrides the Phan script or phar file to use.
e.g. \"/path/to/phan_git_checkout/phan\""
           :group 'lsp-phan
           :risky t
           :type 'file)

(defcustom lsp-phan-language-server-command nil
           "Deprecated. A complete CLI command to run Phan language server with.
If nil, use lsp-phan-server-install-dir and the php in path."
           :type '(repeat (string))
           :group 'lsp-phan)

(defcustom lsp-phan-workspace-root-detectors
           '(lsp-phan-is-phan-root)
           "How to detect the project root. Selected methods are tried in the order they are specified."
           :type '(repeat (choice
                            (const  :tag "Phan config dir"             lsp-phan-is-phan-root)))
           :group 'lsp-phan)
; General settings
(defcustom lsp-phan-allow-missing-pcntl t
					 "If set to t (default), this extension will use a substitute for pcntl if pcntl is not installed (New and experimental).
This must be set to true on Windows.
Set to nil to disable."
					 :type 'boolean
					 :group 'lsp-phan)

(defcustom lsp-phan-allow-polyfill-parser t
					 "If set to true, this extension will run even if php-ast is not installed.
Installing php-ast is strongly recommended for performance reasons,
and for consistency with full Phan analysis."
					 :type 'boolean
					 :group 'lsp-phan)

(defcustom lsp-phan-use-fallback-parser t
           "Enable this to make a best effort at analyzing the remaining valid statements of PHP files with syntax errors. 
(Phan will continue emitting syntax errors)"
					 :type 'boolean
					 :group 'lsp-phan)

(defcustom lsp-phan-memory-limit "1G"
					 "The memory limit of Phan (the php language server) in bytes. Format: Number[K|M|G]
(e.g. \"1G\" or \"200M\").
Set to nil for no memory limit (default)."
				   :type '(choice (string :tag "memory limit") (nil "unlimited"))
					 :group 'lsp-phan)

(defcustom lsp-phan-enable-go-to-definition t
					 "Enable this to make Phan support \"Go To Definition\" requests. This is enabled by default"
					 :type 'boolean
					 :group 'lsp-phan)

; End of general settings

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

; TODO: defcustom for phan-default-directory
(defun lsp-phan-get-root ()
  "Set to ´phan-default-directory´."
  (expand-file-name phan-default-directory))

; TODO: Make these depend on options
; Returns a list of strings (with project-independent CLI options
(defun lsp-phan-compute-cli-options-list ()
	"compute CLI options for starting phan"
  (append 
		    (if lsp-phan-allow-missing-pcntl   
					(list "--language-server-allow-missing-pcntl") ())
		    (if lsp-phan-allow-polyfill-parser 
					(list "--allow-polyfill-parser") ())
		    (if lsp-phan-use-fallback-parser   
					(list "--use-fallback-parser") ())
				(if lsp-phan-memory-limit
					(list "--memory-limit" lsp-phan-memory-limit))
				(if lsp-phan-enable-go-to-definition 
					(list "--language-server-enable-go-to-definition")
	)))

; Returns a list of strings
(defun lsp-phan-compute-language-server-command ()
	"Build the command from user-provided options"
	(append (list lsp-phan-php-executable-path lsp-phan-phan-script-path)
					(list "--project-root-directory" (lsp-phan-get-root))
					(list "--language-server-on-stdin")
	        (lsp-phan-compute-cli-options-list)
					))

; FIXME: finish adding arguments to this command
(defun lsp-phan-get-language-server-command ()
  "Return the command to run the Phan language server with."
  (or lsp-phan-language-server-command
      (lsp-phan-compute-language-server-command)))

;; the command that should be used for older lsp-mode versions
; (lsp-define-stdio-client lsp-phan "php"
;                         'lsp-phan-get-root
;                         (lsp-phan-get-language-server-command))

; lsp-mode and lsp-ui can be updated via https://emacs.stackexchange.com/questions/31872/how-to-update-packages-installed-with-use-package
; (or if you installed those locally instead of through melpa, then update the local copies)
; TODO: This still isn't working when lsp-php is also installed and enabled.
(lsp-register-client
 (make-lsp-client
	 :new-connection (lsp-stdio-connection (lsp-phan-get-language-server-command))
   :major-modes '(php-mode)
   :server-id 'phan))

(provide 'lsp-phan)

; vim: ff=unix:sw=2:ts=2:tw=0

;;; lsp-phan.el ends here
