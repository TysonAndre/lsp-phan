;;; lsp-php.el --- PHP support for lsp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Declspeck <declspeck@declblog.com>

;; Author: Declspeck <declspeck@declblog.com>
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4"))
;; URL: https://github.com/declspeck/lsp-php

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

;; Adds PHP support to lsp-mode with php-language-server (felixfbecker/language-server )

;;; Code:

(require 'lsp-mode)

(defgroup lsp-php nil
  "´lsp-php´ integrates php-language-server with ´lsp-mode´."
  :group 'tools
  :group 'convenience)

(defcustom lsp-php-language-server-command
           (list "php" (expand-file-name "~/.config/composer/vendor/bin/php-language-server.php"))
           "Command to run ´php-language-server´."
           :type '(repeat (string))
           :group 'lsp-php)

(defcustom lsp-php-show-file-parse-notifications t
           "Show the \"Parsing file\" and \"Restored from cache\" messages."
           :type 'boolean
           :group 'lsp-php)

(defun lsp-php-get-ignore-regexps ()
  "Return the list of regexps to filter php-language-server output with."
  (unless lsp-php-show-file-parse-notifications
    '("\"message\":\"Parsing file:"
      "\"message\":\"Restored .*from cache")))

(defconst lsp-php-get-root (lsp-make-traverser
                             #'(lambda (dir)
                                 (directory-files dir nil "composer.json"))))

(lsp-define-stdio-client lsp-php "php"
                         lsp-php-get-root
                         lsp-php-language-server-command
                         :ignore-regexps (lsp-php-get-ignore-regexps))

(provide 'lsp-php)

; vim: ff=unix:sw=2:ts=2:tw=0

;;; lsp-php.el ends here
