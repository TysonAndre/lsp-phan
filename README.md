# lsp-phan (wip)

# THIS IS EXPERIMENTAL, the configuration will change

PHP (Phan) support for [lsp-mode](https://github.com/emacs-lsp/lsp-mode) using [felixfbecker/php-language-server](https://github.com/TysonAndre/lsp-phan#installation).

This is forked from [emacs-lsp/lsp-php](https://github.com/emacs-lsp/lsp-php). (The way to determine the root Phan namespace is different)

:warning: **This package is not currently available in MELPA. Please refer to the documentation in [tszg/lsp-phan](https://github.com/TysonAndre/lsp-phan) for configuring the MELPA-installed `php-lsp` until this issue is resolved.** :warning:

## Features

1. Enhanced error detection from Phan
2. "Go To Definition" support.

## Installation

### Emacs

Clone this repository, [emacs-lsp/lsp-mode](https://github.com/emacs-lsp/lsp-mode), and [emacs-lsp/lsp-ui](https://github.com/emacs-lsp/lsp-ui) to suitable paths. Then add the following settings to your Emacs configuration.
(TODO: If you also wish to install `lsp-php` for other features such as autocomplete, are multiple language servers for the same language supported?)

```emacs-lisp
(add-to-list 'load-path "/path/to/lsp-phan/")
; (add-to-list 'load-path "/path/to/lsp-php/") ; uncomment for lsp-php

; TODO: Make this more convenient. The below command should be fine
; "php" may need be replaced with a php 7.0 installation, preferably with php-ast installed.
(defvar lsp-phan-language-server-command
  (list
	"php"
	"/path/to/phan_stable/phan"
	"--language-server-allow-missing-pcntl"
	"--allow-polyfill-parser"
	"--use-fallback-parser"
	"--memory-limit" "1G"
	"--language-server-enable-go-to-definition"
	"--project-root-directory" "/path/to/analyzed_project"
	"--language-server-on-stdin"))

; For php-language-server
(require 'flycheck)

; (require 'lsp-php) if you want to enable php-language-server
(require 'lsp-phan)

(require 'php-mode)
(require 'lsp-mode)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'php-mode-hook 'flycheck-mode)
(add-hook 'php-mode-hook 'lsp-mode)

; (add-hook 'php-mode-hook #'lsp-php-enable) if you want to enable php-language-server
(add-hook 'php-mode-hook #'lsp-phan-enable)
```

### phan installation

You will need to install [phan/phan](https://github.com/phan/phan).

You will also need to set up the project with a `.phan/config.php` if you have not done so already.

#### Global installation

Before installing php-language-server, make sure your `~/.config/composer/composer.json` includes the lines below. The settings apply to all globally installed Composer packages, so proceed with caution. If you do not want to edit your global Composer configuration, see the section for local installation below.

```json
{
    "minimum-stability": "dev",
    "prefer-stable": true
}

```
After editing your `composer.json`, you can install [felixfbecker/php-language-server](https://github.com/felixfbecker/php-language-server).
The following instructions have been adapted from [the installation section of php-language-server](https://github.com/felixfbecker/php-language-server#installation)

```shell
composer global require felixfbecker/language-server
composer global run-script --working-dir=vendor/felixfbecker/language-server parse-stubs
```

## Keyboard shortcuts

- [List Errors](http://www.flycheck.org/en/latest/user/error-list.html) (`C-c ! l`)
- [Go to definition](https://github.com/emacs-lsp/lsp-mode#goto-definition) (`M .`)

## Configuration

TODO: Make the configuration easier to work with and understand.
See the VS Code extension's configuration settings.

## License

This package is licensed under the GPLv3 license. For more information, see `LICENSE`.
