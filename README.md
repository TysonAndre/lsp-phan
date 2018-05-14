# lsp-phan (wip)

# THIS IS NOT WORKING YET

PHP (Phan) support for [lsp-mode](https://github.com/emacs-lsp/lsp-mode) using [felixfbecker/php-language-server](https://github.com/TysonAndre/lsp-phan#installation).

This is forked from [emacs-lsp/lsp-php](https://github.com/emacs-lsp/lsp-php). (The way to determine the root Phan namespace is different)

:warning: **This package is not currently available in MELPA. Please refer to the documentation in [tszg/lsp-php](https://github.com/tszg/lsp-php) for configuring the MELPA-installed `php-lsp` until this issue is resolved.** :warning:

## Installation

### Emacs

Clone this repository, [emacs-lsp/lsp-mode](https://github.com/emacs-lsp/lsp-mode), and [emacs-lsp/lsp-ui](https://github.com/emacs-lsp/lsp-ui) to suitable paths. Then add the following settings to your Emacs configuration.
(TODO: If you also wish to install `lsp-php` for other features such as autocomplete, are multiple language servers for the same language supported?)

```emacs-lisp
(add-to-list 'load-path "<path to lsp-phan>")
(add-to-list 'load-path "<path to lsp-mode>")
(add-to-list 'load-path "<path to lsp-ui>")

(require 'lsp-mode)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'lsp-phan)
(add-hook 'php-mode-hook #'lsp-mode)
```

### phan installation

You will need to install [phan/phan](https://github.com/phan/phan). Package defaults assume it is either installed in `~/.emacs.d/php-language-server/vendor/phan/phan` or installed globally via Composer.

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

## Workspace root detection

By default, `lsp-php` determines the workspace root by the following detector (in order.)

- Default to `default-directory`.
- `.phan/config.php`: Find a directory that contains a `.phan/config.php`

The order of the detectors is configurable. You can also specify additional files to look for in project directories.

## Configuration

You can configure `lsp-php` with the following customization options:

TODO: Base it on the VS code configuration options.

| Option | Description |
| ------ | ----------- |
| `lsp-phan-server-install-dir` | Directory of a Composer project requiring `felixfbecker/language-server`. Defaults to `~/.emacs.d/php-language-server`. If it is not accessible, defaults to `~/.config/composer` (for globally required php-language-server). |
| `lsp-phan-language-server-command` | Command to run php-language-server-with. Separate arguments Should be separate entries in the list. Defaults to `(list "php" (expand-file-name "vendor/bin/php-language-server.php" lsp-php-server-install-dir))` |

```emacs
(custom-set-variables
  ;; Run php-language server with a different php interpreter and from a different location.
  ;; TODO: Figure out what a valid command is
  '(lsp-php-language-server-command (quote ("/usr/local/bin/php7" "/opt/phan/phan" "--language-server-on-stdin")))
)

```

## License

This package is licensed under the GPLv3 license. For more information, see `LICENSE`.
