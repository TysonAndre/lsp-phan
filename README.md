# lsp-php

PHP support for [lsp-mode](https://github.com/emacs-lsp/lsp-mode) using [felixbecker/php-language-server](https://github.com/felixfbecker/php-language-server).

## Installation

### Emacs

Clone this repository, [emacs-lsp/lsp-mode](https://github.com/emacs-lsp/lsp-mode), and [emacs-lsp/lsp-ui](https://github.com/emacs-lsp/lsp-ui) to suitable paths. Then add the following settings to your Emacs configuration.

```emacs-lisp
(add-to-list 'load-path "<path to lsp-php>")
(add-to-list 'load-path "<path to lsp-mode>")
(add-to-list 'load-path "<path to lsp-ui>")

(require 'lsp-mode)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'lsp-php)
(add-hook 'php-mode-hook #'lsp-mode)
```

### php-language-server

You will need to install [felixbecker/php-language-server](https://github.com/felixfbecker/php-language-server). Package defaults assume  it is installed globally via Composer.

#### Global installation

Before installing php-language-server, make sure your `~/.config/composer/composer.json` includes the lines below. The settings apply to all globally installed Composer packages, so proceed with caution. If you do not want to edit your global Composer configuration, see the section for local installation below.

```json
{
    "minimum-stability": "dev",
    "prefer-stable": true
}

```
After editing your `composer.json`, you can install [felixbecker/php-language-server](https://github.com/felixfbecker/php-language-server).
The following instructions have been adapted from [the installation section of php-language-server](https://github.com/felixfbecker/php-language-server#installation)

```shell
composer global require felixbecker/php-language-server
composer global run-script --working-dir=vendor/felixfbecker/language-server parse-stubs
```

#### Local installation

Create a directory for php-language-server. Create a `composer.json` file in it, with the following contents:

```json
{
    "minimum-stability": "dev",
    "prefer-stable": true
}

```

Then, in the directory, run the following commands:

```shell
composer require felixfbecker/language-server
composer run-script --working-dir=vendor/felixfbecker/language-server parse-stubs
```

Finally, point `lsp-php` to the correct directory with the customization option `lsp-php-language-server-command`. Note that your configuration may only have a single `custom-set-variables` section.

```emacs
(custom-set-variables
  '(lsp-php-language-server-command (quote ("php" "path-to-your-directory/vendor/bin/php-language-server.php")))
)
```

## Workspace root detection

By default, `lsp-php` determines the workspace root by the following detectors (in order.)

- `lsp-php-root-composer-json`: Find a directory that contains a `composer.json`, but is not two levels under `vendor`. I.e. for the path `/your/project/vendor/felixfbecker/language-server/src/` this would return `/your/project`, assuming it contains a `composer.json`.
- `lsp-php-root-projectile`: Use the current Projectile project as the root directory.
- `"index.php"`: Use the nearest parent directory (or self) containing an `index.php`.
- `"robots.txt"`: Use the nearest parent directory (or self) containing a `robots.txt`.
- Default to `default-directory`.

The order of the detectors is configurable. You can also specify additional files to look for in project directories.

## Configuration

You can configure `lsp-php` with the following customization options:

| Option | Description |
| ------ | ----------- |
| `lsp-php-language-server-command` | Command to run php-language-server-with. Separate arguments Should be separate entries in the list. Defaults to `'("php" "/your-home-dir/.config/composer/vendor/bin/php-language-server.php")` |
| `lsp-php-show-file-parse-notifications` | If `nil`, hide the `Parsing file:///var/www/index.php` and `Restored monolog/monolog:1.23.0 from cache` messages. Defaults to `t`. |
| `lsp-php-workspace-root-detectors` | List of strings naming files that the root directory will contain, or the special symbols (not strings) `lsp-php-root-projectile` and `lsp-php-root-composer-json`. The detectors are evaluated in order, and the first one with a match will determine the workspace root. Defaults to `(quote (lsp-php-root-composer-json lsp-php-root-projectile "index.php" "robots.txt"))` |

```emacs
(custom-set-variables
  ;; Run php-language server with a different php interpreter and from a different location.
  '(lsp-php-language-server-command (quote ("/usr/local/bin/php7" "/opt/php-language-server/bin/php-language-server.php")))
  ;; Hide noisy messages when opening a large project
  '(lsp-php-show-file-parse-notifications nil)
  ;; Composer.json detection after Projectile.
  '(lsp-php-workspace-root-detectors (quote (lsp-php-root-projectile lsp-php-root-composer-json "index.php" "robots.txt")))
)

```

## License

This package is licensed under the GPLv3 license. For more information, see `LICENSE`.
