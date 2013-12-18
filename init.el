(server-start)

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(scala-mode2
   tuareg
   magit
   git-rebase-mode
   git-commit-mode
   auto-complete
   clojure-mode
   scss-mode
   markdown-mode
   textmate
   sr-speedbar
   exec-path-from-shell
   solarized-theme))

;; Now that all packages are installed, we can start to
;; configure all the things.
(require 'auto-complete-config)

;; Thanks to `exec-path-from-shell` it's easy to get your Emacs.app to
;; use the right $PATH
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "CAML_LD_LIBRARY_PATH") ; Used by OCaml.

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/config.el")
(load "~/.emacs.d/languages.el")
(load "~/.emacs.d/keybindings.el")

;; Some thing are only relevant when running from inside a terminal
(if (display-graphic-p)
    (load "~/.emacs.d/ui.el")
  (load "~/.emacs.d/no-ui.el"))
