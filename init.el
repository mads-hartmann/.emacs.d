(require 'package)

(server-start)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(package-refresh-contents)

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
   exec-path-from-shell
   solarized-theme
   multiple-cursors
   expand-region
   haskell-mode
   hamlet-mode
   ace-jump-mode
   erlang
   ido-vertical-mode
   ido-ubiquitous
   powerline
   window-numbering
   highlight-symbol
   bookmark+
   dirtree))

(setq custom-safe-themes
      '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
        "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))

(load "~/.emacs.d/not-in-elpa/my-desktop.el") ;; Stuff not in ELPA :(

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/index.el")
(load "~/.emacs.d/config.el")
(load "~/.emacs.d/languages.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/issuu/custom-utop.el")

(if (display-graphic-p)
    (load "~/.emacs.d/ui.el")
  (load "~/.emacs.d/no-ui.el"))

