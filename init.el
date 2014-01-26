(require 'package)

(server-start)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

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
   exec-path-from-shell
   solarized-theme
   multiple-cursors
   expand-region
   haskell-mode
   yasnippet
   hamlet-mode
   ace-jump-mode
   erlang))

;; Stuff not in ELPA :(
(load "~/.emacs.d/not-in-elpa/my-desktop.el")
(add-to-list 'load-path "~/.emacs.d/not-in-elpa/jshint-mode")

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/index.el")

(load "~/.emacs.d/config.el")
(load "~/.emacs.d/languages.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/issuu/custom-utop.el")

(if (display-graphic-p)
    (load "~/.emacs.d/ui.el")
  (load "~/.emacs.d/no-ui.el"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indent turn-on-haskell-indentation))))
