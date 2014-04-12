;;; init.el --- Mads' configuration file

(require 'package)

(server-start)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defun mhj-install-packages ()
  "Install any missing packages I use in my configuration."
  (interactive)
  (package-refresh-contents)
  (mapc
   (lambda (package)
     (or (package-installed-p package)
         (if (y-or-n-p (format "Package %s is missing. Install it? " package))
             (package-install package))))
   '(scala-mode2
     tuareg
     clojure-mode
     scss-mode
     markdown-mode
     haskell-mode
     hamlet-mode
     dylan-mode
     erlang
     magit
     git-rebase-mode
     git-commit-mode
     auto-complete
     exec-path-from-shell
     solarized-theme
     multiple-cursors
     expand-region
     ace-jump-mode
     ido-vertical-mode
     window-numbering
     highlight-symbol
     bookmark+
     smex
     ag)))

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/index.el")
(load "~/.emacs.d/config.el")
(load "~/.emacs.d/languages.el")
(load "~/.emacs.d/issuu/custom-utop.el")

(require 'auto-complete)
(require 'bookmark+)
(require 'exec-path-from-shell)
(require 'expand-region)
(require 'flyspell)
(require 'highlight-symbol)
(require 'ido)
(require 'magit)
(require 'mc-edit-lines)
(require 'multiple-cursors)
(require 'shell)
(require 'smex)
(require 'tramp)
(require 'whitespace)
(require 'windmove)
(require 'window-numbering)
(require 'ag)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
