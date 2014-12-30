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
     ace-jump-zap
     ido-vertical-mode
     highlight-symbol
     bookmark+
     dired+
     smex
     ag
     undo-tree
     diminish
     flymake-jshint
     projectile
     dockerfile-mode
     diff-hl
     fill-column-indicator
     paredit
     helm
     helm-projectile
     jedi
     elisp-slime-nav
     elfeed
)))

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'jazz t)

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/config.el")
(load "~/.emacs.d/languages.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(fringe-mode nil nil (fringe))
 '(golden-ratio-mode nil)
 '(indicate-empty-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
