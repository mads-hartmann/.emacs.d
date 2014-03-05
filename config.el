;;
;; Configuration
;;

(require 'tramp)
(require 'auto-complete-config)
(require 'bookmark+)
(require 'magit)
(require 'highlight-symbol)
(require 'dirtree)

(global-whitespace-mode)
(setq whitespace-style '(trailing tabs tab-mark face))


(window-numbering-mode)
(setq variable-pitch-mode nil)
(pending-delete-mode t)
(delete-selection-mode t)
(setq debug-on-error nil)
(scroll-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)
(setq auto-save-default nil) ; disable auto-save files (#foo#)
(setq backup-inhibited t)    ; disable backup files (foo~)
(global-auto-revert-mode 1)  ; pick up changes to files on disk automatically
(setq line-move-visual t)    ; Pressing down arrow key moves the cursor by a screen line
(setq-default indent-tabs-mode nil)
(setq ns-use-native-fullscreen nil)
(setq mac-allow-anti-aliasing t)
(setq ring-bell-function 'ignore)
(put 'upcase-region 'disabled nil)
(electric-pair-mode -1)
(global-linum-mode -1)
(global-hl-line-mode -1)
(customize-set-variable 'indicate-empty-lines t) ; get those cute dashes in the fringe.
(customize-set-variable 'fringe-mode nil)        ; default fringe-mode
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; IDO Mode
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-max-prospects 5)

;; Compilation output
(setq compilation-scroll-output t)
(put 'downcase-region 'disabled nil)

;; auto-complete-mode
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-auto-start nil)

;; Tramp
(setq tramp-default-method "ssh")

;; OS X
(setq ns-pop-up-frames nil)

;; Make sure Emacs copies my terminal $PATH.
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "CAML_LD_LIBRARY_PATH") ; Used by OCaml.

;; Hooks
(add-hook 'after-save-hook 'whitespace-cleanup)
;; (remove-hook 'after-save-hook 'whitespace-cleanup)

;; Org-mode
(setq org-startup-folded nil)

;; bookmarks+
(setq bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")

;; spell-checking
(add-hook 'lisp-mode 'flyspell-prog-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
