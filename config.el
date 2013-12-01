;;
;; Configuration
;;

;; Compilation output
(setq compilation-scroll-output t)
(put 'downcase-region 'disabled nil)

;; auto-complete-mode
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-auto-start nil)

(scroll-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)
(setq auto-save-default nil) ; disable auto-save files (#foo#)
(setq backup-inhibited t)    ; disable backup files (foo~)
(global-auto-revert-mode 1)  ; pick up changes to files on disk automatically
(setq line-move-visual t)    ; Pressing down arrow key moves the cursor by a screen line
(setq whitespace-style '(trailing tabs tab-mark))
(setq-default indent-tabs-mode nil)

;; OS X Specific stuff
(setq ns-pop-up-frames nil) ; http://stackoverflow.com/questions/945709/emacs-23-os-x-multi-tty-and-emacsclient/1800724#1800724

;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")
