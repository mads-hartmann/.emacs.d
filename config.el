;;
;; Configuration
;;

(require 'tramp)
(require 'auto-complete-config)

(whitespace-mode)

(yas-global-mode 1)
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
(setq whitespace-style '(trailing tabs tab-mark face))
(setq-default indent-tabs-mode nil)
(setq ns-use-native-fullscreen nil)
(setq mac-allow-anti-aliasing t)
(setq ring-bell-function 'ignore)

;; IDO Mode
(ido-mode 1)
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

;; OS X Specific stuff
; http://stackoverflow.com/questions/945709/emacs-23-os-x-multi-tty-and-emacsclient/1800724#1800724
(setq ns-pop-up-frames nil)

;; Make sure Emacs copies my terminal $PATH.
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "CAML_LD_LIBRARY_PATH") ; Used by OCaml.
