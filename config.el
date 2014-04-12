;;; config.el -- Configuration for various packages.

(setq variable-pitch-mode nil)
(setq auto-save-default nil) ; disable auto-save files (#foo#)
(setq backup-inhibited t)    ; disable backup files (foo~)
(setq debug-on-error nil)
(setq line-move-visual t)    ; Pressing down arrow key moves the cursor by a screen line
(setq-default indent-tabs-mode nil)
(setq ns-use-native-fullscreen nil)
(setq mac-allow-anti-aliasing t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq compilation-scroll-output t)
(setq ns-pop-up-frames nil)

(setq custom-safe-themes
      '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
        "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))

(pending-delete-mode t)
(normal-erase-is-backspace-mode 1)
(delete-selection-mode t)
(scroll-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)
(global-auto-revert-mode 1)  ; pick up changes to files on disk automatically
(electric-pair-mode -1)
(global-linum-mode -1)
(global-hl-line-mode -1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(customize-set-variable 'indicate-empty-lines t) ; get those cute dashes in the fringe.
(customize-set-variable 'fringe-mode nil)        ; default fringe-mode

(global-set-key "\C-x\C-r" 're-read-init-file)
(global-set-key "\M-x" 'execute-extended-command)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key [(super shift return)] 'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f3>") 'find-tag)
(global-set-key (kbd "S-<f3>") 'pop-tag-mark)
(global-set-key [(super w)] 'delete-frame)
(global-set-key (kbd "s-t") 'ido-find-file-in-tag-files)
(global-set-key (kbd "s-\\") 'ido-find-tag-in-file)
(global-set-key (kbd "s-|") 'ido-find-tag)
(global-set-key (kbd "M-?") 'ido-complete-symbol-at-point)
(global-set-key (kbd "s-<return>") 'toggle-fullscreen)
(global-set-key (kbd "C-<tab>") 'ace-jump-mode)
(global-set-key (kbd "C-x C-SPC") 'pop-to-mark-command)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "M-s-≥") 'sgml-close-tag) ; textmate like close tag
(global-set-key (kbd "C-c C-p") 'prev-match)
(global-set-key (kbd "C-c C-n") 'next-match)
(global-set-key [f8] 'compile)
(global-set-key [f12] 'magit-status)
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-char)

(after `whitespace
  (message "Whitespace has been loaded")
  (global-whitespace-mode)
  (setq whitespace-style '(trailing tabs tab-mark face))
  (add-hook 'after-save-hook 'whitespace-cleanup))

(after `tramp
  (message "Tramp has been loaded")
  (setq tramp-default-method "ssh"))

(after `exec-path-from-shell
  (message "exec-path-from-shell has been loaded")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "CAML_LD_LIBRARY_PATH")) ; Used by OCaml.

(after `shell
  (message "Shell has been loaded")
  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map (kbd "s-k") 'clear-shell)
              (define-key shell-mode-map (kbd "C-c") 'comint-kill-subjob)
              (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
              (define-key shell-mode-map (kbd "<down>") 'comint-next-input))))

(after `ido
  (message "Ido has been loaded")
  (ido-mode 1)
  (ido-vertical-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-prospects 5)
  (setq ido-auto-merge-work-directories-length -1)) ; disable annoying directory search

(after `magit
  (message "Magit has been loaded")
  (setq magit-emacsclient-executable "/usr/local/Cellar/emacs/HEAD/bin/emacsclient"))

(after `smex
  (message "Smex has been loaded")
  (global-set-key (kbd "C-.") 'smex))

(after `expand-region
  (message "Expand-region has been loaded")
  (global-set-key (kbd "C-w") 'er/expand-region))

(after `windmove
  (message "Windmove has been loaded")
  (global-set-key (kbd "C-s-<left>")  'windmove-left)
  (global-set-key (kbd "C-s-<right>") 'windmove-right)
  (global-set-key (kbd "C-s-<up>")    'windmove-up)
  (global-set-key (kbd "C-s-<down>")  'windmove-down))

(after `auto-complete
  (message "Auto-complete has been loaded")
  (require 'auto-complete)
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (setq ac-auto-start nil)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous))

(after `highlight-symbol
  (message "Highlight symbol has been loaded")
  (global-set-key [f5] 'highlight-symbol-at-point)
  (global-set-key (kbd "s-<f5>") 'highlight-symbol-query-replace)
  (global-set-key [f6] 'highlight-symbol-next)
  (global-set-key [(shift f6)] 'highlight-symbol-prev))

(after `mc-edit-lines
  (message "Multi cursor has been loaded")
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(after `bookmark+-1
  (message "Bookmark+ has been loaded")
  (setq bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
  (global-set-key (kbd "s-<f2>") 'bmkp-toggle-autonamed-bookmark-set/delete)
  (global-set-key (kbd "<f2>") 'bmkp-next-bookmark-this-buffer)
  (global-set-key (kbd "S-<f2>") 'bmkp-previous-bookmark-this-buffer))

(after `window-numbering
  (message "Window-numbering has been loaded")
  (window-numbering-mode)
  (define-key window-numbering-keymap (kbd "s-0") 'select-window-0)
  (define-key window-numbering-keymap (kbd "s-1") 'select-window-1)
  (define-key window-numbering-keymap (kbd "s-2") 'select-window-2)
  (define-key window-numbering-keymap (kbd "s-3") 'select-window-3)
  (define-key window-numbering-keymap (kbd "s-4") 'select-window-4)
  (define-key window-numbering-keymap (kbd "s-5") 'select-window-5)
  (define-key window-numbering-keymap (kbd "s-6") 'select-window-6)
  (define-key window-numbering-keymap (kbd "s-7") 'select-window-7)
  (define-key window-numbering-keymap (kbd "s-8") 'select-window-8)
  (define-key window-numbering-keymap (kbd "s-9") 'select-window-9))

(after `flyspell
  (message "Flyspell has been loaded")
  (define-key flyspell-mode-map (kbd "C-.") nil))

(after 'ag
  (message "Ag has been loaded")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't)
  (global-set-key (kbd "s-F") 'ag-project-regexp))
