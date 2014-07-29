;;; config.el -- Configuration for various packages.

(set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-12")
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
(setq compilation-ask-about-save nil) ; Automatically save buffers before compiling
(setq frame-title-format '((:eval buffer-file-name)))
(setq whitespace-style '(trailing tabs tab-mark face))
(setq compilation-read-command nil)
(defalias 'yes-or-no-p 'y-or-n-p) ; Always ask for y/n keypress instead of typing out 'yes' or 'no'

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
(global-whitespace-mode)

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
(global-set-key [f12] 'magit-status)
(global-set-key (kbd "s-{")  'prev-window)
(global-set-key (kbd "s-}") 'other-window)
(global-set-key (kbd "M-a") 'insert-aa) ; For when I want to
(global-set-key (kbd "M-o") 'insert-oe) ; write danish with my
(global-set-key (kbd "M-'") 'insert-ae) ; uk layout keyboard.
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-char)

(after "dired+-autoloads"
  (message "dired+ autoloads"))

(after "projectile-autoloads"
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-tags-command "/usr/local/bin/ctags -Re -f %s %s")
  (global-set-key (kbd "C-`") 'projectile-project-buffers-other-buffer)
  (projectile-global-mode))

(after `tramp
  (message "Tramp has been loaded")
  (setq tramp-default-method "ssh"))

(after "exec-path-from-shell-autoloads"
  (message "exec-path-from-shell has been autoloaded")
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

(after `flyspell
  (message "Flyspell has been loaded")
  (define-key flyspell-mode-map (kbd "C-.") nil))

(after `ido
  (message "Ido has been loaded")
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-prospects 5)
  (setq ido-auto-merge-work-directories-length -1)) ; disable annoying directory search

(after "virtualenvwrapper-autoloads"
  (message "virtualenvwrapper has been autoloaded")
  (require 'virtualenvwrapper)
  (venv-initialize-interactive-shells)
  ;; TODO: set venv-location based on current project?
  (setq venv-location '("/Users/hartmann/dev/backend-user-notification/python/_venv/")))

(after "ido-vertical-mode-autoloads"
  (message "ido-vertical-mode-autoloads")
  (ido-vertical-mode 1))

(after "magit-autoloads"
  (message "Magit has been loaded")
  (setq magit-emacsclient-executable "/usr/local/Cellar/emacs/24.3/bin/emacsclient"))

(after "smex-autoloads"
  (message "Smex has been autoloaded")
  (global-set-key (kbd "C-.") 'smex))

(after "expand-region-autoloads"
  (message "Expand-region has been loaded")
  (global-set-key (kbd "C-w") 'er/expand-region))

(after "auto-complete-autoloads"
  (message "Auto-complete has been autoloaded")
  (require 'auto-complete)
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (setq ac-auto-start nil)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous))

(after "highlight-symbol-autoloads"
  (message "Highlight symbol has been autoloaded")
  (require 'highlight-symbol)
  (global-set-key [f5] 'highlight-symbol-at-point)
  (global-set-key (kbd "s-<f5>") 'highlight-symbol-query-replace)
  (global-set-key [f6] 'highlight-symbol-next)
  (global-set-key [(shift f6)] 'highlight-symbol-prev))

(after "multiple-cursors-autoloads"
  (message "multiple-cursors autoloads")
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(after "bookmark+-autoloads"
  (message "Bookmark+ autoloads")
  (setq bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
  (setq bmkp-auto-light-when-set 'autonamed-bookmark)
  (global-set-key (kbd "s-<f2>") 'bmkp-toggle-autonamed-bookmark-set/delete)
  (global-set-key (kbd "<f2>") 'bmkp-next-bookmark-this-buffer)
  (global-set-key (kbd "S-<f2>") 'bmkp-previous-bookmark-this-buffer))

(after "ag-autoloads"
  (message "Ag has been autoloaded")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't)
  (global-set-key (kbd "s-F") 'ag-project-regexp))

(after "undo-tree-autoloads"
  (message "undo-tree has been autoloaded")
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))

(after "diminish-autoloads"
  (message "diminish-autoloads")
  (after 'undo-tree (diminish 'undo-tree-mode " undo"))
  (after 'projectile (diminish 'projectile-mode " P")))
