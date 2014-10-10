;;; config.el -- Configuration for various packages.

(if window-system
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-13:antialias=subpixel"))

(unless window-system
  (menu-bar-mode -1))

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

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
(setq speedbar-show-unknown-files t)

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

(global-set-key [(super shift return)] 'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-r") 're-read-init-file)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-w") 'delete-frame)
(global-set-key (kbd "M-?") 'ido-complete-symbol-at-point)
(global-set-key (kbd "s-<return>") 'toggle-fullscreen)
(global-set-key (kbd "C-<tab>") 'ace-jump-mode)
(global-set-key (kbd "C-x C-SPC") 'pop-to-mark-command)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "M-s-≥") 'sgml-close-tag) ; textmate like close tag
(global-set-key (kbd "C-c C-p") 'prev-match)
(global-set-key (kbd "C-c C-n") 'next-match)
(global-set-key (kbd "s-{")  'prev-window)
(global-set-key (kbd "s-}") 'other-window)
(global-set-key (kbd "M-a") 'insert-aa) ; For when I want to
(global-set-key (kbd "M-o") 'insert-oe) ; write danish with my
(global-set-key (kbd "M-'") 'insert-ae) ; uk layout keyboard.
(global-set-key (kbd "C-`") 'switch-buffer-visual)
(global-set-key (kbd "s-`") 'ns-next-frame)
(global-set-key (kbd "s-¬") 'ns-prev-frame)

(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-char)

(after "dired+-autoloads"
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "<tab>") 'dired-insert-subdir))))

(after `tramp
  (setq tramp-default-method "ssh"))

(after "exec-path-from-shell-autoloads"
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "CAML_LD_LIBRARY_PATH")) ; Used by OCaml.

(after `eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "C-c C-c") 'eshell-kill-process))))

(after `shell
  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map (kbd "s-k") 'clear-shell)
              (define-key shell-mode-map (kbd "C-c") 'comint-kill-subjob)
              (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
              (define-key shell-mode-map (kbd "<down>") 'comint-next-input))))

(after `flyspell
  (define-key flyspell-mode-map (kbd "C-.") nil))

(after `ido
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-prospects 5)
  (setq ido-auto-merge-work-directories-length -1)) ; disable annoying directory search

(after "virtualenvwrapper-autoloads"
  (require 'virtualenvwrapper)
  (venv-initialize-interactive-shells)
  ;; TODO: set venv-location based on current project?
  (setq venv-location '("/Users/hartmann/dev/payment/_venv/")))

(after "ido-vertical-mode-autoloads"
  (ido-vertical-mode 1))

(after "magit-autoloads"
  ;; TODO: This shouldn't be required.
  (setq magit-emacsclient-executable "/usr/local/Cellar/emacs/24.3/bin/emacsclient"))

(after "projectile-autoloads"
  (projectile-global-mode)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'helm)
  (setq projectile-tags-command "/usr/local/bin/ctags -Re -f %s %s")
  (setq projectile-use-git-grep t)
  (global-set-key (kbd "s-F") 'projectile-grep))

(after "helm-autoloads"
  ;; TODO: Bind helm occur to something less crazy?
  ;; TODO: Use helm-occur-from-isearch?
  (helm-mode 1)
  (setq helm-follow-mode t)
  (global-set-key (kbd "C-.") 'helm-M-x)
  (global-set-key (kbd "C-c h") 'helm-mini)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  ;; (global-set-key (kbd " M-/") 'helm-dabbrev)
  (global-set-key (kbd " M-/") 'dabbrev-expand)
)

(after "helm-projectile-autoloads"
  ;; Remove 'helm-source-projectile-projects' from C-c p h as it is
  ;; possible to switch project using 'helm-projectile-switch-project'
  (setq helm-projectile-sources-list
        '(helm-source-projectile-files-list
          helm-source-projectile-buffers-list
          helm-source-projectile-recentf-list))
  (global-set-key (kbd "C-c p p") 'helm-projectile-switch-project))

(after "expand-region-autoloads"
  (global-set-key (kbd "C-w") 'er/expand-region))

(after "auto-complete-autoloads"
  (global-auto-complete-mode t)
  (ac-config-default)
  (setq ac-auto-start nil)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous))

(after "highlight-symbol-autoloads"
  (global-set-key [f5] 'highlight-symbol-at-point)
  (global-set-key (kbd "s-<f5>") 'highlight-symbol-query-replace)
  (global-set-key [f6] 'highlight-symbol-next)
  (global-set-key [(shift f6)] 'highlight-symbol-prev))

(after "multiple-cursors-autoloads"
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(after "bookmark+-autoloads"
  (setq bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
  (setq bmkp-auto-light-when-set 'autonamed-bookmark)
  (global-set-key (kbd "s-<f2>") 'bmkp-toggle-autonamed-bookmark-set/delete)
  (global-set-key (kbd "<f2>") 'bmkp-next-bookmark-this-buffer)
  (global-set-key (kbd "S-<f2>") 'bmkp-previous-bookmark-this-buffer))

(after "ag-autoloads"
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

(after "undo-tree-autoloads"
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))

(after "diminish-autoloads"
  (after 'undo-tree (diminish 'undo-tree-mode " undo"))
  (after 'projectile (diminish 'projectile-mode " P")))

(after "yasnippet-autoloads"
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippet")))

(after "diff-hl-autoloads"
  (global-diff-hl-mode))

(after "fill-column-indicator-autoloads"
  (add-hook 'python-mode-hook 'fci-mode)
  (set-fill-column 80)
  (setq fci-rule-width 1)
  (setq fci-rule-color "yellow"))

(after `org
  (require 'ob-ocaml)
  (require 'ob-sh)
  (require 'ob-sql)
  (require 'ob-python)

  (setq org-confirm-babel-evaluate nil) ;; Living on the edge
  (setq org-startup-indented t)

  (setq org-agenda-files
        '("~/Dropbox/org"
          "~/Dropbox/org/issuu"
          "~/Dropbox/org/notes"))

  (setq org-babel-load-languages
        '((ocaml . t)
          (emacs-lisp . t)
          (sh . t)
          (sql . t)
          (python . t)))

  (define-key org-mode-map (kbd "C-c C-a") 'org-agenda)

  (add-hook 'org-mode-hook 'configure-org-buffer))
