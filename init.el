;;; init.el --- Mads' configuration file
(server-start)

(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(require 'use-package)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(load-theme 'wombat t)
(load "~/.emacs.d/functions.el")

(if window-system
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-13:antialias=subpixel"))

(unless window-system
  (menu-bar-mode -1))

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

(setq x-select-enable-clipboard t)
(setq require-final-newline t)
(set-default 'truncate-lines t)
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
(setq enable-local-variables :all) ; Sort of scary.
(setq dabbrev-case-replace nil)
(setq dabbrev-case-distinction nil)
(setq dabbrev-case-fold-search nil)

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
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key (kbd "s-w") 'delete-frame)
(global-set-key (kbd "s-<return>") 'toggle-fullscreen)
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
(global-set-key (kbd "C-c C-1") 'previous-buffer)
(global-set-key (kbd "C-c C-2") 'next-buffer)

(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-char)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ace-jump-mode
  :bind ("C-<tab>" . ace-jump-mode))

(use-package ace-jump-zap
  :bind ("M-z" . ace-jump-zap-to-char))

(use-package dired+
  :ensure t
  :pre-load (setq diredp-hide-details-initially-flag nil)
  :config
  (progn
    (setq-default dired-omit-files-p t)
    (setq dired-omit-files
          (concat dired-omit-files "\\|\\.pyc$"))
    (add-hook 'dired-mode-hook
              (lambda ()
                (define-key dired-mode-map (kbd "<tab>") 'dired-insert-subdir)))))

(use-package tramp
  :config (setq tramp-default-method "ssh"))

(use-package exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "CAML_LD_LIBRARY_PATH"))) ; Used by OCaml.

(use-package shell
  :config
  (progn
    (define-key shell-mode-map (kbd "s-k") 'clear-shell)
    (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
    (define-key shell-mode-map (kbd "<down>") 'comint-next-input)))

(use-package flyspell
  :config (progn
            (define-key flyspell-mode-map (kbd "C-.") nil)))

(use-package ido
  :ensure t
  :init
  (progn
    (ido-mode 1)
    (setq ido-enable-flex-matching t)
    (setq ido-use-filename-at-point nil)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 5)
    (setq ido-auto-merge-work-directories-length -1))) ; disable annoying directory search

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :diminish " P"
  :bind ("s-F" . projectile-grep)
  :init (progn
          (projectile-global-mode)
          (setq projectile-switch-project-action 'projectile-dired)
          (setq projectile-completion-system 'helm)
          (setq projectile-tags-command "/usr/local/bin/ctags -Re -f %s %s")
          (setq projectile-use-git-grep t)))

(use-package helm
  :ensure t
  :init
  (progn
    (helm-mode 1)
    (setq helm-follow-mode t)
    (setq helm-split-window-in-side-p t)
    (setq helm-buffers-fuzzy-matching t))
  :bind (("C-c C-s" . helm-occur)
         ("C-." . helm-M-x)
         ("C-c h" . helm-mini)
         ("C-x b" . helm-buffers-list)
         (" M-/" . dabbrev-expand)))

(use-package helm-projectile
  :ensure t
  :config
  (progn
    ;; Removes 'helm-source-projectile-projects' from C-c p h as it is
    ;; possible to switch project using 'helm-projectile-switch-project'
    (setq helm-projectile-sources-list
          '(helm-source-projectile-files-list
            helm-source-projectile-buffers-list
            helm-source-projectile-recentf-list)))
  :bind ("C-c p p" . helm-projectile-switch-project))

(use-package expand-region
  :ensure t
  :bind ("C-w" . er/expand-region))

(use-package auto-complete
  :ensure t
  :init (global-auto-complete-mode t)
  :config (progn
            (ac-config-default)
            (setq ac-auto-start nil)
            (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
            (define-key ac-complete-mode-map "\C-n" 'ac-next)
            (define-key ac-complete-mode-map "\C-p" 'ac-previous)))

(use-package highlight-symbol
  :ensure t
  :bind (([f5]           . highlight-symbol-at-point)
         ((kbd "s-<f5>") . highlight-symbol-query-replace)
         ([f6]           . highlight-symbol-next)
         ([(shift f6)]   . highlight-symbol-prev)
         ([f7]           . highlight-symbol-occur)
         ([f8]           . highlight-symbol-remove-all))
  :init (highlight-symbol-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package bookmark+
  :ensure t
  :config (progn
            (setq bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
            (setq bmkp-auto-light-when-set 'autonamed-bookmark))
  :bind (("s-<f2>" . bmkp-toggle-autonamed-bookmark-set/delete)
         ("<f2>" . bmkp-next-bookmark-this-buffer)
         ("S-<f2>" . bmkp-previous-bookmark-this-buffer)))

(use-package ag
  :ensure t
  :config (progn
            (setq ag-highlight-search t)
            (setq ag-reuse-buffers 't)))

(use-package diminish
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish " undo"
  :init (progn
          (global-undo-tree-mode t)
          (setq undo-tree-visualizer-relative-timestamps t)
          (setq undo-tree-visualizer-timestamps t)))

(use-package yasnippet
  :ensure t
  :defer
  :config
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-reload-all)))

(use-package diff-hl
  :ensure t
  :disabled ; Doesn't seem to work with my emacs version
  :init (progn (global-diff-hl-mode)))

(use-package elfeed
  :defer
  :ensure t
  :config
  (progn
    (setq elfeed-db-directory "~/Dropbox/Apps/elfeed")
    (setq-default elfeed-search-filter "@1-week-ago +unread ")
    (setq elfeed-max-connections 1)
    (setq elfeed-feeds
          '(("http://feeds.feedburner.com/amirmc" tech)
            ("http://sachachua.com/blog/feed" tech)
            ("http://medium.com/feed/@jlouis666" tech)
            ("http://psnively.github.com/atom.xml" tech)
            ("http://www.drmaciver.com/feed/" tech)
            ("http://davehakkens.nl/feed/" tech)
            ("http://www.defmacro.org/feed.atom" tech)
            ("http://feeds.feedburner.com/holman" tech)
            ("http://worrydream.com/feed.xml" tech)
            ("http://caitiem.com/feed/" tech)
            ("http://feeds.feedburner.com/tom-preston-werner" tech)
            ("http://mads379.github.io/feed.xml" tech)
            ("http://logicaltypes.blogspot.com/feeds/posts/default?alt=rss" tech)
            ("http://anil.recoil.org/feeds/atom.xml" tech)
            ("http://feeds.feedburner.com/threeriversinstitute/khkV" tech)
            ("http://nerds.airbnb.com/feed/" company)
            ("http://stripe.com/blog/feed.rss" company)
            ("http://blog.alfredapp.com/feed/" company)
            ("http://github.com/blog.atom" company)
            ("http://techblog.netflix.com/feeds/posts/default" company)
            ("http://www.paus.dk/blog?format=RSS" misc)
            ("http://designairs.com/feed/" company)
            ("http://ocaml.janestreet.com/?q=rss.xml" company)
            ("http://functionaltalks.org/feed.xml" programming)
            ("http://googleonlinesecurity.blogspot.com/feeds/posts/default" programming)
            ("http://www.openmirage.org/blog/atom.xml" programming)
            ("http://planet.ocamlcore.org/atom.xml" programming)
            ("http://okmij.org/ftp/rss.xml" programming)
            ("http://emacsrocks.com/atom.xml" misc emacs)
            ("http://endlessparentheses.com/atom.xml" emacs)
            ("http://emacsrocks.com/atom.xml" emacs)
            ("http://planet.emacsen.org/atom.xml" emacs)
            ("http://whattheemacsd.com/atom.xml" emacs)
            ("http://www.lunaryorn.com/feed.atom" emacs ocaml)
            ("http://folkelab.dk/feed/atom.xml" misc)
            ("http://feeds.wnyc.org/radiolab" misc)
            ("http://blog.genbyg.dk/feed/" misc)
            ("http://logicaltypes.blogspot.com/feeds/posts/default?alt=rss" programming)
            ("http://anil.recoil.org/feeds/atom.xml" tech ocaml)
            ("http://www.somerandomidiot.com/atom.xml" tech ocaml)
            ("http://davehakkens.nl/feed/" misc)
            ("http://www.masteringemacs.org/feed/" tech)
            ("http://firstlook.org/theintercept/feed/" news)
            ("http://www.schneier.com/blog/atom.xml" tech security)
            ("http://feeds.feedburner.com/TroyHunt" tech security)
            ("http://lambda-the-ultimate.org/rss.xml" tech programming-languages)
            ("http://politiken.dk/rss/ibyen.rss")
            ("http://soundvenue.com/category/musik/feed" music)
            ("http://www.slow-journalism.com/feed" news)
            ))))

(use-package org
  :init
  (progn
    (require 'ob-ocaml)
    (require 'ob-sh)
    (require 'ob-sql)
    (require 'ob-python)
    (require 'ob-js)
    (require 'ob-R)

    (define-key org-mode-map (kbd "C-c C-a") 'org-agenda)
    (define-key org-mode-map (kbd "C-<tab>") nil)

    (setq org-startup-folded nil)
    (setq org-confirm-babel-evaluate nil) ;; Living on the edge
    (setq org-startup-indented nil)

    ;; Blogging
    (setq org-publish-project-alist
          '(("org-mads379.github.com"
             ;; Path to your org files.
             :base-directory "~/dev/mads379.github.com/_org/"
             :base-extension "org"
             ;; Path to your Jekyll project.
             :publishing-directory "~/dev/mads379.github.com/"
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4
             :html-extension "html"
             :body-only t) ;; Only export section between <body> </body>
            ("org-static-mads379.github.com"
             :base-directory "~/dev/mads379.github.com/org/"
             :base-extension "css\\|js\\|png\\|jpg\\|gif"
             :publishing-directory "~/dev/mads379.github.com/"
             :recursive t
             :publishing-function org-publish-attachment)
            ("mads379.github.com"
             :components ("org-ianbarton" "org-static-ian"))))

    (setq org-babel-load-languages
          '((ocaml . t)
            (emacs-lisp . t)
            (sh . t)
            (sql . t)
            (python . t)
            (js . t)
            (r . R)))

    (setq org-agenda-files
          '("~/Dropbox/org"
            "~/Dropbox/org/issuu"
            "~/Dropbox/org/notes"))

    (add-hook 'org-mode-hook 'yas-minor-mode)))

(use-package smart-mode-line
  :ensure t
  :init (progn
          (setq sml/no-confirm-load-theme t)
          (sml/setup)
          (sml/apply-theme 'respectful)))

(use-package scss-mode
  :defer
  :config (setq scss-compile-at-save nil))

(use-package javascript-mode
  :defer
  :config (setq js-indent-level 4))

(use-package flymake-jshint
  :defer
  :config
  (progn
    (defun on-js-mode ()
      (flymake-mode)
      (flymake-jshint-load))
    (add-hook 'js-mode-hook 'on-js-mode)))

(use-package markdown-mode
  :defer
  :config
  (progn
    (define-key markdown-mode-map (kbd "M-<tab>") 'ido-complete-word-ispell)
    (add-hook 'markdown-mode-hook 'flyspell-mode)))

(use-package lisp-mode
  :defer
  :config
  (progn
    (define-key lisp-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
    (define-key lisp-mode-map (kbd "M-,") 'pop-tag-mark)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
    (add-hook 'lisp-mode 'flyspell-prog-mode)))

(use-package ess
  :ensure t)

(use-package octave
  :defer
  :config
  (progn
    (autoload 'octave-mode "octave-mod" nil t)
    (setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
    (add-hook 'octave-mode-hook (lambda ()
                                  (abbrev-mode 1)
                                  (auto-fill-mode 1)
                                  (if (eq window-system 'x)
                                      (font-lock-mode 1))))))

(use-package erlang
  :defer
  :config
  (progn
    (add-to-list 'load-path "/usr/local/share/distel/elisp") ; Not in melpa yet
    (require 'distel)
    (require 'erlang-flymake)

    (defun erlang-make-cmd ()
      (concat "make -w -C " (or (upward-find-file "Makefile") ".")))

    (distel-setup)
    ;; http://parijatmishra.wordpress.com/2008/08/15/up-and-running-with-emacs-erlang-and-distel/
    ;; http://alexott.net/en/writings/emacs-devenv/EmacsErlang.html#sec8
    (setq inferior-erlang-machine-options '("-sname" "emacs"))

    (define-key erlang-mode-map (kbd "M-.") 'erl-find-source-under-point)
    (define-key erlang-mode-map (kbd "M-,") 'erl-find-source-unwind)
    (define-key erlang-mode-map (kbd "M-<tab>") 'erl-complete)
    (define-key erlang-mode-map (kbd "C-c C-c") 'compile)
    (define-key erlang-mode-map (kbd "<return>")'newline-and-indent)

    (add-hook 'erlang-mode-hook 'flymake-mode)
    (add-hook 'erlang-mode-hook (lambda ()
                                  (set (make-local-variable 'compile-command) (erlang-make-cmd))))))

(use-package tuareg
  :defer
  :config
  (progn

    ;; Add opam emacs directory to the load-path
    (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
    (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

    ;; Setup environment variables using OPAM
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))

    ;; One of the `opam config env` variables is PATH. Update `exec-path` to that.
    (setq exec-path (split-string (getenv "PATH") path-separator))

    ;; Load merlin-mode
    (require 'merlin)
    (require 'ocp-indent)

    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    (setq merlin-use-auto-complete-mode 'easy)

    ;; Automatically load utop.el.
    (add-to-list 'load-path "/Users/hartmann/dev/utop/src/top")
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)

    ;; Used if I want to run some of ISSUU's OCaml projects in UTOP .
    (setenv "AGGREGATOR_CONF_SHADOW" "")
    (setenv "AGGREGATOR_HOME" "/Users/hartmann/dev/backend-insight/aggregator")
    (setenv "PROMOTED_HOME" "/Users/hartmann/dev/backend-promoted")
    (setenv "PROMOTED_CONF_SHADOW" "")

    (define-key merlin-mode-map (kbd "M-<tab>") 'merlin-try-completion)
    (define-key merlin-mode-map "\M-." 'merlin-locate)
    (define-key merlin-mode-map "\M-," 'merlin-pop-stack)
    (define-key merlin-mode-map (kbd "C-c C-p") 'prev-match)
    (define-key merlin-mode-map (kbd "C-c C-n") 'next-match)
    (define-key tuareg-mode-map (kbd "C-x C-r") 'tuareg-eval-region)
    (define-key tuareg-mode-map (kbd "C-c C-s") nil)

    ;; (setq merlin-logfile "/Users/hartmann/Desktop/merlin.log")
    (setq merlin-error-after-save nil)

    (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    (add-hook 'tuareg-mode-hook (lambda ()
                                  (setq indent-line-function 'ocp-indent-line)))))

(use-package python
  :defer
  :config
  (progn
    (defconst pylint-conf-filename "pylint.cfg")
    (defconst epylint-path "/Users/hartmann/.emacs.d/python/epylint.py")
    (defconst global-conf-dir "/Users/hartmann/.emacs.d/python")

    ;; Enable flymake for python files. Make sure it respect the pylint.cfg
    ;; config files if one exists.
    (when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((project-root (upward-find-file ".git"))
               (temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
               (local-file (file-relative-name
                            temp-file
                            (file-name-directory buffer-file-name)))
               (conf-file-dir (or (upward-find-file pylint-conf-filename) global-conf-dir))
               (pylint-path (if (file-exists-p (concat project-root "/_venv/bin/pylint"))
                                (concat project-root "/_venv/bin/pylint")
                              "pylint"))
               (full-conf-path (concat conf-file-dir "/" pylint-conf-filename)))
          (list epylint-path (list pylint-path full-conf-path temp-file))))

      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.py\\'" flymake-pylint-init)))

    (define-key python-mode-map (kbd "M-<tab>") 'jedi:complete)
    (define-key python-mode-map (kbd "C-c C-s") 'helm-occur)
    (define-key python-mode-map (kbd "C-c C-c") 'compile)
    (define-key python-mode-map (kbd "C-c C-p") nil)

    (add-hook 'python-mode-hook 'flymake-mode)
    (add-hook 'python-mode-hook 'jedi:setup)))

(use-package jedi
  :defer
  :config
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)
    (add-hook 'jedi-mode-hook (lambda ()
                                (define-key jedi-mode-map (kbd "C-<tab>") nil)
                                (define-key jedi-mode-map "\M-." 'jedi:goto-definition)
                                (define-key jedi-mode-map "\M-," 'jedi:goto-definition-pop-marker)
                                ))))
