;;; languages.el --- Programming Language specific configuration.

(after `scss-mode
  (setq scss-compile-at-save nil))

(after `javascript-mode
  (setq js-indent-level 4)

  (after "flymake-jshint-autoloads"
    (require 'flymake-jshint)
    (defun on-js-mode ()
      (flymake-mode)
      (flymake-jshint-load))
    (add-hook 'js-mode-hook 'on-js-mode)))

(after `markdown-mode
  (define-key markdown-mode-map (kbd "M-<tab>") 'ido-complete-word-ispell)
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(after `lisp-mode
  (define-key lisp-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
  (define-key lisp-mode-map (kbd "M-,") 'pop-tag-mark)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  (add-hook 'lisp-mode 'flyspell-prog-mode))

(after `octave
  (autoload 'octave-mode "octave-mod" nil t)
  (setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
  (add-hook 'octave-mode-hook (lambda ()
    (abbrev-mode 1)
    (auto-fill-mode 1)
    (if (eq window-system 'x)
        (font-lock-mode 1)))))

(after `erlang
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
    (set (make-local-variable 'compile-command) (erlang-make-cmd)))))

(after `tuareg

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

  ;; Automatically load utop.el and make it the default toplevel.
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)

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

  (add-hook 'tuareg-mode-hook (lambda ()
    (merlin-mode)
    (setq indent-line-function 'ocp-indent-line)
    (utop-setup-ocaml-buffer))))

(after `python
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
  (add-hook 'python-mode-hook 'jedi:setup)

  (add-hook 'jedi-mode-hook (lambda ()
    (define-key jedi-mode-map (kbd "C-<tab>") nil)
    (define-key jedi-mode-map "\M-." 'jedi:goto-definition)
    (define-key jedi-mode-map "\M-," 'jedi:goto-definition-pop-marker)
  )))
