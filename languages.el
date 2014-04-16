;;; languages.el --- Programming Language specific configuration.

(after `scss-mode
  (message "Sass has been loaded")
  (setq scss-compile-at-save nil))

(after `javascript-mode
  (message "Javascript has been loaded")
  (setq js-indent-level 4))

(after `markdown-mode
  (message "Markdown has been loaded")
  (define-key markdown-mode-map (kbd "M-<tab>") 'ido-complete-word-ispell)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (flyspell-mode))))

(after `lisp-mode
  (message "Lisp-mode has been loaded")
  (add-hook 'lisp-mode 'flyspell-prog-mode))

(after `org
  (message "Org has been loaded")
  (setq org-startup-folded nil))

(after `octave
  (message "Octave has been loaded")
  (autoload 'octave-mode "octave-mod" nil t)
  (setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
  (add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1)))))

(after `erlang
  (message "Erlang has been loaded")

  (defconst erlang-make-cmd
    (concat "make -w -C " (or (upward-find-file "Makefile") ".")))

  (add-hook 'erlang-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command) erlang-make-cmd))))

(after `tuareg
  (message "OCaml has been loaded")

  (defconst make-cmd
    (concat "make -w -j4 -C " (or (upward-find-file "Makefile") ".")))

  ;; Add OPAM installed elisp files to the load-path.
  (push
   (concat (substring (shell-command-to-string "opam config var share") 0 -1)
           "/emacs/site-lisp") load-path)

  ;; Setup environment variables using OPAM
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))

  ;; One of the `opam config env` variables is PATH. Update `exec-path` to that.
  (setq exec-path (split-string (getenv "PATH") path-separator))

  ;; Tell merlin where to find the executable.
  (setq merlin-command
        (concat (substring (shell-command-to-string "opam config var bin") 0 -1)
                "/ocamlmerlin"))

  ;; merlin-mode is provided in merlin.el on the load-path.
  (autoload 'merlin-mode "merlin" "Merlin mode" t)

  ;; Automatically load utop.el and make it the default toplevel.
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)

  ;; Used if I want to run some of ISSUU's OCaml projects in UTOP .
  (setenv "AGGREGATOR_CONF_SHADOW" "")
  (setenv "AGGREGATOR_HOME" "/Users/hartmann/dev/backend-insight/aggregator")
  (setenv "PROMOTED_HOME" "/Users/hartmann/dev/backend-promoted")
  (setenv "PROMOTED_CONF_SHADOW" "")

  (add-hook 'tuareg-mode-hook
            (lambda ()

              (merlin-mode)
              (utop-setup-ocaml-buffer)

              ;; Better default make command for OCaml projects.
              (set (make-local-variable 'compile-command) make-cmd)

              (define-key merlin-mode-map "\M-." 'merlin-locate)
              (define-key merlin-mode-map "\M->" 'merlin-pop-stack)
              (define-key merlin-mode-map (kbd "C-c C-p") 'prev-match)
              (define-key merlin-mode-map (kbd "C-c C-n") 'next-match)
              (define-key tuareg-mode-map (kbd "C-x C-r") 'tuareg-eval-region))))

(after `python
  (message "Python has been loaded")

  (defconst pylint-conf-filename "pylint.cfg")
  (defconst epylint-path "/Users/hartmann/.emacs.d/python/epylint.py")
  (defconst python-make-cmd (concat "make -w -C " (or (upward-find-file "Makefile") ".") " pylint"))

  ;; Enable flymake for python files. Make sure it respect the pylint.cfg
  ;; config files if one exists.
  (when (load "flymake" t)
    (defun flymake-pylint-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name)))
             (conf-file (or (upward-find-file pylint-conf-filename) "")))
        (list epylint-path (list (concat conf-file "/" pylint-conf-filename) local-file))))

    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.py\\'" flymake-pylint-init)))

  (add-hook 'python-mode-hook 'flymake-mode)

  ;; Better default compile-command for my Python projects.
  (add-hook 'python-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command) python-make-cmd))))
