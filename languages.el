;;
;; Languages
;;

;; Octave support
;;
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; OCaml
;;

;; Update the Emacs load path.
(push
 (concat (substring (shell-command-to-string "opam config var share") 0 -1)
         "/emacs/site-lisp") load-path)

;; Setup environment variables using opam
(dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; Update the emacs path
(setq exec-path (split-string (getenv "PATH") path-separator))

;; Merlin
(setq merlin-command
      (concat (substring (shell-command-to-string "opam config var bin") 0 -1)
              "/ocamlmerlin"))

(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;; Better default compile-command for my Ocaml projects.
(add-hook 'tuareg-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "make -w -j4 -C " (or (upward-find-file "Makefile") ".")))))

;; OCP-Indent. Pretty indentation for OCaml code (installed through opam)
(require 'ocp-indent)

;; Automatically load utop.el and make it the default toplevel.
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

;; Erlang
;;
(add-hook 'erlang-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "make -w -C " (or (upward-find-file "Makefile") ".")))))

;; Sass
;;
(setq scss-compile-at-save nil)

;; Javascript
;;
(setq js-indent-level 4)

;; Python
;;

;; Better default compile-command for my Python projects.
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "make -w -C " (or (upward-find-file "Makefile") ".") " pylint-errors"))))
