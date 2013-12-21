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
(push
 (concat (substring (shell-command-to-string "opam config var share") 0 -1)
         "/emacs/site-lisp") load-path)

(setq merlin-command
      (concat (substring (shell-command-to-string "opam config var bin") 0 -1)
              "/ocamlmerlin"))

(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

(add-hook 'tuareg-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "make -w -j4 -C " (or (upward-find-file "Makefile") ".")))))

;; OCP-Indent. Pretty indentation for OCaml code.
(add-to-list 'load-path (concat
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "opam config var share"))
  "/emacs/site-lisp"))
(require 'ocp-indent)

;;
;; Setup environment variables using opam
(dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; Update the emacs path
(setq exec-path (split-string (getenv "PATH") path-separator))

;; Update the emacs load path
(push (concat (getenv "OCAML_TOPLEVEL_PATH") "/../../share/emacs/site-lisp") load-path)

;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)

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
(setq js-indent-level 2)
