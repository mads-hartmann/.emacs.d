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
;; (add-hook 'tuareg-mode-hook 'merlin-mode)
;; (add-hook 'caml-mode-hook 'merlin-mbode)
(remove-hook 'tuareg-mode-hook 'merlin-mode)
(remove-hook 'caml-mode-hook 'merlin-mbode)

;;;;;;; ocamlspot

;; ; load-path ;; use same as ocamlspot-command but with lib.
;; (setq load-path (cons "/Users/hartmann/.opam/issuu/lib/ocamlspot/" load-path))

;; (require 'ocamlspot)

;; ; tuareg mode hook (use caml-mode-hook instead if you use caml-mode)
;; (add-hook 'tuareg-mode-hook
;;   '(lambda ()
;;     (local-set-key "\C-c;" 'ocamlspot-query)
;;     (local-set-key "\C-c:" 'ocamlspot-query-interface)
;;     (local-set-key "\C-c'" 'ocamlspot-query-uses)
;;     (local-set-key "\C-c\C-t" 'ocamlspot-type)
;;     (local-set-key "\C-c\C-i" 'ocamlspot-xtype)
;;     (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
;;     (local-set-key "\C-ct" 'caml-types-show-type)
;;     (local-set-key "\C-cp" 'ocamlspot-pop-jump-stack)))

;; ; set the path of the ocamlspot binary. If you did make opt, ocamlspot.opt is recommended.
;; (setq ocamlspot-command
;;       (concat (substring (shell-command-to-string "opam config var bin") 0 -1)
;;               "/ocamlspot.opt"))

;;;;;;; ocamlspot end

;; Better default compile-command for my Ocaml projects.

(defun compile-ocaml-project ()
  (interactive)
  (compile (concat "make -w -j4 -C " (or (upward-find-file "Makefile") "."))))


(add-hook 'tuareg-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "make -w -j4 -C " (or (upward-find-file "Makefile") ".")))
            ;; (add-hook 'after-save-hook 'compile-ocaml-project)
            ))

;; (remove-hook 'after-save-hook 'compile-ocaml-project)

(setenv "AGGREGATOR_CONF_SHADOW" "")
(setenv "AGGREGATOR_HOME" "/Users/hartmann/dev/backend-insight/aggregator")
(setenv "PROMOTED_HOME" "/Users/hartmann/dev/backend-promoted")
(setenv "PROMOTED_CONF_SHADOW" "")

;; Colored buffers
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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
