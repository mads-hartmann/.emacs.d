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

;; Erlang
;;
(add-hook 'erlang-mode-hook 
	  (lambda () 
	    (set (make-local-variable 'compile-command)
		 (concat "make -w -C " (or (upward-find-file "Makefile") ".")))))
