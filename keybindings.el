;;
;; Keybindings
;;

(global-set-key "\C-x\C-r" 're-read-init-file)

;; misc
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key [(super shift return)] 'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f3>") 'find-tag)
(global-set-key (kbd "S-<f3>") 'pop-tag-mark)
(global-set-key [(super w)] 'kill-buffer)
(global-set-key (kbd "C-w") 'my-mark-current-word)
(global-set-key [f11] 'sr-speedbar-toggle)

;; movement
(global-set-key (kbd "C-s-<left>")  'windmove-left)
(global-set-key (kbd "C-s-<right>") 'windmove-right)
(global-set-key (kbd "C-s-<up>")    'windmove-up)
(global-set-key (kbd "C-s-<down>")  'windmove-down)

;; git
(global-set-key (kbd "s-F") 'magit-grep)

;; Auto-complete mode
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; compilation
(global-set-key [f1] 'ffap)
(global-set-key [f8] 'compile)
(global-set-key [f12] 'refresh-safari)

;; ocaml
(global-set-key "\M-." 'merlin-locate)
(global-set-key "\M->" 'merlin-pop-stack)
