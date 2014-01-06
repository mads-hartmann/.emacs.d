;;
;; Keybindings
;;

(require 'multiple-cursors)
(require 'expand-region)

(global-set-key "\C-x\C-r" 're-read-init-file)

;; misc
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key [(super shift return)] 'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f3>") 'find-tag)
(global-set-key (kbd "S-<f3>") 'pop-tag-mark)
(global-set-key [(super w)] 'kill-buffer)
(global-set-key [f11] 'sr-speedbar-toggle)
(global-set-key (kbd "C-w") 'er/expand-region)

;; movement
(global-set-key (kbd "C-s-<left>")  'windmove-left)
(global-set-key (kbd "C-s-<right>") 'windmove-right)
(global-set-key (kbd "C-s-<up>")    'windmove-up)
(global-set-key (kbd "C-s-<down>")  'windmove-down)
(global-set-key (kbd "s-<return>") 'toggle-fullscreen)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)

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

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
