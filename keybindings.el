;;
;; Keybindings
;;

(require 'multiple-cursors)
(require 'expand-region)

(global-set-key "\C-x\C-r" 're-read-init-file)

;; misc
(global-set-key "\M-x" 'ido-mx)
(global-set-key (kbd "C-.") 'ido-mx)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key [(super shift return)] 'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "<f3>") 'find-tag)
(global-set-key (kbd "S-<f3>") 'pop-tag-mark)

(global-set-key [(super w)] 'delete-frame)
(global-set-key (kbd "C-w") 'er/expand-region)

(global-set-key (kbd "s-t") 'ido-find-file-in-tag-files)
(global-set-key (kbd "s-\\") 'ido-find-tag-in-file)
(global-set-key (kbd "s-|") 'ido-find-tag)
(global-set-key (kbd "M-?") 'ido-complete-symbol-at-point)

;; movement
(global-set-key (kbd "C-s-<left>")  'windmove-left)
(global-set-key (kbd "C-s-<right>") 'windmove-right)
(global-set-key (kbd "C-s-<up>")    'windmove-up)
(global-set-key (kbd "C-s-<down>")  'windmove-down)
(global-set-key (kbd "s-<return>") 'toggle-fullscreen)
(global-set-key (kbd "C-c C-c") 'ace-jump-mode)

;; git
(global-set-key (kbd "s-F") 'magit-grep)

;; Auto-complete mode
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; compilation
(global-set-key [f4] 'ffap)
(global-set-key [f8] 'compile)
(global-set-key [f12] 'magit-status)

;; highlighting symbols
(global-set-key [f5] 'highlight-symbol-at-point)
(global-set-key (kbd "s-<f5>") 'highlight-symbol-query-replace)
(global-set-key [f6] 'highlight-symbol-next)
(global-set-key [(shift f6)] 'highlight-symbol-prev)

;; ocaml
(global-set-key "\M-." 'merlin-locate)
(global-set-key "\M->" 'merlin-pop-stack)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "M-s-≥") 'sgml-close-tag)

;; Shell buffer
(define-key shell-mode-map (kbd "C-c") 'clear-shell)

;; Bookmarks+
;; Textmate like bookmark behaviour
(global-set-key (kbd "s-<f2>") 'bmkp-toggle-autonamed-bookmark-set/delete)
(global-set-key (kbd "<f2>") 'bmkp-next-autonamed-bookmark-repeat)
(global-set-key (kbd "S-<f2>") 'bmkp-previous-autonamed-bookmark-repeat)

(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; window-numbering
;;
(global-set-key (kbd "s-0") 'select-window-0)
(global-set-key (kbd "s-1") 'select-window-1)
(global-set-key (kbd "s-2") 'select-window-2)
(global-set-key (kbd "s-3") 'select-window-3)
(global-set-key (kbd "s-4") 'select-window-4)
(global-set-key (kbd "s-5") 'select-window-5)
(global-set-key (kbd "s-6") 'select-window-6)
(global-set-key (kbd "s-7") 'select-window-7)
(global-set-key (kbd "s-8") 'select-window-8)
(global-set-key (kbd "s-9") 'select-window-9)
