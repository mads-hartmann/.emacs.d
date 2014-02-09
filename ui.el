(setq linum-format "%3d\u2502")

(load-theme 'solarized-dark)
(set-face-attribute 'default nil :font "Monaco-11")

;; Using Powerline to get a fancy mode-line.
;;
;; The current version of powerline has a bug, however,
;; so this also contains the neccesary work-around. See
;; https://github.com/milkypostman/powerline/issues/37

(custom-set-faces

 '(mode-line
   ((t :foreground "#93A1A1"
       :background "#063642"
       :box nil)))

 '(mode-line-inactive
   ((t :inherit mode-line
       :background "#063642"
       :box nil)))

 '(powerline-active2
   ((t :inherit mode-line
       :foreground "#93A1A1"
       :background "#063642")))

 '(powerline-active1
   ((t :inherit mode-line
       :foreground "##FDF6E3"
       :background "#2AA198")))

 '(powerline-inactive1
   ((t :inherit mode-line-inactive
       :background "#042931")))

 '(powerline-inactive2
   ((t :inherit mode-line-inactive
       :foreground "#93A1A1"
       :background "#063642"))))

(add-hook 'post-command-hook (lambda ()
                               (when (not (minibuffer-selected-window))
                                 (setq powerline-selected-window (selected-window)))))

;;
;; Removed the 'scrollbar' on the right side of the mode-line.
(defun my-powerline-default-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (eq powerline-selected-window (selected-window)))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-raw mode-line-mule-info nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(my-powerline-default-theme)
