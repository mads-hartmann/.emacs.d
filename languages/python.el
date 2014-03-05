;;
;;
;;

(defconst pylint-conf-filename "pylint.cfg")
(defconst epylint-path "/Users/hartmann/.emacs.d/python/epylint.py")

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
            (set (make-local-variable 'compile-command)
                 (concat "make -w -C " (or (upward-find-file "Makefile") ".") " pylint"))))
