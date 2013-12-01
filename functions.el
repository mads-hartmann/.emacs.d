(defun re-read-init-file ()
  "Reread ~/.emacs.d/init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun toggle-maximize-buffer () 
  "Maximize buffer - in a nice iTerm style way"
  (interactive)
  (if (= 1 (length (window-list)))
    (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

(defun compile-next-makefile ()
  (interactive)
  (let* ((default-directory (or (upward-find-file "Makefile") "."))
         (compile-command (concat "cd " default-directory " && "
                                  compile-command)))
    (compile compile-command)))

(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name
                  (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get
                    ; to / so that we only check it once

    ; While we've neither been at the top last time nor have we found
    ; the file.
    (while (not (or found top))
      ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
          (setq top t))

      ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
        ; If not, move up a directory
        (setq dirname (expand-file-name ".." dirname))))
    ; return statement
    (if found dirname nil)))

;; TODO: Can we make this run in the background?
;; Have a look at 'async shell command'
(defun create-tags ()
  (interactive)
  (let* ((ctags-path "/usr/local/Cellar/ctags/5.8/bin/ctags")
	 (default-directory (or (upward-find-file ".gitignore") "."))
         (compile-command (concat "cd " default-directory
				  " && " ctags-path " -e -R " default-directory " TAGS")))
    (shell-command compile-command)))

(defun refresh-safari ()
  "Executes a shell-script, that executes an applescript, that
   refreshes the active tab in the front-most safari window,
   in the background."
  (interactive)
  (shell-command "source ~/.emacs.d/shell-functions/refresh-safari.sh"))

(defun my-mark-current-word (&optional arg allow-extend)
    "put point at beginning of current word, set mark at end."
    (interactive "p\np")
    (setq arg (if arg arg 1))
    (if (and allow-extend
             (or (and (eq last-command this-command) (mark t))
                 (region-active-p)))
        (set-mark
         (save-excursion
           (when (< (mark) (point))
             (setq arg (- arg)))
           (goto-char (mark))
           (forward-word arg)
           (point)))
      (let ((wbounds (bounds-of-thing-at-point 'word)))
        (unless (consp wbounds)
          (error "no word at point"))
        (if (>= arg 0)
            (goto-char (car wbounds))
          (goto-char (cdr wbounds)))
        (push-mark (save-excursion
                     (forward-word arg)
                     (point)))
        (activate-mark))))
