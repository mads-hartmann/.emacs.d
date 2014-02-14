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
      (message "Looking at %s" (expand-file-name filename dirname))
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
        ; If not, move up a directory
        (setq dirname (expand-file-name ".." dirname))))
    ; return statement
    (if found dirname nil)))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun refresh-safari ()
  "Executes a shell-script, that executes an applescript, that
   refreshes the active tab in the front-most safari window,
   in the background."
  (interactive)
  (shell-command "source ~/.emacs.d/shell-functions/refresh-safari.sh"))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun ido-mx ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (all-completions "" obarray 'commandp)))))

(defun string/starts-with (s begins)
      "returns non-nil if string S starts with BEGINS.  Else nil."
      (cond ((>= (length s) (length begins))
             (string-equal (substring s 0 (length begins)) begins))
            (t nil)))

(defun rgrep-in-project ()
  ;; TODO: Figure out a way to have a reasonable 'accepted-source-files' list.
  (interactive)
  (let ((path (upward-find-file "TAGS"))
        (symbol (thing-at-point 'symbol t)))
    (rgrep symbol "*.py" path)))

(defun clear-shell ()
   (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))
