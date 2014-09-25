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

(defun string/starts-with (s begins)
      "returns non-nil if string S starts with BEGINS.  Else nil."
      (cond ((>= (length s) (length begins))
             (string-equal (substring s 0 (length begins)) begins))
            (t nil)))

(defun clear-shell ()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))

(defun ido-complete-word-ispell ()
  "Completes the symbol at point based on entries in the
dictionary"
  (interactive)
  (let* ((word (thing-at-point 'symbol))
         (boundaries (bounds-of-thing-at-point 'symbol))
         (start (car boundaries))
         (end (cdr boundaries))
         (words (lookup-words word)))
    (let ((selection (ido-completing-read "Completions: " words)))
      (if selection
          (progn
            (delete-region start end)
            (insert selection))))))

(defun prev-match ()
  (interactive)
  (next-match -1))

(defun insert-aa ()
  (interactive)
  (insert "å"))

(defun insert-ae ()
  (interactive)
  (insert "æ"))

(defun insert-oe ()
  (interactive)
  (insert "ø"))

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun window-setup-1/3 ()
  "Split window: 2 coulms, first with 1 row, second with 3 rows."
  (interactive)
  (delete-other-windows)

  ;; Compilation buffer
  (split-window-right)
  (other-window 1)
  (switch-to-buffer "*compilation*" nil t)

  ;; Magit-status buffer
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "what-i-did" nil t)

  ;; Notes buffer
  (split-window-below)
  (other-window 1)

  ;; Call magit-status here.
  ;; (magit-status (upward-find-file (buffer-file-name)) 'switch-to-buffer)

  ;; Focus the original buffer.
  (other-window -1)
  (other-window -1)
  (other-window -1)
  (balance-windows))

;; CSS color values colored by themselves
;; http://news.ycombinator.com/item?id=873541

(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]+"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))

(defun hexcolor-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolor-keywords))

;; (add-hook 'css-mode-hook 'hexcolor-add-to-font-lock)
