;;; functions.el -- Random functions
;;; Commentary:
;;; Code:

(defun re-read-init-file ()
  "Reread ~/.emacs.d/init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun toggle-maximize-buffer ()
  "Maximize buffer - in a nice iTerm style way."
  (interactive)
  (if (= 1 (length (window-list)))
    (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain FILENAME.

If we manage to find it, return the containing directory.  Else if we
get to the toplevel directory and still can't find it, return
nil.  Start at STARTDIR or default to CWDIR if STARTDIR is not given"

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
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun refresh-safari ()
  "Refresh safari browser.

This is done by executing a shell-script, that executes an
applescript, that refreshes the active tab in the front-most safari
window, in the background."
  (interactive)
  (shell-command "source ~/.emacs.d/shell-functions/refresh-safari.sh"))

(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun string/starts-with (s begins)
      "Return non-nil if string S begins with BEGINS.  Else nil."
      (cond ((>= (length s) (length begins))
             (string-equal (substring s 0 (length begins)) begins))
            (t nil)))

(defun clear-shell ()
  "Clear the current shell."
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))

(defun ido-complete-word-ispell ()
  "Completes the symbol at point based on entries in the dictionary."
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

(defun insert-aa ()
  "Insert the character å."
  (interactive)
  (insert "å"))

(defun insert-ae ()
  "Insert the character æ."
  (interactive)
  (insert "æ"))

(defun insert-oe ()
  "Insert the character ø."
  (interactive)
  (insert "ø"))

(defun prev-window ()
  "Focus the previous window."
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
(defun camel-to-snake ()
  (interactive)
  (let* ((word (thing-at-point 'symbol))
         (boundaries (bounds-of-thing-at-point 'symbol))
         (start (car boundaries))
         (end (cdr boundaries))
         (case-fold-search nil))
    (while (string-match "[A-Z]" word 1)
      (setq word (replace-match (concat "_" (downcase (match-string 0 word)))
                                t nil word)))
    (delete-region start end)
    (insert (downcase word))))

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun mhj-org-publish-project-keep-location ()
  (interactive)
  (save-excursion
    (org-publish-current-project)))

(defun comment-line-dwim (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

;; http://oremacs.com/2015/01/26/occur-dwim/
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

(defun json-format ()
  "Use python to pretty-print the json in the selected region"
  (interactive)
  (when (region-active-p)
    (shell-command-on-region (region-beginning) (region-end) "python -m json.tool" nil t)))

;; http://emacsredux.com/blog/2015/01/18/clear-comint-buffers/
(defun comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/compare/%s"
     (replace-regexp-in-string
      "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
      (magit-get "remote"
                 (magit-get-current-remote)
                 "url"))
     (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'endless/visit-pull-request-url))

;;; functions.el ends here
