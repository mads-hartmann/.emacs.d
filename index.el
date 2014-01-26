;;
;; I like to maintain an ctags index of all of my projects
;;
;; I use the TAGS file to provide the following functionality
;;
;;   * Jump to definition under the cursor
;;   * Jump to any definition (using ido)
;;   * Open any file matching string (using ido)
;;
;;
;;

(defconst ctags-path "/usr/local/Cellar/ctags/5.8/bin/ctags")

;;
;; Maintaining the index
;;

(defun forget-current-tags-table ()
  "Forget everything we know about the current tags-table."
  (if (get-buffer "TAGS")
      (kill-buffer "TAGS"))
  (tags-reset-tags-tables)
  (setq tags-completion-table nil))

(defun focus-project-containing-file (&optional path)
  "Resets the tags-table, clear the 'tags-completion-table' cache
   and visits the TAGS file closest (upwards) to 'path'. If no
   value for 'path' is supplied it will use the current file."
  (interactive)
  (let ((dir (if path path (upward-find-file "TAGS"))))
    (forget-current-tags-table)
    (visit-tags-table (concat dir))))

(defun index-current-project ()
  "TODO: Write documentation for this sucker"
  (interactive)
  (let ((d (upward-find-file ".gitignore")))
    (message "dir is %s" d)
    (if d
        (let* ((dir (or (upward-find-file ".gitignore") "."))
               (ignored (read-lines (concat dir "/.gitignore")))
               (ignored-args-str (mapconcat (lambda (i) (concat "--exclude=" i)) ignored " "))
               (compile-command (format "cd %s && %s -e -R %s . TAGS 2>/dev/null" dir ctags-path ignored-args-str)))
          (forget-current-tags-table)
          (shell-command compile-command)
          (focus-project-containing-file dir)
          (message "Done indexing project"))
      (message "No .gitignore file found."))))

;;
;; Hooks
;;

(add-hook 'after-save-hook 'index-current-project)

;;
;; Navigation
;;

;;
;; TODO: When any of these functions are called we want to check that
;; the current file is in the active tags-table, otherwise we should
;; load that instead.
;;
;; This might not be disreable when looking at attached sources
;; and the likes?
;;

(defun ido-find-file-in-tag-files ()
  "Find a file listed in the current tag file. From Stuart
   Halloway's 'What You Can Learn From ido.el' screencast."
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t)) (visit-tags-table-buffer))
    (find-file (expand-file-name
                (ido-completing-read "Project file: "
                                     (tags-table-files) nil t)))))

(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-tag-in-file ()
  "Jump to a tags entry in the currently active file."
  ;; Possible optimization: Use something other than find-tag
  ;; as it performs a completely new tags search from scratch.
  ;; There's really no need for that as we have already 'found'
  ;; the tag when producing the completion-list.
  (interactive)
  (let* ((symbols (symbol-in-file-completion-list "index.el"))
         (selected (ido-completing-read "Tag: " symbols)))
    (message selected)
    (find-tag selected)))

(defun symbol-in-file-completion-list (file)
  "Generate a completion list of available symbols in the
   currently active file based on the associated tags-table"
  (save-excursion
    (let* ((enable-recursive-minibuffers t)
           (files nil))
      (visit-tags-table-buffer)
      (goto-char (point-min))
      (let* ((beginning (search-forward "index.el" nil t))
             (end (re-search-forward "" nil t)))
        (goto-char beginning)
        (while (and (re-search-forward "\\(.*\\)" nil t)
                    (<= (point) end))
          (push (buffer-substring (match-beginning 1)
                                  (match-end 1))
                files))
        files))))
