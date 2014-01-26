;;
;; I find (c/e)tags to be really useful when navigating projects of
;; almost any size. This file contains a few functions that improve
;; the way I prefer use TAGS.
;;
;;   * Jump to any definition in the project (using ido)
;;   * Jump to any definition in the currently opened buffer (using ido)
;;   * Open any file in the current project (using ido)
;;   * (TODO) Complete word at point (code-completion) (using ido)
;;
;; Most of the above listed functions are supported by etags out of
;; the box, but the functions below uses some conventions that makes
;; using tags so much more enjoyable (IMO).
;;
;;   * A project is defined by having a .gitignore file
;;   * When indexing your project it will skip any folders that are
;;     excluded in your .gitignore file.
;;   * The entire index is re-built whenever you save a file. This
;;     might not be optimal if you work on large projects but
;;     from my experience it's very convenient.
;;   * (TODO) If the current buffer isn't part of the currently
;;     active tags-table it will clear all tags-related caches
;;     and load the tags-table that is associated wih the given
;;     file (this only happens if such a TAGS file exist)
;;
;; This configuration works well for me.
;;
;; /Mads Hartmann
;;

(defconst ctags-path "/usr/local/Cellar/ctags/5.8/bin/ctags")

(defun create-index-cmd-str (dir ignore)
  "Shell command used to generate the TAGS file"
  (format "cd %s && %s -e -R %s . TAGS 2>/dev/null"
          dir
          ctags-path
          ignore))

(defun forget-current-tags-table ()
  "Forget everything we know about the current tags-table."
  (if (get-buffer "TAGS")
      (kill-buffer "TAGS"))
  (tags-reset-tags-tables)
  (setq tags-completion-table nil))

(defun focus-project-containing-file (&optional path)
  "Resets the currently active tags-table and visits the TAGS
   file closest (upwards) to 'path'. If no value for 'path' is
   supplied it will start the search at the directory containing
   the currently opened file."
  (interactive)
  (let ((dir (if path path (upward-find-file "TAGS"))))
    (forget-current-tags-table)
    (visit-tags-table dir)))

(defun index-current-project ()
  "Creates a TAGS file for the project that contains the
   currently opened file.

   It will clear the current tags-table and load the newly
   generated TAGS file."
  (interactive)
  (let ((dir (upward-find-file ".gitignore")))
    (if dir
        (let* ((ignored (read-lines (concat dir "/.gitignore")))
               (ignored-args-str (mapconcat (lambda (i) (concat "--exclude=" i)) ignored " "))
               (index-cmd (create-index-cmd-str dir ignored-args-str)))
          (forget-current-tags-table)
          (shell-command index-cmd)
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

(defun ido-find-file-in-tag-files ()
  "Find a file listed in the current tag file. From Stuart
   Halloway's 'What You Can Learn From ido.el' screencast."
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file (expand-file-name
                (ido-completing-read "Project file: "
                                     (tags-table-files) nil t)))))

(defun ido-find-tag ()
  "Jump to any tag in the project using ido."
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-tag-in-file ()
  "Jump to any tag in the currently active file."
  ;; Possible optimization: Use something other than find-tag
  ;; as it performs a completely new tags search from scratch.
  ;; There's really no need for that as we have already 'found'
  ;; the tag when producing the completion-list.
  (interactive)
  (let* ((file-name (buffer-name))
         (symbols (symbol-in-file-completion-list file-name)))
    (if symbols
        (find-tag (ido-completing-read "Tag: " symbols))
      (message "No symbols in current file, sorry."))))

(defun symbol-in-file-completion-list (file-name)
  "Generates a completion list of available symbols in the
   currently active file based on the associated tags-table"
  (save-excursion
    (let* ((enable-recursive-minibuffers t)
           (symbol-names nil))
      (visit-tags-table-buffer)
      (goto-char (point-min))
      (let* ((beginning (search-forward file-name nil t))
             (end (re-search-forward "" nil t)))
        (goto-char beginning)
        (while (and (re-search-forward "\\(.*\\)" nil t)
                    (<= (point) end))
          (push (buffer-substring (match-beginning 1)
                                  (match-end 1))
                symbol-names))
        symbol-names))))

;; Random notes
;;
;; TODO: It might be nice to allow all the navigation functions to
;; automatically focus the TAGS file associated with the file. This
;; might be annoying when browsing attatched code (maybe it should only
;; do it if there is already a TAGS file?)
;;
