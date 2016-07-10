;;; tabs.el --- Display tabs using the header line

;; Author: Mads Hartmann <mads379@gmail.com>
;; Maintainer: Mads Hartmann <mads379@gmail.com>
;; Created: 10 July 2016
;; Keywords: user-interface

;;; Commentary:
;;
;; This is a tabs manager that window specific.
;;
;; Window-parameters are used keep track of the state of each
;; window.  The following parameters are stored for each window:
;;
;;     'tabs-show :: A boolean value.  True if tabs should be shown.
;;     'tabs-list :: A list of buffers
;;     'tabs-active-tab-index :: A number representing the currently selected tab
;;
;; To change the look you should change the
;;
;;    header-line
;;    tabs-active-tab
;;    tabs-inactive-tab
;;
;;; Code:
;;
;; - TODO: The header-line-format is buffer-local. We want
;;         window-local.
;; - TODO: Change the tab face if it has git-changes, isn't saved etc.?
;; - TODO: Use a namespace?

;;; User Customizable Variables:

(defconst tabs-version "2016-07-12")

(defgroup tabs nil
  "WindowTabs -- Emacs window-specific tabs"
  :tag "WindowTabs"
  :group 'environment)

(defface tabs-active-tab
  '((t (:background "black"
        :foreground "Gray50"
        :height 110
        :slant italic
        :box (:line-width 4 :color "black" :style nil))))
  "Face for current tab."
  :group 'tabs)

(defface tabs-inactive-tab
  '((t (:foreground "black"
        :background "grey75"
        :height 110
        :box (:line-width 4 :color "grey75" :style nil))))
  "Face to fontify background of tab line."
  :group 'tabs)

;;; Key bindings:

(defvar tabs-keymap (make-sparse-keymap)
  "Keymap for ElScreen.")

;;; View:

(defun header-line-format-for-buffer (index buffer)
  "Get the header-line for the the INDEX BUFFER."
  (let ((tab (cond
             ((eq index (tabs-get-current-index)) (header-line-format-active-tab (buffer-name buffer)))
             (t (header-line-format-inactive-tab (buffer-name buffer)))))
        (spacing " "))
    (list tab spacing)))

(defun header-line-format-left-fringe ()
  "Header-line format for the fringe.

The header-line extends over the fringe.  This function creates
the header-line format that can be used to add left-padding in
order to not have tabs in the fringe."
  (concat
   (make-string (car (window-fringes)) 32)
   "  "))

(defun header-line-format-inactive-tab (name)
  "Header-line format for the active tab using NAME."
  (list (propertize name 'face 'tabs-inactive-tab)))

(defun header-line-format-active-tab (name)
  "Header-line format for the inactive tab using NAME."
  (list (propertize name 'face 'tabs-active-tab)))

(defun header-line-format-tabs ()
  "Header-line format for the tabs."
  (cl-mapcar 'header-line-format-for-buffer
          (number-sequence 0 (- (length (tabs-get-tabs)) 1))
          (tabs-get-tabs)))

(defun tabs-render ()
  "Render the UI."
  (setq header-line-format
        (list
         (header-line-format-left-fringe)
         (header-line-format-tabs))))

;;; Convenience

(defun wt/remove-at (index xs)
  "Remove an element at INDEX based from the list XS."
  (if (eq index 0)
      (cdr xs)
    (concatenate 'list (subseq xs 0 index) (subseq xs (+ 1 index)))))

;;; Model:

(defun tabs-set-current-index (index)
  "Set the currently selected tab to be INDEX."
  (set-window-parameter nil :tabs-current-index index)
  (tabs-render))

(defun tabs-get-current-index ()
  "Return the index of the currently selected tab."
  (window-parameter nil :tabs-current-index))

(defun tabs-get-tabs ()
  "Get the tabs of the current window."
  (window-parameter nil 'tabs-list))

(defun tabs-new-tab-with-buffer (buffer)
  "Create a new tab with BUFFER in the current window."
  (set-window-parameter nil 'tabs-list (cons buffer (tabs-get-tabs)))
  (tabs-render))

(defun tabs-close-tab (index)
  "Close the tab at the given INDEX."
  (set-window-parameter
   nil 'tabs-list
   (wt/remove-at (tabs-get-current-index) (tabs-get-tabs)))
  (tabs-set-current-index (- (tabs-get-current-index) 1))
  (tabs-render))

;;; Interactive functions:
;;  Functions that I expect a bufer to call.

(defun tabs-new-tab ()
  "Create a new tab using the default buffer."
  (interactive)
  (tabs-new-tab-with-buffer (get-buffer-create "*scratch*")))

(defun tabs-close-current-tab ()
  "Close the current tab."
  (interactive)
  (tabs-close-tab (tabs-get-current-index))
  (tabs-new-tab-with-buffer (get-buffer-create "*scratch*")))

(defun tabs-next-tab ()
  "Select the next (right) tab in the window."
  (interactive)
  (tabs-set-current-index (+ 1 (tabs-get-current-index))))

(defun tabs-previous-tab ()
  "Select the previous (left) tab in the window."
  (interactive)
  (tabs-set-current-index (- (tabs-get-current-index) 1)))

(defun tabs-select-tab-index (index)
  "Select the tab with the specific INDEX.

The index starts at 0"
  (interactive)
  (message "not implemented"))

(defun tabs-enable ()
  "Show tabs for the current window."
  (interactive)
  ;; TODO: If there are no tabs, add the current tab
  ;; TODO: Initialize some things.
  (tabs-render))

(defun tabs-disable ()
  "Don't show tabs for the current window."
  (interactive)
  (setq header-line-format nil))

;;;###autoload
(define-minor-mode tabs-mode
  "Minor mode for showing tabs." nil " tabs" tabs-keymap
  (if tabs-mode (tabs-enable) (tabs-disable)))

(provide 'tabs)

;;; tabs.el ends here
