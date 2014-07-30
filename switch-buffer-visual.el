;;; switch-buffer-visual.el --- Pretty buffer switching

(defun read-number ()
  (string-to-number (char-to-string (read-char "Window: "))))

(defun indexed-windows ()
  (let ((counter 0))
    (mapcar (lambda (win)
              (incf counter)
              (list counter win)) (window-list))))

(defun full-size-overlay (counter-win-tuple)
  (let* ((counter (nth 0 counter-win-tuple))
         (win (nth 1 counter-win-tuple))
         (buf (window-buffer win))
         (start 0)
         (end most-positive-fixnum)
         (ol (make-overlay start end buf)))
    (overlay-put ol 'face 'ace-jump-face-background)
    (overlay-put ol 'display (number-to-string counter))
    ol))

(defun switch-buffer-visual ()
  (interactive)
  (let* ((overlays (mapcar 'full-size-overlay (indexed-windows)))
         (selected (- (read-number) 1))
         (buf (window-buffer (nth selected (window-list)))))
    (mapc 'delete-overlay overlays)
    (pop-to-buffer buf)))

;;
;; Dev helpers
;;

(defun panic-delete-all-overlays ()
  "This was very, very useful when developing this - Made it
possible to clean up after my mess."
  (interactive)
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (mapc 'delete-all-overlays buffers)))
