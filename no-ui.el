;; 
;; Configuration this is speicifc to when emacs is loaded inside of a
;; Terminal
;; 

(menu-bar-mode -1)

(set-face-attribute  'mode-line
                     nil
                     :foreground "red"
                     :background "white"
                     :box '(:line-width 1 :style released-button))
  
(set-face-attribute  'mode-line-inactive
                     nil
                     :foreground "gray30"
                     :background "red"
                     :box '(:line-width 1 :style released-button))
