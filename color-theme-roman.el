(require 'color-theme)

;;;###autoload
(defun color-theme-roman ()
  "Color theme by Roman Scherer."
  (interactive)
  (color-theme-install
   '(color-theme-roman
     ()
     ()
     (mode-line ((t (:background "black" :foreground "white" :box (:line-width -1 :style released-button)))))
     (fringe ((t (:background "white")))))))

(add-to-list 'color-themes '(color-theme-roman  "Roman" "Roman Scherer"))

(provide 'color-theme-roman)
