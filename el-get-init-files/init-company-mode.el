(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-tooltip-selection ((t (:inherit default :background ,(color-lighten-name bg 6)))))
   `(company-scrollbar-bg ((t (:inherit default :background ,(color-lighten-name bg 4)))))
   `(company-scrollbar-fg ((t (:inherit default :background ,(color-lighten-name bg 8)))))
   `(company-tooltip-common ((t (:inherit default :background ,bg))))
   `(company-tooltip-common-selection ((t (:inherit default :background ,(color-lighten-name bg 8)))))))

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(global-set-key (kbd "TAB") 'indent-or-complete)

(add-hook 'after-init-hook 'global-company-mode)
