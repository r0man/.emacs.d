(yas-reload-all)

(defun yas-hook ()
  (yas-minor-mode))

(add-hook 'html-mode-hook 'yas-hook)
(add-hook 'web-mode-hook 'yas-hook)
