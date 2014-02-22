(yas-reload-all)

(defun yas-hook ()
  (yas-minor-mode))

(add-hook 'prog-mode-hook '(lambda () (yas-minor-mode)))
