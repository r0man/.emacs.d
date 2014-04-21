
(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(global-set-key (kbd "TAB") 'indent-or-complete)
(add-hook 'after-init-hook 'global-company-mode)
