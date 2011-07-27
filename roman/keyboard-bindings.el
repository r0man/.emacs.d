;; Restore some default key bindings overridden by emacs starter kit.
(global-set-key (kbd "C-x h") 'mark-whole-buffer)
(global-set-key (kbd "C-x ^") 'enlarge-window)
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)
(global-unset-key (kbd "C-x g"))

(global-set-key (kbd "C-c C-n") 'durendal-sort-ns)
(global-set-key (kbd "C-c C-s") 'swap-windows)
(global-set-key (kbd "C-x C-g b") 'mo-git-blame-current)
(global-set-key (kbd "C-x C-g s") 'magit-status)
(global-set-key (kbd "C-x I") 'indent-buffer)
(global-set-key (kbd "C-x TAB") 'indent-rigidly)
(global-set-key [f5] 'compile)

(provide 'keyboard-bindings)
