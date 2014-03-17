
(defun enable-clj-refactor-mode ()
  (clj-refactor-mode 1))

(add-hook 'clojure-mode-hook 'enable-clj-refactor-mode)
(cljr-add-keybindings-with-prefix "C-c C-x")
