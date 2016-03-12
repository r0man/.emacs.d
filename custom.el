(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#141b23")
 '(background-mode dark)
 '(cider-cljs-lein-repl
   "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
 '(column-number-mode t)
 '(cursor-color "#839496")
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(display-time-mode t)
 '(foreground-color "#839496")
 '(helm-M-x-fuzzy-match t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-mode t)
 '(safe-local-variable-values
   (quote
    ((bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
     (cider-cljs-lein-repl . "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
     (emacs-lisp-docstring-fill-column . 75)
     (encoding . binary)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 143 :width normal))))
 '(Man-overstrike ((t (:foreground "#d33682" :background "#141b23" :inverse-video nil))))
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "darkgray"))))
 '(company-scrollbar-fg ((t (:background "black"))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(cursor ((t (:background "orange red" :foreground "white"))))
 '(ediff-current-diff-C ((t (:background "black"))))
 '(ert-test-result-expected ((t (:background "green3" :foreground "white smoke"))))
 '(helm-M-x-key ((t (:foreground "#839496" :underline t :weight bold))))
 '(helm-selection ((t (:background "#586e75" :foreground "white smoke"))))
 '(helm-source-header ((t (:foreground "white smoke" :weight bold :height 1.1 :family "Sans Serif"))))
 '(mode-line ((t (:background "white smoke" :foreground "black"))))
 '(mode-line-inactive ((t (:background "dark gray" :foreground "black"))))
 '(vertical-border ((t (:foreground "black")))))
