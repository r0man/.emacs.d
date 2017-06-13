(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#141b23")
 '(background-mode dark)
 '(cider-cljs-lein-repl
   "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))")
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(cider-repl-wrap-history t)
 '(column-number-mode t)
 '(cursor-color "#839496")
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(display-time-mode t)
 '(foreground-color "#839496")
 '(package-selected-packages
   (quote
    (org-plus-contrib timesheet plantuml plantuml-mode org org-ac terraform-mode flycheck-flow flycheck indium ensime haskell-mode helm-projectile avy-menu solarized-theme helm yaml-mode which-key web-mode virtualenvwrapper use-package soundklaus smooth-scrolling smex slime slamhound scss-mode sayid rainbow-mode projectile pretty-lambdada popwin multi-term markdown-preview-mode markdown-preview-eww magit ido-vertical-mode ido-ubiquitous hy-mode graphql-mode github-browse-file flymd flx-ido expand-region exec-path-from-shell engine-mode elpy elisp-slime-nav ein company-quickhelp color-theme clojure-mode-extra-font-locking clj-refactor cask avy auto-dictionary)))
 '(safe-local-variable-values
   (quote
    ((bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
     (cider-cljs-lein-repl . "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
     (emacs-lisp-docstring-fill-column . 75)
     (encoding . binary))))
 '(timesheet-invoice-number 108))
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
 '(ediff-fine-diff-A ((t (:background "#aa2222" :foreground "white"))))
 '(ediff-fine-diff-B ((t (:background "#22aa22" :foreground "white"))))
 '(ediff-fine-diff-C ((t (:background "#aaaa22" :foreground "white"))))
 '(ert-test-result-expected ((t (:background "green3" :foreground "white smoke"))))
 '(helm-M-x-key ((t (:foreground "#839496" :underline t :weight bold))))
 '(helm-selection ((t (:background "#586e75" :foreground "white smoke"))))
 '(helm-source-header ((t (:foreground "white smoke" :weight bold :height 1.1 :family "Sans Serif"))))
 '(mode-line ((t (:background "white smoke" :foreground "black"))))
 '(mode-line-inactive ((t (:background "dark gray" :foreground "black"))))
 '(vertical-border ((t (:foreground "black")))))
