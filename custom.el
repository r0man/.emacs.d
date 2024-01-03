(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#141b23")
 '(background-mode dark)
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(cider-repl-wrap-history t)
 '(column-number-mode t)
 '(cursor-color "#839496")
 '(custom-safe-themes
   '("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(display-time-mode t)
 '(foreground-color "#839496")
 '(package-selected-packages
   '(company org-plus-contrib timesheet plantuml plantuml-mode org org-ac terraform-mode flycheck-flow flycheck indium ensime haskell-mode helm-projectile avy-menu solarized-theme helm yaml-mode which-key web-mode virtualenvwrapper use-package soundklaus smooth-scrolling smex slime slamhound scss-mode sayid rainbow-mode projectile pretty-lambdada popwin multi-term markdown-preview-mode markdown-preview-eww magit ido-vertical-mode ido-ubiquitous hy-mode graphql-mode github-browse-file flymd flx-ido expand-region exec-path-from-shell engine-mode elpy elisp-slime-nav ein company-quickhelp color-theme clojure-mode-extra-font-locking clj-refactor cask avy auto-dictionary))
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 0)
      (thread-last . 0)
      (transient-define-prefix . defmacro)
      (transient-define-suffix . defmacro))
     (checkdoc-package-keywords-flag)
     (geiser-guile-binary "guix" "repl")
     (geiser-repl-per-project-p . t)
     (eval progn
           (require 'lisp-mode)
           (defun emacs27-lisp-fill-paragraph
               (&optional justify)
             (interactive "P")
             (or
              (fill-comment-paragraph justify)
              (let
                  ((paragraph-start
                    (concat paragraph-start "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                   (paragraph-separate
                    (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
                   (fill-column
                    (if
                        (and
                         (integerp emacs-lisp-docstring-fill-column)
                         (derived-mode-p 'emacs-lisp-mode))
                        emacs-lisp-docstring-fill-column fill-column)))
                (fill-paragraph justify))
              t))
           (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph))
     (eval with-eval-after-load 'yasnippet
           (let
               ((guix-yasnippets
                 (expand-file-name "etc/snippets/yas"
                                   (locate-dominating-file default-directory ".dir-locals.el"))))
             (unless
                 (member guix-yasnippets yas-snippet-dirs)
               (add-to-list 'yas-snippet-dirs guix-yasnippets)
               (yas-reload-all))))
     (eval add-to-list 'completion-ignored-extensions ".go")
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval let
           ((root-dir-unexpanded
             (locate-dominating-file default-directory ".dir-locals.el")))
           (when root-dir-unexpanded
             (let*
                 ((root-dir
                   (expand-file-name root-dir-unexpanded))
                  (root-dir*
                   (directory-file-name root-dir)))
               (unless
                   (boundp 'geiser-guile-load-path)
                 (defvar geiser-guile-load-path 'nil))
               (make-local-variable 'geiser-guile-load-path)
               (require 'cl-lib)
               (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval setq-local guix-directory
           (locate-dominating-file default-directory ".dir-locals.el"))
     (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
     (cider-cljs-lein-repl . "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
     (emacs-lisp-docstring-fill-column . 75)
     (encoding . binary)))
 '(timesheet-invoice-number 108))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-overstrike ((t (:foreground "#d33682" :background "#141b23" :inverse-video nil))))
 '(cursor ((t (:background "orange red" :foreground "white"))))
 '(ediff-current-diff-C ((t (:background "black"))))
 '(ediff-fine-diff-A ((t (:background "#aa2222" :foreground "white"))))
 '(ediff-fine-diff-B ((t (:background "#22aa22" :foreground "white"))))
 '(ediff-fine-diff-C ((t (:background "#aaaa22" :foreground "white"))))
 '(ert-test-result-expected ((t (:background "green3" :foreground "white smoke"))))
 '(helm-M-x-key ((t (:foreground "#839496" :underline t :weight bold))))
 '(helm-locate-finish ((t (:foreground "#859900"))))
 '(helm-selection ((t (:background "#586e75" :foreground "white smoke"))))
 '(helm-source-header ((t (:foreground "white smoke" :weight bold :height 1.1 :family "Sans Serif"))))
 '(mode-line ((t (:background "white smoke" :foreground "black"))))
 '(mode-line-inactive ((t (:background "dark gray" :foreground "black"))))
 '(vertical-border ((t (:foreground "black")))))
