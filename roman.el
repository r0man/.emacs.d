(setq elpa-packages
      '(auto-complete
        clojure-mode
        clojure-test-mode
        closure-template-html-mode
        css-mode
        color-theme
        durendal
        find-file-in-project
        gist
        haml-mode
        json
        rvm
        sass-mode
        slime-repl
        smart-tab
        starter-kit
        starter-kit-bindings
        starter-kit-js
        starter-kit-lisp
        starter-kit-ruby
        yaml-mode
        yasnippet-bundle))

(defun install-elpa-packages ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (let ((refreshed nil))
    (dolist (package elpa-packages)
      (unless (package-installed-p package)
        (unless refreshed
          (message "Refreshing the ELPA archive.")
          (setq refreshed t)
          (package-refresh-contents))
        (message "Installing %s" (symbol-name package))
        (package-install package)))))

;; Install ELPA packages.
; (install-elpa-packages)

;; Delete trailing whitespace when saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; AMAZON WEB SERVICES
(let ((aws-credentials (expand-file-name "~/.aws.el")))
  (if (file-exists-p aws-credentials)
      (progn
        (load-file aws-credentials)
        (setenv "AWS_ACCOUNT_NUMBER" aws-account-number)
        (setenv "AWS_ACCESS_KEY_ID" aws-access-key-id)
        (setenv "AWS_SECRET_ACCESS_KEY" aws-secret-access-key)
        (setenv "EC2_PRIVATE_KEY" (expand-file-name ec2-private-key))
        (setenv "EC2_CERT" (expand-file-name ec2-cert))
        (setenv "S3_ACCESS_KEY" aws-access-key-id)
        (setenv "S3_SECRET_KEY" aws-secret-access-key))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

;; Show the menu-bar.
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))

;; Use custom color theme.
(require 'color-theme)
(load-file "~/.emacs.d/color-theme-roman.el")
(color-theme-roman)

;; Highlight trailing whitespace
(setq show-trailing-whitespace t)

;; Enable cut-and-paste between Emacs and X clipboard.
(setq x-select-enable-clipboard t)

;; Controls the operation of the TAB key.
(setq tab-always-indent 'complete)

;; Do not add a final newline when saving.
(setq require-final-newline nil)

;; AUTO-COMPLETE
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
