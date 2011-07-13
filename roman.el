;; PACKAGE
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("elpa" . "http://tromey.com/elpa/")))

(setq elpa-packages
      '(clojure-mode
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
(install-elpa-packages)
