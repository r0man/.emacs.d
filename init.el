;; Set file for customizations.
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

(require 'package)

;; Set the package sources.
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("elpa" . "http://tromey.com/elpa/")))

;; The packages.
(setq elpa-packages
      '(auctex
        auto-complete
        clojure-mode
        clojure-test-mode
        closure-template-html-mode
        color-theme
        css-mode
        elein
        emms
        find-file-in-project
        gist
        haml-mode
        inf-ruby
        json
        ruby-test-mode
        rvm
        sass-mode
        scss-mode
        slime-repl
        smart-tab
        smex
        starter-kit
        starter-kit-bindings
        starter-kit-js
        starter-kit-lisp
        starter-kit-ruby
        yaml-mode
        yasnippet-bundle))

;; Initialize the package system.
(package-initialize)

;; Refresh package archives when necessary.
(unless (file-exists-p "~/.emacs.d/elpa/archives")
  (package-refresh-contents))

;; Install all packages.
(dolist (package elpa-packages)
  (when (not (package-installed-p package))
    (package-install package)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/workspace/soundcloud-el"))

;; Delete trailing whitespace when saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable display of time, load level, and mail flag in mode lines.
(display-time)

;; If non-nil, `next-line' inserts newline to avoid `end of buffer' error.
(setq next-line-add-newlines nil)

;; Whether to add a newline automatically at the end of the file.
(setq require-final-newline t)

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

;; ;; Use custom color theme.
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

;; ANSI TERM - http://stackoverflow.com/a/3284268
;; C-x C-j - term line mode
;; C-c C-k - character mode
(defun switch-to-ansi-term ()
  (interactive)
  (let ((buffer (get-buffer "\*shell\*")))
    (if buffer (switch-to-buffer buffer)
      (ansi-term "/bin/bash" "shell"))))

;; AUTO-COMPLETE
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; COFFEE MODE
(let ((coffee-mode-directory "~/.emacs.d/coffee-mode"))
  (when (file-directory-p coffee-mode-directory)
    (add-to-list 'load-path coffee-mode-directory)
    (require 'coffee-mode)
    (defun coffee-mode-customization ()
      (setq coffee-tab-width 2
            tab-width 2)
      (define-key coffee-mode-map "\C-c\C-k" 'coffee-compile-buffer)
      (define-key coffee-mode-map "\C-c\C-r" 'coffee-compile-region))
    (add-hook 'coffee-mode-hook '(lambda () (coffee-mode-customization)))))

;;; COMPILE-MODE
(setq compilation-scroll-output 't)

;; CLOJURE-MODE
(defun define-clojure-indent-words ()
  (define-clojure-indent (DELETE 1))
  (define-clojure-indent (GET 1))
  (define-clojure-indent (POST 1))
  (define-clojure-indent (PUT 1))
  (define-clojure-indent (admin-db-test 1))
  (define-clojure-indent (analytic-db-test 1))
  (define-clojure-indent (api-admin-test 1))
  (define-clojure-indent (api-test 1))
  (define-clojure-indent (are 1))
  (define-clojure-indent (context 1))
  (define-clojure-indent (controller-test 1))
  (define-clojure-indent (database-test 1))
  (define-clojure-indent (datastore-test 1))
  (define-clojure-indent (dbtest 1))
  (define-clojure-indent (emits-once 1))
  (define-clojure-indent (ensure-open 1))
  (define-clojure-indent (expect 1))
  (define-clojure-indent (memcache-test 1))
  (define-clojure-indent (task-queue-test 1))
  (define-clojure-indent (uncountable 1))
  (define-clojure-indent (user-test 1)))

(add-hook 'clojure-mode-hook 'define-clojure-indent-words)

;; CLOJURESCRIPT
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(setq inferior-lisp-program "browser-repl")

;; CLOSURE-TEMPLATE-HTML-MODE
(require 'closure-template-html-mode)

;; CSS-MODE
(setq css-indent-offset 2)

;;; EMMS
(require 'emms-setup)
(emms-all)
(emms-default-players)

(require 'emms-player-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(emms-player-mpd-connect)

(require 'emms-source-file)
(setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(setq emms-player-mpd-music-directory (expand-file-name "~/Music"))

(let ((filename "~/.emms.el"))
  (when (file-exists-p filename)
    (load-file filename)))

;; FIND-FILE-IN-PROJECT
(setq ffip-patterns '("*.coffee" "*.clj" "*.cljs" "*.rb" "*.html" "*.el" "*.js" "*.rhtml"))

;; GIST
(setq gist-view-gist t)

;; RVM
(when (file-exists-p "/usr/local/rvm")
  (require 'rvm)
  (set 'rvm-executable (if (file-exists-p "~/.rvm/bin/rvm") "~/.rvm/bin/rvm" "/usr/local/bin/rvm"))
  (rvm-use-default)
  (setenv "rvm_path" "/usr/local/rvm"))

;; GIT-BLAME-MODE
(dolist (filename '("/usr/share/emacs/site-lisp/git-blame.el"
                    "/usr/share/git/emacs/git-blame.el"))
  (if (file-exists-p filename) (load-file filename)))

;; HASKELL-MODE
(require 'haskell-mode)
(require 'inf-haskell)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; HIPPIE EXPAND
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name))

;; JAVA

;; Indent Java annotaions.
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-04/msg00262.html
(add-hook
 'java-mode-hook
 '(lambda ()
    (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; MOZ-REPL
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun enable-moz-minor-mode ()
  (moz-minor-mode 1)
  (message "Moz minor mode enabled."))

(add-hook 'espresso-mode-hook 'enable-moz-minor-mode)
(add-hook 'javascript-mode-hook 'enable-moz-minor-mode)
(add-hook 'js-mode-hook 'enable-moz-minor-mode)

(defun reload-firefox-after-save-hook ()
  (add-hook
   'after-save-hook
   '(lambda ()
      (interactive)
      (require 'moz)
      (require 'slime)
      (comint-send-string (inferior-moz-process) "BrowserReload();")
      (message (format "Firefox reloaded via MozRepl. Take a look at your browser, for a shiny new world!")))
   'append 'local))

;; MULTI-TERM
(require 'multi-term)
(setq multi-term-program "/bin/bash"
      multi-term-dedicated-select-after-open-p t)

;; PAREDIT-MODE
(defadvice paredit-open-round (after paredit-open-round) ()
  "Don't insert space via paredit-open-round in non-lisp modes."
  (when (memq major-mode '(coffee-mode js2-mode))
    (backward-char)
    (backward-delete-char-untabify 1)
    (forward-char)))

(ad-activate 'paredit-open-round)

;; RCIRC
(if (file-exists-p "~/.rcirc.el") (load-file "~/.rcirc.el"))
(setq rcirc-default-nick "r0man"
      rcirc-default-user-name "r0man"
      rcirc-default-full-name "Roman Scherer"
      rcirc-server-alist '(("irc.freenode.net" :channels ("#clojure" "#clojureql" "#pallet")))
      rcirc-private-chat t
      rcirc-debug-flag t)
(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively) 8192)
            (rcirc-track-minor-mode 1)
            (flyspell-mode 1)))

;; SCSS-MODE
(setq scss-compile-at-save nil)

;; SMART-TAB
(setq smart-tab-using-hippie-expand t)
(dolist
    (hook
     '(LaTeX-mode-hook
       c-mode-hook
       css-mode-hook
       coffee-mode-hook
       emacs-lisp-mode-hook
       haml-mode-hook
       html-mode-hook
       java-mode-hook
       paredit-mode
       ruby-mode-hook
       slime-mode-hook
       yaml-mode-hook))
  (add-hook hook (lambda () (smart-tab-mode t))))

;; SMEX
(when (require 'smex nil t)
  (global-set-key (kbd "M-x") 'smex)
  (smex-initialize))

;; SQL-MODE
(let ((filename "~/.sql.el"))
  (when (file-exists-p filename)
    (load-file filename)))

(add-hook 'sql-interactive-mode-hook '(lambda () (setq truncate-lines t)))

;; SWANK-JS
(let ((directory "/usr/share/emacs/site-lisp/slime"))
  (when (file-exists-p directory)
    (add-to-list 'load-path directory)
    (add-to-list 'load-path (expand-file-name "~/.emacs.d"))
    (load-file (concat directory "/slime.el"))
    (slime-setup '(slime-repl slime-js))))

(defun slime-js-refresh-stylesheets ()
  (interactive)
  (slime-js-eval
   (format "SwankJS.refreshCSS('%s')"
           (replace-regexp-in-string
            "(')" "\\\\\\1"
            (if (string-match "\\.s?css$" (buffer-file-name))
                (replace-regexp-in-string
                 "\.css\.s?css$" ".css"
                 (replace-regexp-in-string "^.*/" "" (buffer-file-name)))
              "")))
   #'(lambda (v) (message "Reloading stylesheets."))))

(defun slime-js-refresh-stylesheets-after-save-hook ()
  (add-hook
   'after-save-hook
   '(lambda ()
      (interactive)
      (slime-js-refresh-stylesheets))
   'append 'local))

;; (add-hook 'css-mode-hook 'slime-js-refresh-stylesheets-after-save-hook)

;; TRAMP
(require 'tramp)
(tramp-set-completion-function
 "ssh"
 '((tramp-parse-shosts "~/.ssh/known_hosts")
   (tramp-parse-hosts "/etc/hosts")))

;; RUBY-TEST MODE
(require 'ruby-test-mode)
(setq ruby-test-ruby-executables '("/usr/local/rvm/rubies/ruby-1.9.2-p136/bin/ruby")
      ruby-test-rspec-executables '("/usr/local/rvm/gems/ruby-1.9.2-p136/bin/rspec"))

;; EMACS RAILS RELOADED
(setq rails/rspec-bundle/command "bundle exec rspec")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-rails-reloaded"))
(setq rails/webserver-bundle/default-type "webrick")
(require 'rails-autoload)

(defun switch-to-rails-runner-buffer ()
  (switch-to-buffer-other-window rails/runner/buffer-name)
  (other-window -1))

(defadvice rails/compile/current-method (after rails/compile/current-method-advice) ()
  "Switch to the rails runner buffer after running the method test."
  (switch-to-rails-runner-buffer))

(ad-activate 'rails/compile/current-method)

(defadvice rails/compile/single-file (after rails/compile/single-file-advice) ()
  "Switch to the rails runner buffer after running the file test."
  (switch-to-rails-runner-buffer))

(ad-activate 'rails/compile/single-file)

;; YASNIPPET
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt))

;; Fix pretty fns for javascript.
(eval-after-load 'js
  '(font-lock-add-keywords
    'js-mode `(("\\(function *\\)("
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "\u0192")
                          nil))))))

;; Don't use ido-ubiquitous yet. Breaks rgrep.
(setq ido-ubiquitous-enabled nil)

;; Start an ANSI terminal.
(multi-term-dedicated-open)

;; Load keyboard bindings (after everything else).
(load-file (expand-file-name "~/.emacs.d/roman/keyboard-bindings.el"))
(put 'ido-exit-minibuffer 'disabled nil)
