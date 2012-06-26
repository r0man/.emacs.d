
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
      '(ace-jump-mode
        auctex
        auto-complete
        clojure-mode
        clojure-test-mode
        color-theme
        css-mode
        elein
        emms
        expand-region
        find-file-in-project
        find-file-in-git-repo
        gist
        haml-mode
        inf-ruby
        json
        multi-term
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
        rainbow-delimiters
        undo-tree
        volatile-highlights
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

;; Enter debugger if an error is signaled?
(setq debug-on-error nil)

;; Use custom color theme.
(require 'color-theme)
(load-file "~/.emacs.d/color-theme-roman.el")
(color-theme-roman)

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/workspace/soundcloud-el"))

;; Hide scroll and tool bar, and show menu.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))

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
        (setenv "AWS_ACCESS_KEY" aws-access-key)
        (setenv "AWS_SECRET_KEY" aws-secret-key)
        (setenv "EC2_PRIVATE_KEY" (expand-file-name ec2-private-key))
        (setenv "EC2_CERT" (expand-file-name ec2-cert)))))

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

;; Highlight trailing whitespace
(setq show-trailing-whitespace t)

;; Enable cut-and-paste between Emacs and X clipboard.
(setq x-select-enable-clipboard t)

;; Controls the operation of the TAB key.
(setq tab-always-indent 'complete)

;; The maximum size in lines for term buffers.
(setq term-buffer-maximum-size (* 10 2048))

;; Do not add a final newline when saving.
(setq require-final-newline nil)

;; ACE-JUMP-MODE
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; AUTO-COMPLETE
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; BIG BROTHER DATABASE
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-gnus)

;;; COMPILE-MODE
(setq compilation-scroll-output 't)

;; Show colors in compilation buffers.
;; http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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
  (define-clojure-indent (web-test 1))
  (define-clojure-indent (datastore-test 1))
  (define-clojure-indent (dbtest 1))
  (define-clojure-indent (emits-once 1))
  (define-clojure-indent (ensure-open 1))
  (define-clojure-indent (expect 1))
  (define-clojure-indent (memcache-test 1))
  (define-clojure-indent (task-queue-test 1))
  (define-clojure-indent (uncountable 1))
  (define-clojure-indent (user-test 1)))

(put 'defcontrol 'clojure-backtracking-indent '((2)))
(put 'defcomponent 'clojure-backtracking-indent '((2)))
(put 'defrenderer 'clojure-backtracking-indent '((2)))
(put 'defclass 'clojure-backtracking-indent '(4 (2)))
(put 'defprovider 'clojure-backtracking-indent '(4 (2)))

(add-hook 'clojure-mode-hook 'define-clojure-indent-words)

;; CLOJURESCRIPT
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(setq inferior-lisp-program "lein trampoline cljsbuild repl-launch chromium")

(defun lein-cljs-build ()
  (interactive)
  (compile "lein clean; lein cljsbuild auto"))

;; CSS-MODE
(setq css-indent-offset 2)

;; DIRED

;; Switches passed to `ls' for Dired.  MUST contain the `l' option.
(setq dired-listing-switches "-alh")

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

(add-to-list
 'emms-stream-default-list
 '("SomaFM: Space Station" "http://www.somafm.com/spacestation.pls" 1 streamlist))

;; ERLANG
(let ((directory "/usr/lib/erlang/lib/tools-2.6.7/emacs/"))
  (when (file-exists-p directory)
    (add-to-list 'load-path directory)
    (add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
    (setq erlang-root-dir "/usr/lib/erlang")
    (setq inferior-erlang-machine-options '("-sname" "emacs"))))

;; EXPAND-REGION
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;; DISTEL
(let ((directory "/usr/share/distel/elisp"))
  (when (file-exists-p directory)
    (add-to-list 'load-path directory)
    (require 'distel)
    (distel-setup)))

;; FIND-DIRED
(defun find-dired-clojure (dir)
  "Run find-dired on Clojure files."
  (interactive (list (read-directory-name "Run find (Clojure) in directory: " nil "" t)))
  (find-dired dir "-name \"*.clj\""))

(defun find-dired-ruby (dir)
  "Run find-dired on Ruby files."
  (interactive (list (read-directory-name "Run find (Ruby) in directory: " nil "" t)))
  (find-dired dir "-name \"*.rb\""))

;; FIND-FILE-IN-GIT-REPO
(require 'find-file-in-git-repo)

;; FIND-FILE-IN-PROJECT
(setq ffip-patterns '("*.coffee" "*.clj" "*.cljs" "*.rb" "*.html" "*.el" "*.js" "*.rhtml" "*.java"))

;; GIST
(setq gist-view-gist t)

;; RVM
(when (file-exists-p "/usr/local/rvm")
  (require 'rvm)
  (rvm-use-default))

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

;; Indent Java annotations.
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-04/msg00262.html
(add-hook
 'java-mode-hook
 '(lambda ()
    (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; MINGUS
(add-to-list 'load-path "~/.emacs.d/mingus")
(autoload 'mingus "mingus-stays-home" nil t)

;; MULTI-TERM
(require 'multi-term)
(setq multi-term-program "/bin/bash"
      multi-term-dedicated-select-after-open-p t
      multi-term-dedicated-window-height 25)

;; Enable compilation-shell-minor-mode in multi term.
;; http://www.masteringemacs.org/articles/2012/05/29/compiling-running-scripts-emacs/
;; (add-hook 'term-mode-hook 'compilation-shell-minor-mode)

(dolist
    (bind '(
            ("<S-down>" . multi-term)
            ("<S-left>" . multi-term-prev)
            ("<S-right>" . multi-term-next)
            ("C-<backspace>" . term-send-backward-kill-word)
            ("C-<delete>" . term-send-forward-kill-word)
            ("C-<left>" . term-send-backward-word)
            ("C-<right>" . term-send-forward-word)
            ("C-c C-j" . term-line-mode)
            ("C-c C-k" . term-char-mode)
            ("C-y" . term-paste)
            ("C-z" . term-stop-subjob)
            ("M-d" . term-send-forward-kill-word)
            ("M-DEL" . term-send-backward-kill-word)))
  (add-to-list 'term-bind-key-alist bind))

(defun last-term-mode-buffer (list-of-buffers)
  "Returns the most recently used term-mode buffer."
  (when list-of-buffers
    (if (eq 'term-mode (with-current-buffer (car list-of-buffers) major-mode))
        (car list-of-buffers) (last-term-mode-buffer (cdr list-of-buffers)))))

(defun switch-to-term-mode-buffer ()
  "Switch to the most recently used term-mode buffer, or create a
new one."
  (interactive)
  (let ((buffer (last-term-mode-buffer (buffer-list))))
    (if (not buffer)
        (multi-term)
      (switch-to-buffer buffer))))

;; NUGG.AD
(let ((filename "~/.nuggad.el"))
  (when (file-exists-p filename)
    (load-file filename)))

;; RCIRC
(if (file-exists-p "~/.rcirc.el") (load-file "~/.rcirc.el"))
(setq rcirc-default-nick "r0man"
      rcirc-default-user-name "r0man"
      rcirc-default-full-name "Roman Scherer"
      rcirc-server-alist '(("irc.freenode.net" :channels ("#clojure" "#pallet")))
      rcirc-private-chat t
      rcirc-debug-flag t)
(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively) 8192)
            (rcirc-track-minor-mode 1)
            (flyspell-mode 1)))

;; SCSS-MODE
(setq scss-compile-at-save nil)

;; SLIME
(defun slime-common-lisp ()
  (interactive)
  (setq inferior-lisp-program "sbcl")
  (delete (expand-file-name "~/.emacs.d/elpa/slime-20100404.1") load-path)
  (delete (expand-file-name "~/.emacs.d/elpa/slime-repl-20100404") load-path)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
  (load-file "/usr/share/emacs/site-lisp/slime/slime.el")
  (slime-setup '(slime-repl))
  (slime))

(defun slime-clojure ()
  (interactive)
  (delete (expand-file-name "/usr/share/emacs/site-lisp/slime") load-path)
  (add-to-list 'load-path "~/.emacs.d/elpa/slime-20100404.1")
  (add-to-list 'load-path "~/.emacs.d/elpa/slime-repl-20100404")
  (load-file "~/.emacs.d/elpa/slime-20100404.1/slime.el")
  (slime-setup '(slime-repl))
  (slime-connect "localhost" 4005))

;; SMART-TAB
(setq smart-tab-using-hippie-expand t)
(dolist
    (hook
     '(LaTeX-mode-hook
       c-mode-hook
       coffee-mode-hook
       css-mode-hook
       emacs-lisp-mode-hook
       haml-mode-hook
       html-mode-hook
       java-mode-hook
       paredit-mode-hook
       ruby-mode-hook
       slime-mode-hook
       sql-mode-hook
       yaml-mode-hook))
  (add-hook hook (lambda () (smart-tab-mode t))))

;; SQL-MODE
(let ((filename "~/.sql.el"))
  (when (file-exists-p filename)
    (load-file filename)))

;; SQL-INDENT
(require 'sql-indent)
(setq sql-indent-offset 2)

;; SQL-TRANSFORM
(require 'sql-transform)
(add-hook 'sql-mode-hook (function (lambda () (local-set-key "\C-cd" 'sql-to-delete))))
(add-hook 'sql-mode-hook (function (lambda () (local-set-key "\C-cu" 'sql-to-update))))
(add-hook 'sql-mode-hook (function (lambda () (local-set-key "\C-cs" 'sql-to-select))))
(add-hook 'sql-mode-hook
          (function (lambda ()
                      (master-mode t)
                      (master-set-slave sql-buffer))))

(add-hook 'sql-set-sqli-hook (function (lambda () (master-set-slave sql-buffer))))

;; If you don’t like window splittings related to the SQL buffer, try
;; the following, per Force Same Window (Manual).
;; (add-to-list 'same-window-buffer-names "*SQL*")

;; TRAMP
(require 'tramp)
(tramp-set-completion-function
 "ssh"
 '((tramp-parse-shosts "~/.ssh/known_hosts")
   (tramp-parse-hosts "/etc/hosts")))

;; RAINBOW DELIMITERS
(global-rainbow-delimiters-mode)

;; RUBY-TEST MODE
(require 'ruby-test-mode)
(setq ruby-test-ruby-executables '("/usr/local/rvm/rubies/ruby-1.9.2-p180/bin/ruby")
      ruby-test-rspec-executables '("bundle exec rspec"))
(setq ruby-test-ruby-executables '("/usr/local/rvm/rubies/ruby-1.9.3-p194/bin/ruby")
      ruby-test-rspec-executables '("bundle exec rspec"))

;; UNDO TREE
(require 'undo-tree)

;; VOLATILE HIGHLIGHTS
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; YASNIPPET
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt))
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/roman/snippets")
(yas/load-directory yas/root-directory)

;; Fix pretty fns for javascript.
(eval-after-load 'js
  '(font-lock-add-keywords
    'js-mode `(("\\(function *\\)("
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "\u0192")
                          nil))))))

;; EMAIL

;; Send mail via smtpmail.
(setq send-mail-function 'smtpmail-send-it)

;; The name of the host running SMTP server.
(setq smtpmail-smtp-server "smtp.gmail.com")

;; SMTP service port number.
(setq smtpmail-smtp-service 587)

;; Type of SMTP connections to use.
(setq smtpmail-stream-type 'starttls)

;; Whether to print info in buffer *trace of SMTP session to <somewhere>*.
(setq smtpmail-debug-info t)

;; User name to use when looking up credentials in the authinfo file.
(setq smtpmail-smtp-user user-mail-address)

;; GNUS

;; Default method for selecting a newsgroup.
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; A regexp to match uninteresting newsgroups. Use blank string for Gmail.
(setq gnus-ignored-newsgroups "")

;; Start a terminal.
(multi-term)

;; Load keyboard bindings (after everything else).
(load-file (expand-file-name "~/.emacs.d/roman/keyboard-bindings.el"))
