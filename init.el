;; Hide scroll, tool and menu bars.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

;; Set file for customizations.
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; Custom faces.
(custom-set-faces
 '(cursor ((t (:background "orange red" :foreground "white"))))
 '(mode-line ((t (:background "white smoke" :foreground "black"))))
 '(mode-line-inactive ((t (:background "dark gray" :foreground "black"))))
 '(vertical-border ((t (:foreground "black")))))

;; Set the package sources.
(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files")

;; EL-GET

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-sources
      '(el-get
	(:name emacs-color-theme-solarized-r0man
	       :type github
	       :pkgname "r0man/emacs-color-theme-solarized"
	       :description "Emacs highlighting using Ethan Schoonover’s Solarized color scheme "
	       :prepare (add-to-list 'custom-theme-load-path default-directory))
	(:name inflect
	       :type github
	       :pkgname "r0man/inflect-el"
	       :description "Emacs Lisp Inflection Library."
	       :compile ("inflect.el"))
	(:name emacs-request
	       :type github
	       :pkgname "tkf/emacs-request"
	       :description "Easy HTTP request for Emacs Lisp"
	       :load "request.el"
	       :compile ("request.el"))
	(:name jss
	       :type github
	       :pkgname "segv/jss"
	       :depends websocket
	       :description "jsSlime - An emacs toolkit for developing and debugging in-browser javascript code "
	       :load "jss.el"
	       :compile ("jss.el" "jss-browser-webkit.el"))
	(:name macrostep
	       :type github
	       :pkgname "joddie/macrostep"
	       :description "Elisp macro stepper"
	       :compile ("macrostep.el"))
	(:name slamhound
	       :type github
	       :pkgname "technomancy/slamhound"
	       :description "Slamhound rips your namespace form apart and reconstructs it."
	       :compile ("slamhound.el"))))

(el-get
 'sync
 'ace-jump-mode
 'ack-and-a-half
 'auto-complete
 'auto-complete-css
 'auto-complete-emacs-lisp
 'auto-complete-etags
 'auto-complete-ruby
 'auto-complete-yasnippet
 'auto-dictionary
 'cider
 'clojure-mode
 'ac-nrepl
 'dired-toggle-sudo
 'elisp-slime-nav
 'elnode
 'emacs-color-theme-solarized-r0man
 'emacs-request
 'emms
 'expand-region
 'find-file-in-project
 'flx
 'gnus-notify
 'haskell-mode
 'haskell-mode-exts
 'highlight-cl
 'hive
 'ido-vertical-mode
 'inflect
 'js2-mode
 'jss
 'macrostep
 'magit
 'markdown-mode
 'multi-term
 'multiple-cursors
 'paredit
 'popwin
 'pretty-lambdada
 'projectile
 'ruby-mode
 'ruby-test-mode
 'rvm
 'scss-mode
 'slamhound
 'smex
 'smooth-scrolling
 'vertica
 'web-mode
 'websocket
 'yasnippet)

(defun compass-watch ()
  "Find the project root and run compass watch."
  (interactive)
  (let ((directory (locate-dominating-file (expand-file-name (directory-file-name ".")) "config.rb"))
        (compilation-ask-about-save nil)
        (compilation-buffer-name-function (lambda (mode) "*compass*")))
    (if directory
        (compile (message (format "cd %s; compass watch" directory)))
      (message "Can't find compass project root."))))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun untabify-buffer ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun cleanup-buffer ()
  "Cleanup the current buffer."
  (interactive)
  (indent-buffer)
  (delete-trailing-whitespace))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun swap-buffers ()
  "Swap your buffers."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* ((w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun rotate-buffers ()
  "Rotate your buffers."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun xresources ()
  "Reload the ~/.Xresources configuration."
  (interactive)
  (shell-command "xrdb -merge ~/.Xresources ")
  (message "X resources reloaded."))

(setq user-full-name "Roman Scherer")

;; Global Auto Revert mode is a global minor mode that reverts any
;; buffer associated with a file when the file changes on disk.
(global-auto-revert-mode 1)

;; Ask user a "y or n" question.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Enter debugger if an error is signaled?
(setq debug-on-error nil)

;; Don't show startup message.
(setq inhibit-startup-message t)

;; Delete trailing whitespace when saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Toggle column number display in the mode line.
(column-number-mode)

;; Enable display of time, load level, and mail flag in mode lines.
(display-time)

;; Whether to add a newline automatically at the end of the file.
(setq require-final-newline t)

;; This variable describes the behavior of the command key.
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; Highlight trailing whitespace
(setq show-trailing-whitespace t)

;; Controls the operation of the TAB key.
(setq tab-always-indent 'complete)

;; The maximum size in lines for term buffers.
(setq term-buffer-maximum-size (* 10 2048))

;; ABBREV MODE
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq default-abbrev-mode t)
(setq save-abbrevs t)

;; BACKUP

;; Put all backup files in a separate directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Make backups for files under version control as well.
(setq vc-make-backup-files t)

;; If t, delete excess backup versions silently.
(setq delete-old-versions t)

;; Number of newest versions to keep when a new numbered backup is made.
(setq kept-new-versions 6)

;; Number of oldest versions to keep when a new numbered backup is made.
(setq kept-old-versions 2)

;; Make numeric backup versions unconditionally.
(setq version-control t)

;;; COMPILE-MODE
(setq compilation-scroll-output 't)

;; ;; Show colors in compilation buffers.
;; ;; http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer

;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region (point-min) (point-max))
;;   (toggle-read-only))

;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq inferior-lisp-program "lein trampoline cljsbuild repl-launch chromium")

(defun lein-cljsbuild ()
  (interactive)
  (compile "lein clean; lein cljsbuild auto"))

(defun lein-chrome-repl ()
  "Start a Chrome Browser repl via Leiningen."
  (interactive)
  (run-lisp "lein trampoline cljsbuild repl-launch chromium"))

(defun lein-firefox-repl ()
  "Start a Chrome Browser repl via Leiningen."
  (interactive)
  (run-lisp "lein trampoline cljsbuild repl-launch firefox"))

(defun lein-rhino-repl ()
  "Start a Rhino repl via Leiningen."
  (interactive)
  (run-lisp "lein trampoline cljsbuild repl-rhino"))

(defun lein-node-repl ()
  "Start a NodeJS repl via Leiningen."
  (interactive)
  (run-lisp "lein trampoline noderepl"))

;; CSS-MODE
(setq css-indent-offset 2)

;; DIRED

;; Switches passed to `ls' for Dired. MUST contain the `l' option.
(setq dired-listing-switches "-alh")

(setq dired-dwim-target t)

(defun dired-do-shell-command-in-background (command)
  "In dired, do shell command in background on the file or directory named on
 this line."
  (interactive
   (list (dired-read-shell-command (concat "& on " "%s: ") nil (list (dired-get-filename)))))
  (call-process command nil 0 nil (dired-get-filename)))

;; DIRED-X
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (define-key dired-mode-map "&" 'dired-do-shell-command-in-background)))

;; User-defined alist of rules for suggested commands.
(setq dired-guess-shell-alist-user
      '(("\\.pdf$" "evince")
        ("\\.xlsx?$" "libreoffice")))

;; EMACS LISP
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

;; FIND-DIRED
(defun find-dired-clojure (dir)
  "Run find-dired on Clojure files."
  (interactive (list (read-directory-name "Run find (Clojure) in directory: " nil "" t)))
  (find-dired dir "-name \"*.clj\""))

(defun find-dired-ruby (dir)
  "Run find-dired on Ruby files."
  (interactive (list (read-directory-name "Run find (Ruby) in directory: " nil "" t)))
  (find-dired dir "-name \"*.rb\""))

;; FLY SPELL

(defun enable-flyspell-mode ()
  "Enable Flyspell mode."
  (flyspell-mode 1))

(defun enable-flyspell-prog-mode ()
  "Enable Flyspell Programming mode."
  (flyspell-prog-mode))

(dolist (hook '(text-mode-hook))
  (add-hook hook 'enable-flyspell-mode))

(dolist (hook '(prog-mode-hook))
  (add-hook hook 'enable-flyspell-prog-mode))

;; GNUS
(setq gnus-init-file "~/.emacs.d/gnus.el")

;; IDO-MODE
(ido-mode t)
(setq ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t)

;; JAVA

;; Indent Java annotations.
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-04/msg00262.html
(add-hook
 'java-mode-hook
 '(lambda ()
    (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; OCTAVE
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; RCIRC
(if (file-exists-p "~/.rcirc.el") (load-file "~/.rcirc.el"))
(setq rcirc-default-nick "r0man"
      rcirc-default-user-name "r0man"
      rcirc-default-full-name "Roman Scherer"
      rcirc-server-alist '(("irc.freenode.net" :channels ("#clojure")))
      rcirc-private-chat t
      rcirc-debug-flag t)

(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively) 8192)
            (rcirc-track-minor-mode 1)
            (flyspell-mode 1)))

;; SCSS-MODE
(setq scss-compile-at-save nil)

;; SMTPMAIL
(require 'smtpmail)

;; Send mail via smtpmail.
(setq send-mail-function 'sendmail-send-it)

;; Whether to print info in buffer *trace of SMTP session to <somewhere>*.
(setq smtpmail-debug-info t)

;; The name of the host running SMTP server.
(setq smtpmail-smtp-server "smtp.googlemail.com")

;; SMTP service port number.
(setq smtpmail-smtp-service 465)

;; Fuck the NSA.
(setq mail-signature
      '(progn
         (goto-char (point-max))
         (insert "\n\n--------------------------------------------------------------------------------")
         (spook)))

;; SQL-MODE
(eval-after-load "sql"
  '(progn
     (let ((filename "~/.sql.el"))
       (when (file-exists-p filename)
         (load-file filename)))))

;; SQL-INDENT
(setq sql-indent-offset 2)

;; TRAMP
(eval-after-load "tramp"
  '(progn
     (tramp-set-completion-function
      "ssh"
      '((tramp-parse-shosts "~/.ssh/known_hosts")
        (tramp-parse-hosts "/etc/hosts")))))

;; UNIQUIFY
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-separator "|")
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-after-kill-buffer-p t)

;; WINNER-MODE
(winner-mode)

(add-hook
 'after-init-hook
 (lambda ()

   ;; Load system specific config.
   (let ((system-config (concat user-emacs-directory system-name ".el")))
     (when (file-exists-p system-config)
       (load system-config)))

   (require 'emms-setup)
   (emms-all)
   (emms-default-players)

   (add-to-list 'emms-player-list 'emms-player-mpd)
   (condition-case nil
       (emms-player-mpd-connect)
     (error (message "Can't connect to music player daemon.")))

   (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
   (setq emms-player-mpd-music-directory (expand-file-name "~/Music"))

   (let ((filename "~/.emms.el"))
     (when (file-exists-p filename)
       (load-file filename)))

   (add-to-list 'emms-stream-default-list
		'("SomaFM: Space Station" "http://www.somafm.com/spacestation.pls" 1 streamlist))

   ;; Start a terminal.
   (multi-term)

   ;; Start the Emacs server.
   (server-start)

   ;; Load keyboard bindings.
   (global-set-key (kbd "C-c ,") 'ruby-test-run)
   (global-set-key (kbd "C-c C-+") 'er/expand-region)
   (global-set-key (kbd "C-c C--") 'er/contract-region)
   (global-set-key (kbd "C-c C-.") 'clojure-test-run-test)
   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
   (global-set-key (kbd "C-c M-,") 'ruby-test-run-at-point)
   (global-set-key (kbd "C-c SPC") 'ace-jump-mode)
   (global-set-key (kbd "C-c n") 'cleanup-buffer)
   (global-set-key (kbd "C-x C-g b") 'mo-git-blame-current)
   (global-set-key (kbd "C-x C-d") 'dired)
   (global-set-key (kbd "C-x C-o") 'delete-blank-lines)
   (global-set-key (kbd "C-x TAB") 'indent-rigidly)
   (global-set-key (kbd "C-x ^") 'enlarge-window)
   (global-set-key (kbd "C-x C-f") 'projectile-find-file)
   (global-set-key (kbd "C-x f") 'ido-find-file)
   (global-set-key (kbd "C-x h") 'mark-whole-buffer)
   (global-set-key (kbd "C-c r") 'rotate-buffers)

   (let ((mode emacs-lisp-mode-map))
     (define-key mode (kbd "C-c m") 'macrostep-expand)
     (define-key mode (kbd "C-c e E") 'elint-current-buffer)
     (define-key mode (kbd "C-c e c") 'cancel-debug-on-entry)
     (define-key mode (kbd "C-c e d") 'debug-on-entry)
     (define-key mode (kbd "C-c e e") 'toggle-debug-on-error)
     (define-key mode (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
     (define-key mode (kbd "C-c e l") 'find-library)
     (define-key mode (kbd "C-c e r") 'eval-region)
     (define-key mode (kbd "C-c C-k") 'eval-buffer)
     (define-key mode (kbd "C-c ,") 'ert)
     (define-key mode (kbd "C-c C-,") 'ert))

   (define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
   (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)))
