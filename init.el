;;; init.el --- The Emacs init file

;;; Commentary:

;;; The Emacs init file

;;; Code:

;; Disable menu, scroll and tool bars.

(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Load customization settings first.

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; Load theme.

(defun current-theme ()
  "Return the theme, depending on the current month and hour of the day."
  (let ((month (string-to-number (format-time-string "%m")))
        (hour (string-to-number (format-time-string "%H"))))
    (cond
     ;; Use dark theme in winter
     ((member month '(1 2 3 10 11 12))
      'solarized-dark)
     ((member hour '(10 11 12 13 14 15 16 17 18 19))
      'solarized-light)
     (t 'solarized-dark))))

(let ((theme (current-theme)))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme theme)))
    (load-theme theme)))

;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in literate Org-mode files.

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((started-at (current-time))
       (org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  (require 'ob-tangle)
  (message "Loaded Org Mode (%.03fs)" (float-time (time-since started-at))))

;; Load up all literate org-mode files in this directory
(mapc (lambda (filename)
        (let ((started-at (current-time)))
          (org-babel-load-file filename)
          (message "Loaded Org Babel file: %s (%.03fs)"
                   filename (float-time (time-since started-at)))))
      (directory-files dotfiles-dir t "\\.org$"))

;;; init.el ends here
