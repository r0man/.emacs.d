;; (setq multi-term-dedicated-select-after-open-p t
;;       multi-term-dedicated-window-height 25
;;       multi-term-program "/bin/bash")

;; ;; Enable compilation-shell-minor-mode in multi term.
;; ;; http://www.masteringemacs.org/articles/2012/05/29/compiling-running-scripts-emacs/

;; ;; TODO: WTF? Turns off colors in terminal.
;; ;; (add-hook 'term-mode-hook 'compilation-shell-minor-mode)

(add-hook 'term-mode-hook
          (lambda ()
            (dolist
                (bind '(("<S-down>" . multi-term)
                        ("<S-left>" . multi-term-prev)
                        ("<S-right>" . multi-term-next)
                        ("C-<backspace>" . term-send-backward-kill-word)
                        ("C-<delete>" . term-send-forward-kill-word)
                        ("C-<left>" . term-send-backward-word)
                        ("C-<right>" . term-send-forward-word)
                        ("C-c C-j" . term-line-mode)
                        ("C-c C-k" . term-char-mode)
                        ("C-v" . scroll-up)
                        ("C-y" . term-paste)
                        ("C-z" . term-stop-subjob)
                        ("M-DEL" . term-send-backward-kill-word)
                        ("M-d" . term-send-forward-kill-word)))
              (add-to-list 'term-bind-key-alist bind))))

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

(global-set-key (kbd "C-x M") 'multi-term)
(global-set-key (kbd "C-x m") 'switch-to-term-mode-buffer)
