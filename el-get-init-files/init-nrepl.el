(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook
          (lambda () (define-key nrepl-interaction-mode-map (kbd "C-c C-s") 'clojure-jump-between-tests-and-code)))

(setq nrepl-port "5000")
