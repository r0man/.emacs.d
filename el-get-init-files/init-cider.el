;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide *nrepl-connection* and *nrepl-server* buffers from appearing
;; in some buffer switching commands like switch-to-buffer
(setq nrepl-hide-special-buffers nil)

;; Enabling CamelCase support for editing commands(like forward-word,
;; backward-word, etc) in the REPL is quite useful since we often have
;; to deal with Java class and method names. The built-in Emacs minor
;; mode subword-mode provides such functionality
(add-hook 'cider-repl-mode-hook 'subword-mode)

;; The use of paredit when editing Clojure (or any other Lisp) code is
;; highly recommended. You're probably using it already in your
;; clojure-mode buffers (if you're not you probably should). You might
;; also want to enable paredit in the REPL buffer as well.
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))
