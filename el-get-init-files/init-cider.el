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

;; Auto-select the error buffer when it's displayed:
(setq cider-auto-select-error-buffer t)

;; Controls whether to pop to the REPL buffer on connect.
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Controls whether to auto-select the error popup buffer.
(setq cider-auto-select-error-buffer t)

;; T to wrap history around when the end is reached.
(setq cider-repl-wrap-history t)

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(defun piggiepack-repl ()
  (interactive)
  ;; (cider-jack-in)
  (cider-interactive-eval
   "(require 'cljs.repl.browser)
    (cemerick.piggieback/cljs-repl :repl-env (cljs.repl.browser/repl-env :port 9000))"))

(defun node-repl ()
  (interactive)
  (cider-interactive-eval
   "(require '[cljs.repl.node :as node])
    (node/run-node-nrepl)"))
