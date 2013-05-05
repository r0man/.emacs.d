(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(eval-after-load "clojure-mode"
  '(define-clojure-indent
     ;; COMPOJURE
     (ANY 2)
     (DELETE 2)
     (GET 2)
     (HEAD 2)
     (POST 2)
     (PUT 2)
     (context 2)
     ;; ALGO.MONADS
     (domonad 1)
     ;; SQLINGVO
     (copy 2)
     (create-table 1)
     (delete 1)
     (drop-table 1)
     (insert 2)
     (select 1)
     (truncate 1)
     (update 2)
     ;; CUSTOM
     (api-test 1)
     (web-test 1)
     (database-test 1)
     (defroutes 'defun)
     ))
