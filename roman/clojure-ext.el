(eval-after-load 'clojure-mode
  '(defun clojure-jump-to-test ()
     "Jump from implementation file to test."
     (interactive)
     (find-file
      (command-line-normalize-file-name
       (format "%s/test/%s%s.clj"
               (locate-dominating-file buffer-file-name "src")
               (cond
                ((string-match ".*/src/clj/.*" buffer-file-name) "clj/")
                ((string-match ".*/src/clojure/.*" buffer-file-name) "clojure/")
                (t ""))
               (clojure-test-for (clojure-find-ns)))))))

(eval-after-load 'clojure-test-mode
  '(defun clojure-test-jump-to-implementation ()
     "Jump from test file to implementation."
     (interactive)
     (find-file
      (command-line-normalize-file-name
       (format "%s/src/%s%s.clj"
               (locate-dominating-file buffer-file-name "src")
               (cond
                ((string-match ".*/test/clj/.*" buffer-file-name) "clj/")
                ((string-match ".*/test/clojure/.*" buffer-file-name) "clojure/")
                (t ""))
               (clojure-test-implementation-for (clojure-find-package)))))))

(provide 'clojure-ext)
