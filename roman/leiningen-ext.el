
(defun lein-deps ()
  "Run lein deps and update all dependencies of the current
project."
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (when (not root)
      (error "Hey Cowboy, yOu are nOt in any Leiningen prOject!"))
    ;; you can customize slime-port using .dir-locals.el
    (shell-command (format "cd %s && lein deps &" root) "*lein-deps*")))

(defun lein-ring-server ()
  "Run 'lein ring server' in the current project to start a web
server."
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (when (not root)
      (error "Hey Cowboy, yOu are nOt in any Leiningen prOject!"))
    (shell-command (format "cd %s && lein ring server &" root)) "*lein-ring-server*"))

(provide 'leiningen-ext)
