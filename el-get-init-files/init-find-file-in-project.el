(require 'find-file-in-project)
(dolist (extension '("*.java" "*.edn" "*.sql"))
  (add-to-list 'ffip-patterns extension))
