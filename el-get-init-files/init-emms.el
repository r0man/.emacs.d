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
