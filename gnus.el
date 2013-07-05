
(setq gnus-select-method
      '(nntp "news.online.de"))

;; *Integer that says how verbose Gnus should be.
(setq gnus-verbose 10)

;; Integer that says how verbose the Gnus backends should be.
(setq gnus-verbose-backends 10)

;; ;; Which information should be exposed in the User-Agent header.
;; (setq mail-user-agent 'gnus-user-agent)

;; ;; Default method for selecting a newsgroup.
;; (setq gnus-select-method
;;       '(nnimap "gmail"
;;                (nnimap-address "imap.gmail.com")
;;                (nnimap-server-port 993)
;;                (nnimap-stream ssl)))

;; ;; A regexp to match uninteresting newsgroups. Use blank string for Gmail.
;; (setq gnus-ignored-newsgroups "")

;; ;; Add daemonic server disconnection to Gnus.
;; (gnus-demon-add-disconnection)

;; ;; Add daemonic nntp server disconnection to Gnus.
;; (gnus-demon-add-nntp-close-connection)

;; ;; Add daemonic scanning of mail from the mail backends.
;; (gnus-demon-add-scanmail)

;; ;; Initialize the Gnus daemon.
;; (gnus-demon-init)
