;;; early-init.el --- The Early Init File. -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded before the package system and GUI is
;; initialized.

;;; Code:

;; Number of bytes of consing between garbage collections.
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
(setq gc-cons-threshold 100000000)

;;; early-init.el ends here
