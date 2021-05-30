;;; early-init.el --- The Early Init File. -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded before the package system and GUI is
;; initialized.

;;; Code:

;; Number of bytes of consing between garbage collections.
(setq gc-cons-threshold 402653184)

;; Portion of the heap used for allocation.
(setq gc-cons-percentage 0.6)

;;; early-init.el ends here
