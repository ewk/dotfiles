;;; filetype-config.el --- Summary

;;; Commentary:
;;  Enable linter.
;;  Inform Emacs about additional filetypes it doesn't already recognize.

;;; Code:

;; Enable global syntax check
(global-flycheck-mode)

;; Set major mode for additional file types
(setq auto-mode-alist
  (append
    '(("\\.hdl$" . vhdl-mode))
    auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("rc$" . conf-mode))

;; Go plugin
;;(require go-mode)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'filetype-config)
;;; filetype-config ends here
