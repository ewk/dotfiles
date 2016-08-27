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

;; Go mode
(add-hook 'go-mode-hook
          (lambda ()
	    (require 'company-go)
	    (setq gofmt-command "goimports")
	    (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(provide 'filetype-config)
;;; filetype-config ends here
