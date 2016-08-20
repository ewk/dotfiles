;; Set major mode for additional file types
(setq auto-mode-alist
  (append
    '(("\\.hdl$" . vhdl-mode))
    auto-mode-alist))

;; Enable global syntax check
(global-flycheck-mode)

;; Go plugin
;;(require go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
