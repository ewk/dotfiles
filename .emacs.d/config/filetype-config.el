;; Set major mode for additional file types
(setq auto-mode-alist
  (append
    '(("\\.hdl$" . vhdl-mode))
    auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("rc$" . conf-mode))

;; Enable global syntax check
(global-flycheck-mode)

;; Go plugin
;;(require go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
