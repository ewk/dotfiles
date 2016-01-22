;; Set major mode for additional file types
(setq auto-mode-alist
  (append
    '(("\\.hdl$" . vhdl-mode))
    auto-mode-alist))
