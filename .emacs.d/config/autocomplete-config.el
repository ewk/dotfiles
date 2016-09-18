;;; autocomplete-config.el --- Summary

;;; Commentary:

;;; Code:

;; Turn on company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Trigger completion with TAB
;(global-set-key "\t" 'company-complete)
(setq company-tooltip-limit 15)
(setq company-idle-delay 0)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))

; Interactive do things- find file auto complete
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)

(provide 'autocomplete-config)
;;; autocomplete-config ends here
