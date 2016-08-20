;;; Package --- Summary

;; Turn on autocomplete
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
;(require 'auto-complete-config)
;(ac-config-default)
;(global-auto-complete-mode t)
;(require 'go-autocomplete)

;; Turn on company mode
(add-hook 'after-init-hook 'global-company-mode)
;; Trigger completion with TAB
;(global-set-key "\t" 'company-complete-common)

; Interactive do things- find file auto complete
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)
