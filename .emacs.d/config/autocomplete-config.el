;; Turn on autocomplete
(require 'auto-complete-config)
(ac-config-default)

; Interactive do things- find file auto complete
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)
