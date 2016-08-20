;;; init.el --- Summary

;;; Commentary:
;;  All configuration is maintained in separate files.

;;; Code:

;; Use Common Lisp
(require 'cl-lib)
(set-language-environment "UTF-8")

(load "~/.emacs.d/config/package-config.el")
(load "~/.emacs.d/config/appearance-config.el")
(load "~/.emacs.d/config/buffer-config.el")
(load "~/.emacs.d/config/autocomplete-config.el")
(load "~/.emacs.d/config/whitespace-config.el")
(load "~/.emacs.d/config/org-config.el")
(load "~/.emacs.d/config/filetype-config.el")
(load "~/.emacs.d/config/vc-config.el")
(load "~/.emacs.d/config/alias-config.el")
(load "~/.emacs.d/config/client-config.el")

(provide 'init)
;;; init ends here
