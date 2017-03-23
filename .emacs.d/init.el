;;; init.el --- Summary

;;; Commentary:
;;  All configuration is maintained in separate files.

;;; Code:

;; Use Common Lisp
;; Turn off scrollbars early in startup to avoid window width weirdness.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(define-key menu-bar-tools-menu [games] nil)


;; Initialize must come before configurations installed packages.
(package-initialize)

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
(load "~/.emacs.d/config/emacsclient-config.el")

(provide 'init)
;;; init ends here
