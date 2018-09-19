;;; package-config --- Summary

;;; Commentary:
;;; Add additional sources for package-list

;;; Code:

;; Add melpa repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Default packages
(defvar my-packages
  '(company
    company-go
    flycheck
    git-gutter
    go-mode
    material-theme
    flycheck-rust
    rust-mode
    evil
    org)
  "Default packages to install on startup.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
(package-install p)))

(provide 'package-config)
;;; package-config ends here
