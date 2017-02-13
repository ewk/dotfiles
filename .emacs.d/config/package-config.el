;;; package-config --- Summary

;;; Commentary:
;; Add additional sources for package-list

;;; Code:

;; Add marmalade and melpa repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
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
    org
    rust-mode
    zenburn-theme)
  "Default packages to install on startup.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
(package-install p)))

(provide 'package-config)
;;; package-config ends here
