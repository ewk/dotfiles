;;; package-config --- Summary

;;; Commentary:
;; Add additional sources for package-list

;;; Code:

;; Add marmalade and melpa repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Default packages
(defvar ewk/packages '(ac-slime
			  company
                          flycheck
                          go-mode
                          org
			  zenburn-theme
			  git-gutter)
  "Default packages")

;; Install packages if not present
(defun ewk/packages-installed-p ()
  (cl-loop for pkg in ewk/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (ewk/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ewk/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'package-config)
;;; package-config ends here
