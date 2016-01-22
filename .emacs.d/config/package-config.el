;; Add marmalade and melpa repositories
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

;; Default pacakges
(defvar ewk/packages '(ac-slime
                          auto-complete
                          ;;autopair
                          flycheck
                          go-mode
                          ;;magit
                          marmalade
                          org
                          ;;solarized-theme
			  zenburn-theme
			  monokai-theme
			  git-gutter)
  "Default packages")

;; Install packages if not present
(defun ewk/packages-installed-p ()
  (loop for pkg in ewk/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (ewk/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ewk/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))
