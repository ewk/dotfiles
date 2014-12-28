(set-language-environment "UTF-8")

(setq user-full-name "Eddie Kovsky")
(setq user-mail-address "ewk@edkovsky.org")

;; Use Common Lisp
(require 'cl)

;; Add marmalade and melpa repositories
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

;; Default pacakges
(defvar ewk/packages '(ac-slime
                          auto-complete
                          autopair
                          flycheck
                          go-mode
                          magit
                          marmalade
                          org
                          solarized-theme)
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

;; Turn off scrash screen and scratch mode message
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Appearance
(set-default-font "Inconsolata-14" "Menlo-12")
(column-number-mode 1) ; Show column number in mode-line
(global-linum-mode 1) ; Line numbers in all buffers
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Window size
(setq initial-frame-alist '((top . 10) (left . 30)
                            (width . 90) (height . 50)))
;; Menu bar
(tool-bar-mode -1) ;; Hide toolbar buttons
(define-key menu-bar-tools-menu [games] nil)

;; Themes
(if window-system
    (load-theme 'solarized-dark t)
  (load-theme 'wombat t))

;; Show filename in frame
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
;;(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Backups and autosave
(setq make-backup-files nil) ; No backup files ~
(setq auto-save-timeout 60) ; Autosave every minute

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; Turn off beeping and improve keyboard response
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

;; Highlighting
(show-paren-mode 1) ; Highlight parenthesis pairs
(setq blink-matching-paren t) ; Blinking parenthesis, on by default
(setq show-paren-style 'expression) ; Highlight text between parens
(global-hl-line-mode t) ; Highlight cursor line
(blink-cursor-mode 0)
(setq visible-bell t)
(require 'autopair) ;; Close pairs automatically

; Interactive do things- find file auto complete
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)
; better way to enable ibuffer
(defalias 'list-buffers 'ibuffer)

;; Turn on autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; Turn on spellchecking
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;; Buffer management
(setq pop-up-frame t) ; Buffers in separate frames
(setq frame-title-format "%b - emacs") ; Use buffer name as frame title
(setq enable-recursive-minibuffers t) ; Stack minibuffers
(setq large-file-warning-threshold nil) ; Don't warn opening large files

;; Tabs and spaces
;(setq-default tab-width 8) ;; Set tab width
;(setq-default c-basic-offset 8) ;; Applies to C and most languages
;(setq-default cperl-indent-level 8) ;; Perl, of course, is different
;(setq-default indent-tabs-mode t) ; nil will use spaces instead of tabs
;(setq sentence-end-double-space nil) ; Sentences end with one space
(global-set-key (kbd "C-c w") 'whitespace-mode) ; view all whitespace characters
;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

