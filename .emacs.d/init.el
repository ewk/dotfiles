;;; init.el --- Summary

;;; Commentary:

;;; Code:

;;
;; Package management
;;
(require 'package)
(setq package-enable-at-startup nil)
 (setq package-archives '(("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (eval-when-compile (require 'use-package)))

(setq use-package-always-ensure t)

;;
;; General Config
;;

(set-language-environment "UTF-8")

;; Turn off scrollbars early in startup
(scroll-bar-mode -1)
(tool-bar-mode -1)
(define-key menu-bar-tools-menu [games] nil)

;; Window size
(setq initial-frame-alist '((top . 00) (left . 00)
                            (width . 105) (height . 50)))

;; Turn off splash screen and scratch mode message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(column-number-mode 1)               ; Show column number in mode-line
(global-display-line-numbers-mode t) ; Line numbers in all buffers

;; Turn off beeping and improve keyboard response
(blink-cursor-mode 0)        ; Disable cursor blinking
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)        ; I am not a robot. I mean, the humans are dead.

;; Show filename in frame
(setq frame-title-format
        '(buffer-file-name "%f" (dired-directory dired-directory "%b")))
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; Interactive do things- find file auto complete
(ido-mode 1)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Avoid backslash madness
(require 're-builder)
(setq reb-re-syntax 'string)

;; Buffer management
(setq enable-recursive-minibuffers t)      ; Stack minibuffers
(setq large-file-warning-threshold nil)    ; Don't warn opening large files
(savehist-mode 1)                          ; Save minibuffer history

;; Write all buffers when frame focus is lost
(add-hook 'after-focuse-change-function (lambda () (save-some-buffers t)))
(setq make-backup-files nil)    ; No backup files ~
(global-auto-revert-mode t)     ; Reload files changed outside Emacs

;; Remember last position in each file- .emacs.d/places must exist
(save-place-mode 1)

;; Prevent automatic changes to init.el
(setq custom-file (make-temp-file "emacs-custom"))

;;
;; Appearance
;;

;; Themes
(use-package material-theme
    :config
    (load-theme 'material t))

;; Font
(set-frame-font "Droid Sans Mono-16")

;; Disable syntax highlighting
(global-font-lock-mode 0)

;; Cursorline
(global-hl-line-mode t)                  ; Highlight cursor line
(set-face-background 'hl-line "black")  ; Set line highlight color

;; Fix terminal output characters
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;;
;; Programming
;;

(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

;; assembly
(require 'asm-mode)
(add-hook 'asm-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)    ; use spaces to indent
	    (electric-indent-mode -1)      ; indentation in asm-mode is annoying
	    (setq tab-stop-list (number-sequence 2 60 2))))
(define-key asm-mode-map (kbd "<ret>") 'newline-and-indent)
(define-key asm-mode-map (kbd "<backtab>") 'company-complete) ; complete with Shift-TAB

;; Version control
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;; Highlighting
(electric-pair-mode t)           ; Close pairs automatically
(show-paren-mode 1)
(transient-mark-mode t)          ; Highlight mark selection

;;
;; Editing
;;
(delete-selection-mode t)           ; Replace selected text
(setq select-enable-clipboard t)    ; Interact with system clipboard

;; Where is aspell?
(setq-default flyspell-issue-welcome-flag nil)
(setq-default ispell-program-name "/usr/bin/aspell")
(setq-default ispell-list-command "list")

;; Enable spell checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode) ;; Only checks comments
(add-hook 'diff-mode-hook 'flyspell-mode)

;; Tabs and spaces
(setq-default indent-tabs-mode t)

;; tab-width, c-basic-offset, and cperl-indent-level should always be the same
(setq-default tab-width 8)    ; Width of a tab
(setq-default c-basic-offset 8)
(setq-default cperl-indent-level 8) ;; Perl, of course, is different

;; Sentences end with one space
(setq sentence-end-double-space nil)

;; Add blank line to end of buffer
(setq next-line-add-newlines t)

;; Do not wrap text in programming mode
(add-hook 'c-mode-hook (lambda () (setq truncate-lines t)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; This may interfere with version control and next-line-add-newlines
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init)
;;; init ends here
