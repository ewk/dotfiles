;;; init.el --- Summary

;;; Commentary:

;;; Code:

;;
;; General Config
;;
(load "~/.emacs.d/config/appearance-config.el")

;; Turn off scrollbars early in startup to avoid window width weirdness.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(define-key menu-bar-tools-menu [games] nil)

;; Window size
(setq initial-frame-alist '((top . 00) (left . 00)
                            (width . 90) (height . 50)))
;; Turn off splash screen and scratch mode message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Initialize must come before configurations installed packages.
(package-initialize)
(load "~/.emacs.d/config/package-config.el")

(require 'cl-lib)
(set-language-environment "UTF-8")

(column-number-mode 1) ; Show column number in mode-line
(global-linum-mode 1) ; Line numbers in all buffers

;; Turn off beeping and improve keyboard response
(blink-cursor-mode 0) ; Disable cursor blinking
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t) ;; I am not a robot. I mean, the humans are dead.

;; Show filename in frame
(setq frame-title-format
      ;;(list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b")));;)
;;(setq frame-title-format '(buffer-file-name "%f" ("%b")))
;;(setq frame-title-format "%b - emacs") ; Use buffer name as frame title
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Themes
(if window-system
    (load-theme 'material t)
  (load-theme 'wombat t))

;; Cursorline
(global-hl-line-mode t) ; Highlight cursor line
(set-face-background 'hl-line "black") ; Set line highlight color
(set-face-foreground 'highlight nil) ; Keep syntax highlighitng on highlighted line

;; Font
(if (eq system-type 'darwin)
    (set-frame-font "Menlo-14")
  (set-frame-font "Droid Sans Mono-14"))

;; TODO keep cursor at same point when scrolling; use C-l for now

;; Highlighting
(electric-pair-mode t) ;; Close pairs automatically
(show-paren-mode t) ; Highlight parenthesis pairs
(setq blink-matching-paren t) ; Blinking parenthesis, on by default
(setq show-paren-style 'expression) ; Highlight text between parens

(load "~/.emacs.d/config/buffer-config.el")
(load "~/.emacs.d/config/autocomplete-config.el")
(load "~/.emacs.d/config/org-config.el")
(load "~/.emacs.d/config/alias-config.el")
(load "~/.emacs.d/config/emacsclient-config.el")

(provide 'init)
;;; init ends here
