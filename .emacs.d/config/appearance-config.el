;; Turn off splash screen and scratch mode message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Menu bar
(tool-bar-mode -1) ;; Hide toolbar buttons
(define-key menu-bar-tools-menu [games] nil)

;; Themes
(if window-system
    (load-theme 'monokai t)
  (load-theme 'wombat t))

;; Window size
(setq initial-frame-alist '((top . 10) (left . 30)
                            (width . 90) (height . 50)))

;; Appearance
(set-default-font "Inconsolata-14" "Menlo-12")
(column-number-mode 1) ; Show column number in mode-line
(global-linum-mode 1) ; Line numbers in all buffers
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

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
;;(require 'autopair) ;; Close pairs automatically
;; turn on automatic bracket insertion by pairs.
(electric-pair-mode 1)

;; fix terminal output characters
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
