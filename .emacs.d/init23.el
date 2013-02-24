;; Load Path
(push "/usr/local/bin" exec-path)
(setq exec-path (append exec-path '("/bin"))) ; Change binary path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/modules/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(require 'magit) ;Load git mode

; Use Common Lisp features, can be problematic
 (require 'cl)

;; Appearance
(load-theme 'zenburn)
(set-default-font "Inconsolata-14" "Menlo-12")
(global-linum-mode 1) ; Line numbers in all buffers
(set-scroll-bar-mode 'right) ; Scrollbar on the right
(setq inhibit-startup-message t) ; No message at startup
(column-number-mode t) ; Show column number in mode-line
(global-font-lock-mode 1) ; Color enabled
(blink-cursor-mode 0) ; No blinking cursor

;; enable autopair in all buffers
(require 'autopair)
(autopair-global-mode)

;; Menu bar
(define-key menu-bar-tools-menu [games] nil)
(tool-bar-mode 0)

;; Keyboard customizations
(define-key global-map (kbd "RET") 'newline-and-indent) ; Indent on return
(defalias 'qrr 'query-replace-regexp) ; Define an alias
(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
(global-unset-key "\C-x\C-v") ; Suppress a shortcut
(global-set-key "\C-x\C-b" 'buffer-menu) ; CxCb puts point on buffer list

;; The outside world
(setq shell-file-name "/bin/bash") ; Set Shell for M-| command
(setq tex-shell-file-name "/bin/bash") ; Set Shell used by TeX
(setq grep-command "grep -i -nH -e ") ; Set grep command options
(setq x-select-enable-clipboard t) ; Access system clipboard

; Interactive do things- find file auto complete
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; Spaces and word bounaries
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq sentence-end-double-space nil) ; Sentences end with one space
(setq default-tab-width 2) ; Tab appears to be 2 spaces, but isn't saved that way

;; Highlighting
(show-paren-mode 1) ; Highlight parenthesis pairs
(setq blink-matching-paren-distance nil) ; Blinking parenthesis
(setq show-paren-style 'expression) ; Highlight text between parens
(global-hl-line-mode t) ; Highlight cursor line

;; Mouse
(mouse-wheel-mode t) ; Mouse-wheel enabled
(setq mouse-yank-at-point t) ; Paste at cursor position
(setq scroll-preserve-screen-position t) ; Scroll without moving cursor
(mouse-avoidance-mode 'jump) ; Mouse avoids cursor

;; Backups and autosave
(setq make-backup-files nil) ; No backup files ~
(setq auto-save-timeout 60) ; Autosave every minute

;; EOF and new lines
(setq track-eol nil) ; Cursor don't track end-of-line
(setq require-final-newline 't) ; Always newline at end of file
(setq next-line-add-newlines t) ; Add newline when at buffer end
(setq truncate-partial-width-windows nil) ; Don't truncate long lines

;; Buffer management
(setq pop-up-frame t) ; Buffers in separate frames
(setq frame-title-format "%b - emacs") ; Use buffer name as frame title
(setq enable-recursive-minibuffers t) ; Stack minibuffers

;; Annoying things
;(desktop-save-mode t) ; Save session before quitting
;(setq confirm-kill-emacs 'yes-or-no-p) ; Confirm quit
(setq visible-bell t) ; No beep when reporting errors

;; Default mode
(setq default-major-mode 'text-mode) ; Text-mode is default mode
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; auto-formatting in text-mode

;; Dictionary
(setq ispell-dictionary "english") ; Set ispell dictionary

;; Calendars
(setq calendar-week-start-day 1) ; Week starts monday
(setq european-calendar-style 't) ; European style calendar

;; Bunch o stuff
(setq suggest-key-bindings nil) ; No hints for M-x
(setq-default case-fold-search t) ; Search is case sensitive
(setq tab-width 2) ; Length of tab is 2 SPC
(put 'narrow-to-region 'disabled nil) ; Allow narrow-to-region command
(setq disabled-command-hook nil) ; Allow all disabled commands
(setq undo-limit 100000) ; Increase number of undo
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6938c51c0a89f078c61b979af23ae4c32204458f16a6a08c1a683ab478a7bc6b" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
