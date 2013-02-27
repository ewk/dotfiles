;; Load path
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/modules/")
(add-to-list 'load-path "~/.emacs.d/modules/inf-ruby/")
(add-to-list 'load-path "~/.emacs.d/modules/rinari/")
(add-to-list 'load-path "~/.emacs.d/modules/nxhtml/")
(add-to-list 'load-path "~/.emacs.d/modules/ecb-2.40/")
;; Tabs and spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;(setq-default c-basic-offset 2)
;;(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
;;(setq sentence-end-double-space nil) ; Sentences end with one space
;;(setq default-tab-width 2) ; Tab appears to be 2 spaces, but isn't saved that way
;;(setq-default tab-width 2) ; Length of tab is 2 SPC

;; Appearance
(set-default-font "Inconsolata-14" "Menlo-12")
(require 'zenburn-theme)
(column-number-mode 1) ; Show column number in mode-line
(global-linum-mode 1) ; Line numbers in all buffers

;; Dictionary
(setq ispell-dictionary "english") ; Set ispell dictionary
;; Menu bar
(define-key menu-bar-tools-menu [games] nil)
(tool-bar-mode 0)
(setq inhibit-startup-message t) ; No message at startup

;; enable autopair in all buffers
(require 'autopair)
(autopair-global-mode)

;; Highlighting
(show-paren-mode 1) ; Highlight parenthesis pairs
(setq blink-matching-paren-distance nil) ; Blinking parenthesis
(setq show-paren-style 'expression) ; Highlight text between parens
(global-hl-line-mode t) ; Highlight cursor line

; Interactive do things- find file auto complete
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; Buffer management
(setq pop-up-frame t) ; Buffers in separate frames
(setq frame-title-format "%b - emacs") ; Use buffer name as frame title
(setq enable-recursive-minibuffers t) ; Stack minibuffers

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

;; Backups and autosave
(setq make-backup-files nil) ; No backup files ~
(setq auto-save-timeout 60) ; Autosave every minute

;; Print argument list of function you are writing
(eldoc-mode 1)

;; Default mode
(setq default-major-mode 'text-mode) ; Text-mode is default mode
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; auto-formatting in text-mode
