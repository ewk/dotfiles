(set-language-environment "UTF-8")

;; Load path
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/modules/")
(add-to-list 'load-path "~/.emacs.d/modules/auto-complete-1.3.1")
;; Window size
(setq initial-frame-alist '((top . 10) (left . 30)
                            (width . 90) (height . 50)))
;; Appearance
(set-default-font "Inconsolata-14" "Menlo-12")
(require 'monokai-theme)
(column-number-mode 1) ; Show column number in mode-line
(global-linum-mode 1) ; Line numbers in all buffers

;; Tabs and spaces
(setq-default tab-width 8) ;; Should probably stop doing this
(setq-default c-basic-offset 8)
(setq-default indent-tabs-mode t) ; nil will use spaces instead of tabs
;;(setq sentence-end-double-space nil) ; Sentences end with one space

;; Menu bar
(define-key menu-bar-tools-menu [games] nil)
(tool-bar-mode 0)
(setq inhibit-startup-message t) ; No message at startup

;; enable electric pair in all buffers
(electric-pair-mode t)
(defun my-disable-electric-indentation ()
  "Stop ';', '}', etc. from re-indenting the current line." 
  (c-toggle-electric-state -1)) 
(add-hook 'c-mode-common-hook 'my-disable-electric-indentation)

;; Highlighting
(show-paren-mode 1) ; Highlight parenthesis pairs
(setq blink-matching-paren t) ; Blinking parenthesis, on by default
(setq show-paren-style 'expression) ; Highlight text between parens
(global-hl-line-mode t) ; Highlight cursor line
(blink-cursor-mode 0)
(setq visible-bell t)

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
;; (setq confirm-kill-emacs 'yes-or-no-p) ;; Never quit accidentally

;; Backups and autosave
(setq make-backup-files nil) ; No backup files ~
(setq auto-save-timeout 60) ; Autosave every minute

;; Print argument list of function you are writing
(eldoc-mode 1)

;; Default mode
(setq default-major-mode 'text-mode) ; Text-mode is default mode
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; auto-formatting in text-mode

;; Auto-complete
;;(require 'auto-complete-config)
;;(ac-config-default)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; unique buffer names
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Got a wheel mouse!
(mouse-wheel-mode 1)

(setq kill-whole-line t)

;; Some small hacks Chris Kirchen
(setq user-mail-address "edkovsky@gmail.com")
(setq display-time-24hr-format t)
(display-time-mode 1)
(setq isearch-lazy-highlight nil)
(setq Info-use-header-line nil)
(setq Info-fontify nil)
(setq ps-paper-type 'a4)
(setq kill-read-only-ok t)
(setq diff-switches "-urN")

;; (require ruby-mode)
;; .rb and .rbx are Ruby files
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rbx$" . ruby-mode))

;; go-mode
;;(require 'go-mode)
;;(add-hook 'go-mode-hook 'subword-mode)
;;(add-hook 'go-mode-hook #'(lambda () (setq fill-column 79)))
;;(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

;; For emacsclient
;(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(custom-safe-themes (quote ("1ac9a474d289e6a44894d1b484b3aa5eb345cea6ed6f32ec5214c797ac7ddf23" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(magit-diff-use-overlays nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#CF4F1F") (60 . "#C26C0F") (80 . "#b58900") (100 . "#AB8C00") (120 . "#A18F00") (140 . "#989200") (160 . "#8E9500") (180 . "#859900") (200 . "#729A1E") (220 . "#609C3C") (240 . "#4E9D5B") (260 . "#3C9F79") (280 . "#2aa198") (300 . "#299BA6") (320 . "#2896B5") (340 . "#2790C3") (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
