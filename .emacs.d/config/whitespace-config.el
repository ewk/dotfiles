;;; Commentary

;; Enable whitespace mode globally
(require 'whitespace)
 (setq whitespace-style '(face empty tabs tab-mark empty lines-tail trailing))
(global-whitespace-mode t)

;; But provide an off switch
(global-set-key (kbd "C-c w") 'whitespace-mode) ; view all whitespace characters
;; Empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; This is likely to interfere with version control and next-line-add-newlines
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Tabs and spaces
(setq-default indent-tabs-mode t) ; whether tabs are used for indentation

;; tab-width, c-basic-offset, and cperl-indent-level should always be the same
(setq-default tab-width 8) ;; How wide a tab is
;; Indentation in CC mode applies to C and most languages
(setq-default c-basic-offset 8)
(setq-default cperl-indent-level 8) ;; Perl, of course, is different
;;(setq c-default-style "linux")

;; Do not wrap text in programming mode
(add-hook 'c-mode-hook (lambda () (setq truncate-lines t)))

;; Syntax of these languages can be fussy
;; FileType c,perl,sh,zsh,mmix inherit CC mode
;; Make must always uses tabs, never spaces.

;; Go uses 4 space tabs and no wrapping; should be set by plugin
;; 2 soft spaces
(setq html-indent-level 2)

;; 2 soft spaces
(setq ruby-indent-level 2)

;; 4 space tabs
(setq css-indent-offset 4)

(setq sentence-end-double-space nil) ; Sentences end with one space

;; Auto indent on newline - Ctrl-j
(define-key global-map (kbd "RET") 'newline-and-indent)
;; Add blank line to end of buffer
(setq next-line-add-newlines t)
