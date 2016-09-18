;;; whitespace-config.el --- Summary

;;; Commentary:

;;; Code:

;; Toggle whitespace mode for Emacs session
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Highlight lines over 80 columns. Requires whitespace-style (face)
(setq whitespace-line-column 81)

;; Select types of whitespace to highlight
(setq whitespace-style '(face tabs tab-mark newline newline-mark lines-tail trailing))

;; Select characters to represent whitespace
;; All numbers are Unicode codepoint. View with (insert-char 182)
(setq whitespace-display-mappings '(
	(tab-mark 9 [9654 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▶」
        (newline-mark 10 [182 10]) ; 10 LINE FEED 「¶」
   ))

;; Customize colors highlighting whitespace
(custom-set-faces
  '(whitespace-line ((t (:foreground "black" :background "magenta"))))
  '(whitespace-newline ((t (:foreground "white" :background nil))))
  '(whitespace-tab ((t (:foreground "white" :background nil))))
  '(whitespace-trailing ((t (:foreground "red" :background "yellow"))))
  )

;; Empty lines
(setq-default indicate-empty-lines t)

;; This may interfere with version control and next-line-add-newlines
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
(setq html-indent-level 2) ;; 2 soft spaces
(setq ruby-indent-level 2) ;; 2 soft spaces
(setq css-indent-offset 4) ;; 4 space tabs

 ; Sentences end with one space
(setq sentence-end-double-space nil)

;; Auto indent on newline - Ctrl-j
(define-key global-map (kbd "RET") 'newline-and-indent)
;; Add blank line to end of buffer
(setq next-line-add-newlines t)

(provide 'whitespace-config)
;;; whitespace-config ends here
