;; Highlight whitespace characters in buffer

;; Toggle whitespace mode for Emacs session
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Highlight lines over 80 columns. Requires whitespace-style (face)
(setq whitespace-line-column 81)

;; Select types of whitespace to highlight
(setq-default whitespace-style '(face tabs tab-mark newline newline-mark lines-tail trailing))

;; Select characters to represent whitespace
;; All numbers are Unicode codepoint. View with (insert-char 182)
(setq-default whitespace-display-mappings '(
	(tab-mark 9 [9654 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▶」
        (newline-mark 10 [182 10]) ; 10 LINE FEED 「¶」
   ))

;; Customize colors highlighting whitespace
(custom-set-faces
 '(whitespace-line ((t (:foreground "black" :background "magenta"))))
 '(whitespace-newline ((t (:foreground "white" :background nil))))
 '(whitespace-tab ((t (:foreground "white" :background nil))))
 '(whitespace-trailing ((t (:foreground "red" :background "yellow")))))
