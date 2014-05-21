;;; tom's "theme"

(prelude-ensure-module-deps '(fill-column-indicator
                              hlinum
                              powerline
                              col-highlight))

;; stick to white background & usable colours
(disable-theme 'zenburn )

;; don't want "emacs Prelude - " in the taskbar, the icon & buffer name are sufficient
(setq frame-title-format (cdddr frame-title-format))

;; add a vertical line at column 100
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq-default fill-column 80)
(setq-default fci-rule-color "#dddddd")

;; tone down flymake errors and warnings
                                        ;(require 'flymake)
                                        ;(set-face-attribute 'flymake-errline nil
                                        ;                    :background "#ffe8e8")

;; word-wrap by default
(set-default 'truncate-lines t)

;; highlight everything in whitespace-mode except long lines
(require 'whitespace)

;; show line numbers
(require 'hlinum)
(global-linum-mode)

(defun tom/customise-whitespace-mode-faces () ; defun so can be called again in meltpaton-pc4.el
  (setq whitespace-style (quote
                          (face spaces tabs trailing newline space-before-tab indentation empty space-after-tab space-mark tab-mark newline-mark)))
  ;; no yellow background on spaces
  (set-face-attribute 'whitespace-space nil
                      :background nil)
  ;; subtler trailing space background
  (set-face-attribute 'whitespace-trailing nil
                      :foreground "#F88"
                      :background nil
                      :weight 'light)
  ;; nicer tab & cr indicators
  (setq whitespace-display-mappings
        ;; all numbers are Unicode codepoint in decimal
        '(
          (space-mark   32 [183] [46])      ; 32 SPACE, 183 MIDDLE DOT, 46 FULL STOP
          (newline-mark 10 [182 10])        ; 10 LINE FEED
          (tab-mark      9 [8594 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE
          ))
  (setq indicate-empty-lines t))

(tom/customise-whitespace-mode-faces)

(require 'col-highlight)
(set-face-attribute 'col-highlight nil
                    :background "honeydew")

;; powerline
;; NOTE: on windows, had to delete .elc files in elpa/powerline- folder
;; to remove incessant "pl/ generating ..." messages
(require 'powerline)
;; default colours are way too dark
(set-face-attribute 'mode-line nil :background "grey95")
(set-face-attribute 'powerline-active1 nil :background "grey75")
(set-face-attribute 'powerline-active2 nil :background "grey60")
(set-face-attribute 'powerline-inactive1 nil :background "grey75")
(set-face-attribute 'powerline-inactive2 nil :background "grey60")
;;(powerline-default-theme)

;; custom powerline theme
(defun tom/powerline-theme ()
  "Setup the default mode-line, based on powerline-default-theme.
   - emphasize buffer id, which-func-mode, major and line/col."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (powerline-raw '(" " (:eval (ignore-errors (which-function)))))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-minor-modes face2 'l)
                                     (powerline-narrow face2 'l)
                                     (powerline-raw " " face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-buffer-size face1 'l)
                                     (powerline-raw " " face1)
                                     (powerline-raw "%6p" face1 'r)
                                     (powerline-raw mode-line-mule-info face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw "%4l" mode-line 'l)
                                     (powerline-raw ":" mode-line 'l)
                                     (powerline-raw "%3c" mode-line 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(tom/powerline-theme)

;; diminish minor modes
;; only want things like flycheck that show useful info
(require 'diminish)
(diminish 'whitespace-mode)
(require 'highlight-symbol)
(diminish 'highlight-symbol-mode)
(diminish 'smartparens-mode)
(diminish 'projectile-mode)
(diminish 'prelude-mode)
(require 'yasnippet)
(diminish 'yas-minor-mode)
(diminish 'abbrev-mode)
(require 'anzu)
(diminish 'anzu-mode)
(require 'whole-line-or-region)
(diminish `whole-line-or-region-mode)
(require 'drag-stuff)
(diminish `drag-stuff-mode)

;; make helm current line (C-z) match selection
(custom-set-faces
 '(helm-selection-line ((t (:background "#b5ffd1" :underline nil))))
 '(magit-item-highlight ((t (:background "DarkSeaGreen1")))))
