;;; customisations

(add-to-list 'load-path "~/.emacs.d/personal/lib/")

;; install packages
(prelude-ensure-module-deps '(bm
                              dsvn
                              expand-region
                              highlight-symbol
                              hlinum
                              multiple-cursors
                              smooth-scrolling
                              ack-and-a-half
                              wgrep
                              wgrep-ack
                              browse-kill-ring
                              kill-ring-search
                              fill-column-indicator))

;; load my utility functions
(load-file "~/.emacs.d/personal/lib/toms-utils.el")

;; highlight everything in whitespace-mode except long lines
(require 'whitespace)

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
          (space-mark   32 [183] [46])      ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10])        ; 10 LINE FEED
          (tab-mark      9 [8594 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
          ))
  (setq indicate-empty-lines t))

(tom/customise-whitespace-mode-faces)

(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c DEL") 'tom/delete-trailing-whitespace-current-line)
; delete all spaces around the point
(global-set-key (kbd "<S-delete>") (lambda () (interactive) (just-one-space 0)))

;; add a vertical line at column 100
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq-default fill-column 100)
(setq-default fci-rule-color "#dddddd")

;; tone down flymake errors and warnings
;(require 'flymake)
;(set-face-attribute 'flymake-errline nil
;                    :background "#ffe8e8")

;; quick keys to toggle view
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key [f6] 'whitespace-mode)
(global-set-key [f7] 'highlight-indentation-mode)
(global-set-key [f8] 'linum-mode)

;; word-wrap by default
(setq default-truncate-lines t)

;; show line numbers
(require 'hlinum)
(global-linum-mode)

;; indent after new line
(global-set-key (kbd "RET") 'newline-and-indent)

;; insert new lines, a bit like vi
(global-set-key (kbd "M-o") 'prelude-smart-open-line)
(global-set-key (kbd "M-O") 'prelude-smart-open-line-above)

;; remove extra blanks when joining lines
(global-set-key "\C-k" 'tom/kill-and-join-forward)

;; join lines
(global-set-key (kbd "M-j") 'tom/join-lines)

;; move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; visual studio style bookmarks
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(setq bm-highlight-style 'bm-highlight-only-fringe)
;; toggle bookmarks by clicking in the fringe
(global-set-key (kbd "<left-fringe> <mouse-1>") #'(lambda(event)
                                                    (interactive "e")
                                                    (save-excursion
                                                      (mouse-set-point event)
                                                      (bm-toggle))))

;; highlight symbol under point and alt-left/right to navigate between references
(setq highlight-symbol-idle-delay 0.2)
(setq highlight-symbol-colors '("yellow" "cyan" "SpringGreen1"))

(global-set-key (kbd "<M-right>") 'highlight-symbol-next)
(global-set-key (kbd "<M-left>") 'highlight-symbol-prev)
(global-set-key (kbd "M-h") 'highlight-symbol-at-point)

(defun tom/prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (highlight-symbol-mode t))
(add-hook 'prelude-prog-mode-hook 'tom/prog-mode-defaults t)

;; expand selection
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "M-[") 'mc/mark-previous-like-this)
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

(setq mc/cmds-to-run-for-all
      '(python-electric-colon
        sgml-slash
        python-backspace
        backward-sexp))
(setq mc/cmds-to-run-once
      '(handle-switch-frame
        previous-multiframe-window))

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; indent/outdent region by 4 spaces
;; python version loses the marked region so only works once
(global-set-key (kbd "C-.") [?\C-u    ?4 ?\M-x ?i ?n ?d ?e ?n ?t ?- ?r ?i ?g ?i ?d ?l ?y return])
(global-set-key (kbd "C-,") [?\C-u ?- ?4 ?\M-x ?i ?n ?d ?e ?n ?t ?- ?r ?i ?g ?i ?d ?l ?y return])

;; toggle quotes
(global-set-key (kbd "C-c '") 'tom/toggle-quotes)

;; split line on commas and indent (e.g. split python function params to separate lines)
(global-set-key (kbd "C-c ,") 'tom/split-line-on-comma-and-indent)

;; jump to next = character
(key-chord-define-global "j=" (lambda ()
                                (interactive)
                                (search-forward "=")
                                (left-char 1)))
;; jump to prev ( character
(key-chord-define-global "j9" (lambda ()
                                (interactive)
                                (search-backward "(")))
;; jump to next ) character
(key-chord-define-global "j0" (lambda ()
                                (interactive)
                                (search-forward ")")))

(key-chord-define-global "kk" 'just-one-space)
(key-chord-define-global "KK" 'delete-horizontal-space)

;; helm
(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(loop for ext in '("\\.pyc$" "\\.pyo$")
      do (add-to-list 'helm-c-boring-file-regexp-list ext))

;; find in all buffers
(global-set-key (kbd "M-s") 'tom/multi-occur-in-matching-buffers)

;; switch between multiple windows
(global-set-key (kbd "C-`") 'other-frame)
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-S-tab>") 'previous-multiframe-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-multiframe-window) ;; for ubuntu
(global-set-key (kbd "C-x o") 'other-window)

;; transpose windows
(require 'transpose-frame)
(global-set-key (kbd "C-x t") 'transpose-frame)

;; keep cursor away from the top/bottom of window
(require 'smooth-scrolling)

;; better keys for move line up/down
(global-set-key (kbd "<M-up>") 'prelude-move-line-up)
(global-set-key (kbd "<M-down>") 'prelude-move-line-down)

;; shift-arrows shouldn't change windows
(load "shift_mark")

;; Toggle window dedication
(global-set-key [pause] 'tom/toggle-window-dedicated)

;; .svn support for finding project root
(require 'ack-and-a-half)
(add-to-list 'ack-and-a-half-project-root-file-patterns "\\`.svn\\'")

;; editable grep/ack buffers (C-c C-p)
(require 'wgrep)
(require 'wgrep-ack)

;; browse & search kill ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-c y") 'browse-kill-ring)
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)

;; cycle buffer through major modes I use that may not be detected
;; correctly by emacs
;; (global-set-key (kbd "<f9>") #'(lambda ()
;;                                  (interactive)
;;                                  (tom/cycle-modes '(python-mode
;;                                                     html-mode
;;                                                     javascript-mode
;;                                                     fundamental-mode))))

;; cycling modes is problematic as python-mode leaves broken flycheck- files around everywhere
;; so set up some shortcuts to go directly to the mode I'm after
(global-set-key (kbd "C-c mh") 'html-mode)
(global-set-key (kbd "C-c mp") 'python-mode)
(global-set-key (kbd "C-c mj") 'javascript-mode)
(global-set-key (kbd "C-c ml") 'lisp-interaction-mode)

;; html should use 4 spaces to indent
(setq sgml-basic-offset 4)

;; list-directory command is pointless
(global-set-key (kbd "C-x C-d") 'ido-dired)

;; raise-frame method is annoying
(setq ido-default-buffer-method 'selected-window)

;; i can never remember the name/key for this
(defun unix-eol ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))

;; better find-tag keybindings
(global-set-key (kbd "M-.") 'tom/find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)

;; load config from local/ folder based on current machine name
(let ((local-el (concat "~/.emacs.d/personal/local/" system-name ".el")))
  (when (file-exists-p local-el)
    (message "Loading local configuration files in %s..." local-el)
    (load-file local-el)))

