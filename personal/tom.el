;;; customisations

(add-to-list 'load-path "~/.emacs.d/personal/lib/")

;; install packages
(prelude-ensure-module-deps '(bm
                              dsvn
                              expand-region
                              highlight-symbol
                              hlinum
                              jump
                              jump-char
                              multiple-cursors
                              smooth-scrolling
                              ack-and-a-half
                              wgrep
                              wgrep-ack
                              browse-kill-ring
                              kill-ring-search
                              fill-column-indicator))

;; highlight everything in whitespace-mode except long lines
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
(setq indicate-empty-lines t)

;; add a vertical line at column 100
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq-default fill-column 100)
(setq-default fci-rule-color "#dddddd")

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

;; remove extra blanks when joining lines
(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;; join lines
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; c-a should toggle between start of line and start of indentation
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; jump-char
(require 'jump-char)
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-S-m") 'jump-char-backward)

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
(global-set-key (kbd "<M-right>") 'highlight-symbol-next)
(global-set-key (kbd "<M-left>") 'highlight-symbol-prev)
(global-set-key (kbd "M-h") 'highlight-symbol-at-point)

(defun my-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (highlight-symbol-mode t))
(add-hook 'prelude-prog-mode-hook 'my-prog-mode-defaults t)

;; expand selection
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "M-[") 'mc/mark-previous-like-this)
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

(add-to-list 'mc/cmds-to-run-for-all 'python-backspace)
(add-to-list 'mc/cmds-to-run-once    'handle-switch-frame)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; indent/outdent region by 4 spaces
;; python version loses the marked region so only works once
(global-set-key (kbd "C-.") [?\C-u    ?4 ?\M-x ?i ?n ?d ?e ?n ?t ?- ?r ?i ?g ?i ?d ?l ?y return])
(global-set-key (kbd "C-,") [?\C-u ?- ?4 ?\M-x ?i ?n ?d ?e ?n ?t ?- ?r ?i ?g ?i ?d ?l ?y return])

;; toggle quotes
(defun toggle-quotes ()
  "Toggle single quoted string to double or vice versa, and
  flip the internal quotes as well.  Best to run on the first
  character of the string."
  (interactive)
  (save-excursion
    (re-search-backward "[\"']")
    (let* ((start (point))
           (old-c (char-after start))
           new-c)
      (setq new-c 
            (case old-c
              (?\" "'")
              (?\' "\"")))
      (setq old-c (char-to-string old-c))
      (delete-char 1)
      (insert new-c)
      (re-search-forward old-c)
      (backward-char 1)
      (let ((end (point)))
        (delete-char 1)
        (insert new-c)
        (replace-string new-c old-c nil (1+ start) end)))))
(global-set-key (kbd "C-c '") 'toggle-quotes)

;; helm
(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(loop for ext in '("\\.pyc$" "\\.pyo$")
      do (add-to-list 'helm-c-boring-file-regexp-list ext))

;; find in all buffers
(defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))
(global-set-key (kbd "M-s") 'my-multi-occur-in-matching-buffers)

;; switch between multiple windows
(global-set-key (kbd "C-`") 'other-frame)
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-S-tab>") 'previous-multiframe-window)
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
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(global-set-key [pause] 'toggle-window-dedicated)

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
(defun cycle-modes (modes)
  (let ((next-mode (cadr (memq major-mode modes))))
    (unless next-mode
      (setq next-mode (car modes)))
    (funcall next-mode)))
(global-set-key (kbd "<f9>") #'(lambda ()
                                 (interactive)
                                 (cycle-modes '(python-mode
                                                html-mode
                                                javascript-mode
                                                fundamental-mode))))

;; html should use 4 spaces to indent
(setq sgml-basic-offset 4)

;; load config from local/ folder based on current machine name
(let ((local-el (concat "~/.emacs.d/personal/local/" system-name ".el")))
  (when (file-exists-p local-el)
    (message "Loading local configuration files in %s..." local-el)
    (load-file local-el)))

