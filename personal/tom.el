;;; customisations

(add-to-list 'load-path "~/.emacs.d/personal/lib/")

;; highlight everything in whitespace-mode except long lines
(setq whitespace-style (quote
                        (spaces tabs trailing newline space-before-tab indentation empty space-after-tab space-mark tab-mark newline-mark)))
(setq indicate-empty-lines t)

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

;; c-a should toggle between start of line and start of indentation
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

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
(highlight-symbol-mode t)
(global-set-key (kbd "<M-right>") 'highlight-symbol-next)
(global-set-key (kbd "<M-left>") 'highlight-symbol-prev)
(global-set-key (kbd "M-h") 'highlight-symbol-at-point)

;; expand selection
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "M-[") 'mc/mark-previous-like-this)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; indent/outdent region by 4 spaces
;; python version loses the marked region so only works once
(global-set-key (kbd "C-.") [?\C-u    ?4 ?\M-x ?i ?n ?d ?e ?n ?t ?- ?r ?i ?g ?i ?d ?l ?y return])
(global-set-key (kbd "C-,") [?\C-u ?- ?4 ?\M-x ?i ?n ?d ?e ?n ?t ?- ?r ?i ?g ?i ?d ?l ?y return])

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

