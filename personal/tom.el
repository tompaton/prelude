;;; customisations

(add-to-list 'load-path "~/.emacs.d/personal/lib/")

;; load my utility functions
(load-file "~/.emacs.d/personal/lib/toms-utils.el")

;; install packages
(prelude-ensure-module-deps '(ag
                              bm
                              color-identifiers-mode
                              drag-stuff
                              expand-region
                              highlight-symbol
                              multiple-cursors
                              restclient
                              smooth-scrolling
                              ;swiper-helm
                              wgrep
                              which-key
                              whole-line-or-region))

(global-set-key (kbd "<M-SPC>") 'cycle-spacing)

;; quick keys to toggle view
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key [f6] 'whitespace-mode)


;; indent after new line
;(global-set-key (kbd "RET") 'newline-and-indent)

;; remove extra blanks when joining lines
;(global-set-key "\C-k" 'tom/kill-and-join-forward)

;; join lines
(global-set-key (kbd "M-j") 'tom/join-lines)

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
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.2)
(setq highlight-symbol-colors '("yellow" "cyan" "SpringGreen1"))

(global-set-key (kbd "<M-right>") 'highlight-symbol-next)
(global-set-key (kbd "<M-left>") 'highlight-symbol-prev)
(global-set-key (kbd "M-h") 'highlight-symbol-at-point)

(defun tom/prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (highlight-symbol-mode t))
(add-hook 'prelude-prog-mode-hook 'tom/prog-mode-defaults t)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "M-[") 'mc/mark-previous-like-this)
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

(setq mc/cmds-to-run-for-all
      '(backward-sexp
        delete-horizontal-space
        electric-newline-and-maybe-indent
        indent-for-tab-command
        prelude-move-beginning-of-line
        python-backspace
        python-electric-colon
        sgml-slash
        sp--self-insert-command
        sp-remove-active-pair-overlay
        tom/join-lines
        tom/kill-and-join-forward
        whole-line-or-region-kill-ring-save
        whole-line-or-region-yank))

(setq mc/cmds-to-run-once
      '(handle-switch-frame
        previous-multiframe-window))

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; indent/outdent region by 4 spaces
;; python version loses the marked region so only works once
(global-set-key (kbd "C-.") (lambda () (interactive) (indent-rigidly (region-beginning) (region-end) 4)))
(global-set-key (kbd "C-,") (lambda () (interactive) (indent-rigidly (region-beginning) (region-end) -4)))

;; toggle quotes
(global-set-key (kbd "C-c '") 'tom/toggle-quotes)

;; split line on commas and indent (e.g. split python function params to separate lines)
(global-set-key (kbd "C-c C-,") 'tom/split-line-on-comma-and-indent)

;; switch between multiple windows
;(global-set-key (kbd "C-`") 'other-frame)
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-S-tab>") 'previous-multiframe-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-multiframe-window) ;; for ubuntu

;(global-set-key (kbd "C-x o") 'other-window)

;; keep cursor away from the top/bottom of window
(require 'smooth-scrolling)

;; smartparens key bindings trample on selection/movement and don't work in python-mode
(require 'smartparens)
(define-key sp-keymap (kbd "C-<right>") nil)
(define-key sp-keymap (kbd "C-<left>") nil)
(define-key sp-keymap (kbd "C-M-<left>") nil)
(define-key sp-keymap (kbd "C-M-<right>") nil)
(define-key sp-keymap (kbd "M-<up>") nil)
(define-key sp-keymap (kbd "M-<down>") nil)
(define-key sp-keymap (kbd "M-r") nil)
(define-key sp-keymap (kbd "M-s") nil)
(define-key sp-keymap (kbd "M-j") nil)

(global-set-key (kbd "M-'") 'sp-split-sexp)

(require 'drag-stuff)
;; custom bindings, don't use mode
(global-set-key (kbd "<M-up>") 'drag-stuff-up)
(global-set-key (kbd "<M-down>") 'drag-stuff-down)

(require 'whole-line-or-region)

;; c-w without a selection should kill the whole line
(whole-line-or-region-mode 1)

;; shift-arrows shouldn't change windows
(load "shift_mark")

;; editable grep/ack buffers (C-c C-p)
(require 'wgrep)

;; html should use 4 spaces to indent
(setq sgml-basic-offset 4)

;; list-directory command is pointless
(global-set-key (kbd "C-x C-d") 'ido-dired)

;; default ediff side by side
(setq ediff-split-window-function 'split-window-horizontally)

;; i can never remember the name/key for this
(defun unix-eol ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))

(require 'swiper)
(global-set-key (kbd "M-s") 'swiper)

;; better scratch buffer
(setq initial-major-mode 'python-mode)
(setq initial-scratch-message "\
### *SCRATCH* ###
")

(which-key-mode +1)

;; use dwim ag (redefine ag as ag-project-regexp)
;(defun ag (regexp)
;  (interactive (list (ag/read-from-minibuffer "Search regexp")))
;  (ag/search regexp (ag/project-root default-directory) :regexp t))

;; load config from local/ folder based on current machine name
(let ((local-el (concat "~/.emacs.d/personal/local/" system-name ".el")))
  (when (file-exists-p local-el)
    (message "Loading local configuration files in %s..." local-el)
    (load-file local-el)))
