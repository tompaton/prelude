;;; customisations

(add-to-list 'load-path "~/.emacs.d/personal/lib/")

;; install packages
(prelude-ensure-module-deps '(ag
                              avy
                              bm
                              color-identifiers-mode
                              cycbuf
                              drag-stuff
                              dsvn
                              expand-region
                              highlight-symbol
                              key-chord
                              multiple-cursors
                              smooth-scrolling
                              swiper-helm
                              wgrep
                              browse-kill-ring
                              whole-line-or-region
                              kill-ring-search))

;; load my utility functions
(load-file "~/.emacs.d/personal/lib/toms-utils.el")

(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c DEL") 'tom/delete-trailing-whitespace-current-line)
; delete all spaces around the point
;;(global-set-key (kbd "<S-delete>") (lambda () (interactive) (just-one-space 0)))
(global-set-key (kbd "<M-SPC>") 'cycle-spacing)

;; quick keys to toggle view
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key [f6] 'whitespace-mode)
(global-set-key [f7] 'highlight-indentation-mode)
(global-set-key [f8] 'linum-mode)
(global-set-key [f9] 'color-identifiers-mode)

(require 'cycbuf)
(global-set-key [(meta next)] 'cycbuf-switch-to-next-buffer)
(global-set-key [(meta prior)] 'cycbuf-switch-to-previous-buffer)
;; (global-set-key [(meta shift next)] 'cycbuf-switch-to-next-buffer-no-timeout)
;; (global-set-key [(meta shift prior)] 'cycbuf-switch-to-previous-buffer-no-timeout)

;(global-color-identifiers-mode)

;; indent after new line
(global-set-key (kbd "RET") 'newline-and-indent)

;; remove extra blanks when joining lines
(global-set-key "\C-k" 'tom/kill-and-join-forward)

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

;; expand selection
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c =") 'tom/mark-current-line)

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
        backward-sexp
        sp--self-insert-command
        delete-horizontal-space
        tom/join-lines
        tom/kill-and-join-forward
        prelude-move-beginning-of-line
        sp-remove-active-pair-overlay
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

;; helm
(require 'helm-config)

(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(loop for ext in '("\\.pyc$" "\\.pyo$")
      do (add-to-list 'helm-boring-file-regexp-list ext))

;; find in all buffers
;;(global-set-key (kbd "M-s") 'tom/multi-occur-in-matching-buffers)

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
;; m-up isn't working in move-text to move a single line up any more
;(require 'move-text)
(require 'drag-stuff)
;; custom bindings, don't use mode
(global-set-key (kbd "<M-up>") 'drag-stuff-up)
(global-set-key (kbd "<M-down>") 'drag-stuff-down)
(global-set-key (kbd "<M-S-right>") 'drag-stuff-right)
(global-set-key (kbd "<M-S-left>") 'drag-stuff-left)

(require 'whole-line-or-region)
(custom-set-variables
 '(whole-line-or-region-extensions-alist
   (quote
    ((copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
     (kill-region whole-line-or-region-kill-region nil)
     (kill-ring-save whole-line-or-region-kill-ring-save nil)
     (yank whole-line-or-region-yank nil)
     ;;(move-text-up whole-line-or-region-move-text-up nil)
     ;; add in comment/uncomment single line
     (comment-or-uncomment-region whole-line-or-region-comment-dwim-2 nil)))))

;; c-w without a selection should kill the whole line
(whole-line-or-region-mode 1)

;; shift-arrows shouldn't change windows
(load "shift_mark")

;; Toggle window dedication
(global-set-key [pause] 'tom/toggle-window-dedicated)

;; editable grep/ack buffers (C-c C-p)
(require 'wgrep)

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

;; default ediff side by side
(setq ediff-split-window-function 'split-window-horizontally)

;; i can never remember the name/key for this
(defun unix-eol ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))

;; better find-tag keybindings
;(global-set-key (kbd "M-.") 'tom/find-tag)
(global-set-key (kbd "M-.") 'helm-etags-select)
(global-set-key (kbd "M-,") 'pop-tag-mark)

;; better helm-occur binding
(global-set-key (kbd "C-x c C-o") 'helm-occur)

;; display number of search matches
(require 'anzu)
(global-anzu-mode +1)

;; enable semantic mode so c-x c i is reasonably fast
(semantic-mode 1)
(add-to-list 'semantic-inhibit-functions
             (lambda () (member major-mode '(html-mode mmm-mode))))
(setq semantic-idle-scheduler-idle-time 2)
(setq semantic-idle-scheduler-work-idle-time 120)
(setq semantic-idle-scheduler-max-buffer-size 100000)

;; no flycheck in html as it's choking too often
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'html-tidy flycheck-checkers)))

(setq flycheck-check-syntax-automatically '(save mode-enabled))
(global-flycheck-mode -1)

;; better scratch buffer
(setq initial-major-mode 'python-mode)
(setq initial-scratch-message "\
### *SCRATCH* ###
")

(require 'swiper-helm)
(global-set-key (kbd "C-s") 'isearch-forward)
;; torn between swiper and swiper-helm
;; swiper works better with windows split horizontally, but doesn't do C-w
;; swiper-helm is prettier, but still doesn't do C-w very well or M-e
(global-set-key (kbd "M-s") 'swiper)
(global-set-key (kbd "C-x c s") 'swiper-helm)

(require 'avy)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "C-;") 'avy-goto-word-or-subword-1)
(avy-setup-default)


;; load config from local/ folder based on current machine name
(let ((local-el (concat "~/.emacs.d/personal/local/" system-name ".el")))
  (when (file-exists-p local-el)
    (message "Loading local configuration files in %s..." local-el)
    (load-file local-el)))
