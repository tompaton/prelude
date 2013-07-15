;;; undo unwanted prelude defaults

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problematic key bindings

(setq prelude-guru nil)            ; i like arrow keys as well as emacs nav keys
(global-set-key (kbd "C-v") 'yank) ; C-v for page down is a pain, my fingers want paste

(put 'prelude-kill-other-buffers 'disabled t) ; C-c k is easily mistyped when going for C-x k

(global-set-key (kbd "C-x C-c") nil) ; quitting by mistake is really annoying, M-x save-buffers-kill-terminal
(global-set-key (kbd "C-z") nil) ; as is minimizing window

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; philosophical differences

;; no point hamstringing myself by hiding the menu
(menu-bar-mode t)

;; don't truncate trailing whitespace on save as it makes diffs too noisy
(setq prelude-clean-whitespace-on-save nil)

;; don't auto reload (TODO: not working...?)
(global-auto-revert-mode nil)

;; don't auto save
(setq prelude-auto-save nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; things that were removed from prelude that I'd gotten used to

(prelude-ensure-module-deps '(yasnippet))
(require 'yasnippet)
(defvar personal-snippets-dir (expand-file-name "snippets" prelude-personal-dir))
(add-to-list 'yas-snippet-dirs personal-snippets-dir)
(yas-global-mode 1)

;; insert new lines, a bit like vi
(global-set-key (kbd "M-o") 'prelude-smart-open-line)
(global-set-key (kbd "M-O") 'prelude-smart-open-line-above)
