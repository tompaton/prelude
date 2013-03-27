;;; undo unwanted prelude defaults

;; problematic key bindings:
(setq prelude-guru nil)            ; i like arrow keys as well as emacs nav keys
(global-set-key (kbd "C-v") 'yank) ; C-v for page down is a pain, my fingers want paste

;; C-c k prelude-kill-other-buffers is easily mistyped when going for C-x k
(put 'prelude-kill-other-buffers 'disabled t)

;; no point hamstringing myself by hiding the menu
(menu-bar-mode t)

;; stick to white background & usable colours
(disable-theme 'zenburn )

;; don't truncate trailing whitespace on save as it makes diffs too noisy
(setq prelude-whitespace nil)

;; don't auto reload (TODO: not working...?)
(global-auto-revert-mode nil)

;; don't auto save
(setq prelude-auto-save nil)

