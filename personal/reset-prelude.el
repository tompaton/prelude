;;; undo unwanted prelude defaults

;; problematic key bindings:
(setq prelude-guru nil)            ; i like arrow keys as well as emacs nav keys
(global-set-key (kbd "C-v") 'yank) ; C-v for page down is a pain, my fingers want paste

;; C-c k prelude-kill-other-buffers is easily mistyped when going for C-x k
(put 'prelude-kill-other-buffers 'disabled t)

;; quitting by mistake is really annoying, M-x save-buffers-kill-terminal
(global-set-key (kbd "C-x C-c") nil)

;; no point hamstringing myself by hiding the menu
(menu-bar-mode t)

;; stick to white background & usable colours
(disable-theme 'zenburn )

;; don't truncate trailing whitespace on save as it makes diffs too noisy
(setq prelude-clean-whitespace-on-save nil)

;; don't auto reload (TODO: not working...?)
(global-auto-revert-mode nil)

;; don't auto save
(setq prelude-auto-save nil)

;; don't want "emacs Prelude - " in the taskbar, the icon & buffer name are sufficient
(setq frame-title-format (cdddr frame-title-format))

