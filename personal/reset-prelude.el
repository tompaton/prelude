;;; undo unwanted prelude defaults

;; i like arrow keys too
(setq prelude-guru nil)

;; C-v for page down is a pain, my fingers want paste
(global-set-key (kbd "C-v") 'yank)

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

