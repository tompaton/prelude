(put 'crux-kill-other-buffers 'disabled t) ; C-c k is easily mistyped when going for C-x k

(global-set-key (kbd "C-x C-c") nil) ; quitting by mistake is really annoying, M-x save-buffers-kill-terminal
(global-set-key (kbd "C-z") nil) ; as is minimizing window

;; auto escaping inside strings doesn't seem helpful anywhere
;; but can't disable it, so going to try and learn to use it and sp-split-sexp
;;(setq sp-autoescape-string-quote nil)
;;(setq sp-escape-quotes-after-insert nil)
;;(setq sp-escape-wrapped-region nil)
;;(sp-pair "\"" nil :actions '(:rem escape))

;; C-c keybindings clash with my xplan commands
;;(global-set-key (kbd "s-.") nil)
(global-set-key (kbd "C-c J") nil)

;; no point hamstringing myself by hiding the menu
(menu-bar-mode t)

;; don't want it saving automatically, recover-session is better
(super-save-mode -1)
(setq auto-save-default nil)
