;;; xplan/cygwin specific customisations

(setq python-shell-interpreter "C:\\Python27\\python.exe")

;; one frame per monitor
(make-frame)
(make-frame)

;; menu-bar-on (make-frame turns it off)
(menu-bar-mode)

;; seems like these settings get lost after opening multiple frames
(tom/customise-whitespace-mode-faces)

;; project specific navigation
(load "~/.emacs.d/personal/lib/xplan.el")
(global-set-key (kbd "C-c j") 'xplan/jump)
(global-set-key (kbd "C-c Jo") (lambda () (interactive) (xplan/jump 1))) ; other frame
(global-set-key (kbd "C-c JO") (lambda () (interactive) (xplan/jump 1) (previous-multiframe-window))) ; show in other frame, keep focus in current frame
(global-set-key (kbd "C-c Jp") (lambda () (interactive) (xplan/jump -1))) ; previous frame
(global-set-key (kbd "C-c JP") (lambda () (interactive) (xplan/jump -1) (next-multiframe-window))) ; show in previous frame, keep focus in current frame
(global-set-key (kbd "C-c Jr") 'xplan/jump-rpc)
(global-set-key (kbd "C-c Ju") 'xplan/jump-url)
(global-set-key (kbd "C-c Jb") 'xplan/jump-branch)

(setq recentf-exclude '(xplan/non-trunk-file-p))

;; TAGS files for xplan trunk branch
(setq tags-table-list
      '("c:/xplanbase/version/9.99.999/src/py/xpt/TAGS"
        "c:/xplanbase/version/9.99.999/src/py/xlib/TAGS"
        "c:/xplanbase/version/9.99.999/data/wwwroot/js/TAGS"))
