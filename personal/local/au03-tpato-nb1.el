;;; xplan/cygwin specific customisations

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

;; use find from cygwin
(setq find-program "C:/cygwin64/bin/find")
(setq find-ls-option '("-ls" . "-dilsb"))

;; TAGS files for xplan trunk branch
(setq tags-table-list
      '("c:/xplanbase/version/99.99.999/src/py/xpt/TAGS"
        "c:/xplanbase/version/99.99.999/src/py/xlib/TAGS"
        "c:/xplanbase/version/99.99.999/data/wwwroot/js/TAGS"))

;; helm

(require 'helm-config)

(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(loop for ext in '("\\.pyc$" "\\.pyo$")
      do (add-to-list 'helm-boring-file-regexp-list ext))


(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-hide-emphasis-markers t)

(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
