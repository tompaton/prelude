;;; xplan/cygwin specific customisations

(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

;; cygwin bash as shell
;; need to add CYGWIN=nodosfilewarning to Environment Variables
(setq exec-path (cons "c:/cygwin/bin" exec-path))
;(require 'setup-cygwin)

(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash")
        (explicit-bash-args '("--login" "-i")))
    (call-interactively 'shell)))

;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; for pdb in cygwin-shell
(setenv "PYTHONUNBUFFERED" "x")

;;(require 'w32-symlinks)
;;(setq w32-symlinks-handle-shortcuts t)

;; getting flake8 working in windows
;; 1) add C:\Python27\Scripts to PATH
;; 2) easy_install pip
;; 3) pip install flake8
;; 4) config in ~/.flake8
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

;; getting ack working in windows
;; should be able to have a single copy of ack that runs in cmd.exe and cygwin bash
;; 1) install strawberry perl
;; 2) set up proxy in cpan
;;     cpan> o conf http_proxy http://meldevproxy1:80
;; 3) install ack (have to force as tests don't pass on windows)
;;     cpan> force install App::Ack
;; 4) make sure ~/.ackrc is valid (not a cygwin symlink)
;; 5) can't use cygwin bash as emacs shell
;; 6) fix ack-and-a-half-run so stdin is closed:
(require 'ack-and-a-half)
(defun ack-and-a-half-run (directory regexp pattern &rest arguments)
  "Run ack in DIRECTORY with ARGUMENTS."
  (let ((default-directory (if directory
                               (file-name-as-directory (expand-file-name directory))
                             default-directory)))
    (setq arguments (append ack-and-a-half-arguments
                            (ack-and-a-half-arguments-from-options regexp)
                            arguments
                            (list "--")
                            (list (shell-quote-argument pattern))
                            ;; close stdin
                            (list (concat " < " null-device))
                            ))
    (make-local-variable 'compilation-buffer-name-function)
    (let (compilation-buffer-name-function)
      (setq compilation-buffer-name-function 'ack-buffer-name)
      (compilation-start (mapconcat 'identity (nconc (list ack-and-a-half-executable) arguments) " ")
                         'ack-and-a-half-mode))))

;; one frame per monitor
(make-frame)
(make-frame)

;; menu-bar-on (make-frame turns it off)
(menu-bar-mode)

;;(require 'find-file-in-project)
;;(setq ffip-project-file ".svn")
;;(global-set-key (kbd "C-x f") 'find-file-in-project)

;; project specific navigation
(load "~/.emacs.d/personal/lib/xplan.el")
(global-set-key (kbd "C-c j") 'xplan/jump)
(global-set-key (kbd "C-c Jr") 'xplan/jump-rpc)
(global-set-key (kbd "C-c Ju") 'xplan/jump-url)

