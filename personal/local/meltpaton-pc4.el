;;; xplan/cygwin specific customisations

(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

;; cygwin bash as shell
(setq exec-path (cons "c:/cygwin/bin" exec-path))
(require 'setup-cygwin)

;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;(require 'w32-symlinks)
;;(setq w32-symlinks-handle-shortcuts t)

;;(require 'flymake-python-pyflakes)
;;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;;(require 'find-file-in-project)
;;(setq ffip-project-file ".svn")
;;(global-set-key (kbd "C-x f") 'find-file-in-project)


;;(add-to-list 'load-path "~/.emacs.d/personal/ack-and-a-half.el")

;; (require 'ack-and-a-half)
;; ;; Create shorter aliases
;; (defalias 'ack 'ack-and-a-half)
;; (defalias 'ack-same 'ack-and-a-half-same)
;; (defalias 'ack-find-file 'ack-and-a-half-find-file)
;; (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; (require 'jump)
;; (defjump
;;   'rinari-find-model
;;   '(("app/controllers/\\1_controller.rb#\\2"  . "app/models/\\1.rb#\\2")
;;     ("app/views/\\1/.*"                       . "app/models/\\1.rb")
;;     ("app/helpers/\\1_helper.rb"              . "app/models/\\1.rb")
;;     ("db/migrate/.*create_\\1.rb"             . "app/models/\\1.rb")
;;     ("test/functional/\\1_controller_test.rb" . "app/models/\\1.rb")
;;     ("test/unit/\\1_test.rb#test_\\2"         . "app/models/\\1.rb#\\2")
;;     ("test/unit/\\1_test.rb"                  . "app/models/\\1.rb")
;;     ("test/fixtures/\\1.yml"                  . "app/models/\\1.rb")
;;     (t                                        . "app/models/"))
;;   'rinari-root
;;   "Go to the most logical model given the current location."
;;   '(lambda (path)
;;      (message (shell-command-to-string
;; 	       (format "ruby %sscript/generate model %s"
;; 		       (rinari-root)
;; 		       (and (string-match ".*/\\(.+?\\)\.rb" path)
;; 			    (match-string 1 path))))))
;;   'ruby-add-log-current-method)

(require 'jump)
(defjump
  xplan-find-file
  (
   ("/supersolver/\\2" . "src\\\\py\\\\xpt\\\\supersolver\\\\protocol.py")
   ("callJSON(['\"]\\1\\.\\2['\"]" . "src\\\\py\\\\xpt\\\\\\1\\\\rpc.py")
   )
  "c:\\xplanbase\\version\\2.99.999\\"
  "Follow a logical link from one part of the source to another."
  nil
  'which-function)

;; one frame per monitor
(make-frame)
(make-frame)

;; menu-bar-on
(menu-bar-mode)
