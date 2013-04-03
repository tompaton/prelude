(defun xplan/jump-file (path source)
  "Jump to the given source file.

Normalizes the source filename and adds the xplan base folder and the specified path.
Returns the normalized filename (minus xplan base).
"
  ;; TODO: pick up current path
  (let ((xplan-base "c:\\xplanbase\\version\\2.99.999\\")
        ;; normalize filename
        (normalized (with-temp-buffer
                   (insert source)
                   ;; convert list of path components to a proper path
                   (goto-char (point-min))
                   (while (re-search-forward "['\"[:blank:]]+" nil t)
                     (replace-match "" nil t))
                   (goto-char (point-min))
                   (while (search-forward "," nil t)
                     (replace-match "\\" nil t))
                   ;; fix backslashes
                   (goto-char (point-min))
                   (while (search-forward "/" nil t)
                     (replace-match "\\" nil t))
                   (buffer-string))))
    (find-file (concat xplan-base path normalized))
    ;; return resulting filename
    (concat path normalized)))

(defun xplan/jump-method (method)
  "Jump to top level python function in current buffer"
  (goto-char (point-min))
  (if (ignore-errors
        ;; only match at col 1
        (re-search-forward (concat "^def " method "(")))
      (recenter-top-bottom))
  ;; return method
  method)

(defun xplan/jump ()
     "Jump to the appropriate source file/line based on the current line

Follow python imports, urls to request handlers, rpc calls etc."
     (interactive)
     (let ((cur_line (thing-at-point 'line)))
       (cond

        ;; RPC calls
        ((string-match "callJSON(['\\\"]\\(.+\\)\\.\\(.+\\)['\\\"]" cur_line)
         (let ((module (match-string 1 cur_line))
               (file "rpc.py")
               (method (concat "rpc_" (match-string 2 cur_line))))
           ;; rr, iqm1, iqm2 are in insurance subfolder
           (cond ((string-match "\\_<rr\\|rr_sg\\|rr_gb\\|iqm1\\|iqm2\\_>" module)
                  (setq module (concat "insurance\\" module))))
           ;; sysadmin uses rpc for each module
           (cond ((string-match "\\(sysadmin\\)\\.\\(.+\\)" module)
                  (setq file (concat "rpc_" (match-string 2 module) ".py"))
                  (setq module (match-string 1 module))))
           (message "xplan/jump: callJSON --> %s :: %s"
                    (xplan/jump-file "src\\py\\xpt\\" (concat module "\\" file))
                    (xplan/jump-method method))))

        ;; html template, / separated path
        ((string-match "\\(get_popup_template\\|get_full_page_popup_template\\|Template\\)(['\"]\\(.+\\)['\"]" cur_line)
         (message "xplan/jump: get_popup_template --> %s"
                  (xplan/jump-file "data\\ihtml\\" (match-string 2 cur_line))))

        ;; html template, path in list/tuple
        ((string-match "\\_<\\(get\\w*TPO\\|get_.+_template\\|Template\\|getMainFrame\\)(\\[\\([^]]+\\)\\]" cur_line)
         (message "xplan/jump: Template --> %s"
                  (xplan/jump-file "data\\ihtml\\" (match-string 2 cur_line))))

        ;; <:include html_template:>
        ((string-match "<:include \\(.+\\):>" cur_line)
         (message "xplan/jump: include --> %s"
                  (xplan/jump-file "data\\ihtml\\" (match-string 1 cur_line))))

        ;; $ADD_JAVASCRIPT
        ((string-match "$ADD_JAVASCRIPT(['\"]\\(.+\\)['\"])" cur_line)
         (message "xplan/jump: ADD_JAVASCRIPT --> %s"
                  (xplan/jump-file "data\\wwwroot\\js\\" (match-string 1 cur_line))))

        ;; $ADD_CSS
        ((string-match "$ADD_CSS(['\"]\\(.+\\)['\"])" cur_line)
         (message "xplan/jump: ADD_CSS --> %s"
                  (xplan/jump-file "data\\wwwroot\\css\\" (match-string 1 cur_line))))

        ;; url --> protocol req handler
        ((string-match "/\\(sysadmin\\)/\\(supersolver\\)/\\([[:word:]_]+\\)" cur_line)
         (let ((module (match-string 1 cur_line))
               (file (concat "req_" (match-string 2 cur_line) ".py"))
               (method (concat "req_" (match-string 3 cur_line))))
           (message "xplan/jump: url --> %s :: %s"
                    (xplan/jump-file "src\\py\\xpt\\" (concat module "\\" file))
                    (xplan/jump-method method))))
        ((string-match "/\\(iqm\\+/rr\\|supersolver\\)/\\([[:word:]_]+\\)" cur_line)
         (let ((module (match-string 1 cur_line))
               (file "protocol.py")
               (method (concat "req_" (match-string 2 cur_line))))
           ;; rr, iqm1, iqm2 are in insurance subfolder
           (cond ((string-match "\\(\\_<rr\\|rr_sg\\|rr_gb\\|iqm1\\|iqm2\\_>\\)" module)
                  (setq module (concat "insurance\\" (match-string 1 module)))))
           (message "xplan/jump: url --> %s :: %s"
                    (xplan/jump-file "src\\py\\xpt\\" (concat module "\\" file))
                    (xplan/jump-method method))))

        (t
         (message "xplan/jump: match not found")))))

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

;; (require 'jump)
;; (defjump
;;   xplan-find-file
;;   (
;;    ("/supersolver/\\2" . "src\\\\py\\\\xpt\\\\supersolver\\\\protocol.py")
;;    ("callJSON(['\"]\\1\\.\\2['\"]" . "src\\\\py\\\\xpt\\\\\\1\\\\rpc.py")
;;    )
;;   "c:\\xplanbase\\version\\2.99.999\\"
;;   "Follow a logical link from one part of the source to another."
;;   nil
;;   'which-function)

