(defun xplan/jump-file (path source &optional CREATE)
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
    (let ((file (concat xplan-base path normalized)))
      (if (or (file-exists-p file)
              CREATE)
          (find-file file)
        ;; indicate file doesn't exist in resulting message
        (setq normalized (concat normalized " (not found)"))))
    ;; return resulting filename
    (concat path normalized)))

(defun xplan/get-file-from-module (module)
  (with-temp-buffer
    (insert module)
    (goto-char (point-min))
    (while (search-forward "." nil t)
      (replace-match "\\\\"))
    (concat (buffer-string) ".py")))

(defun xplan/jump-method (method &optional def)
  "Jump to top level python function in current buffer"
  (unless def
    (setq def "def"))
  (let ((pos (point)))
    (goto-char (point-min))
    (if (ignore-errors
          ;; only match at col 1
          (re-search-forward (concat "^" def " " method "(")))
        (recenter-top-bottom)
      (progn
        ;; restore cursor
        (goto-char pos)
        ;; indicate method doesn't exist in resulting message
        (setq method (concat method " (not found)"))))
    ;; return method
    method))

(defun xplan/jump-rpc (module method)
  "Jump to the rpc handler for 'module.method'."
  (interactive "sModule: \nsMethod: ")
  (let ((file "rpc.py"))
    (setq method (concat "rpc_" method))
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

(defun xplan/jump-url (module file method)
  "Jump to the url handler for url 'module/method', in file (e.g. protocol.py.)"
  (interactive "sModule: \nsFile: \nsMethod: ")
  (setq method (concat "req_" method))
  ;; rr, iqm1, iqm2 are in insurance subfolder
  (cond ((string-match "\\(\\_<rr\\|rr_sg\\|rr_gb\\|iqm1\\|iqm2\\_>\\)" module)
         (setq module (concat "insurance\\" (match-string 1 module)))))
  (message "xplan/jump: url --> %s :: %s"
           (xplan/jump-file "src\\py\\xpt\\" (concat module "\\" file))
           (xplan/jump-method method)))

(defun xplan/jump ()
     "Jump to the appropriate source file/line based on the current line

Follow python imports, urls to request handlers, rpc calls etc."
     (interactive)
     (let ((cur_line (thing-at-point 'line)))
       (cond

        ;; RPC calls
        ((string-match "\\(?:callJSON\\|XMLRPC\\.call\\)(['\\\"]\\(.+\\)\\.\\(.+\\)['\\\"]" cur_line)
         (xplan/jump-rpc (match-string 1 cur_line)
                         (match-string 2 cur_line)))

        ;; html template, / separated path
        ((string-match "\\(get_popup_template\\|get_full_page_popup_template\\|init_engage_tpl\\|Template\\)(['\"]\\(.+\\)['\"]" cur_line)
         (message "xplan/jump: get_popup_template --> %s"
                  (xplan/jump-file "data\\ihtml\\" (match-string 2 cur_line) t))) ; create if necessary

        ;; html template, path in list/tuple
        ;; getTPO(request, [...path...])
        ((string-match "\\_<\\(get\\w*TPO\\)([[:word:]_]+,\\W*\\[\\([^]]+\\)\\]" cur_line)
         (message "xplan/jump: Template --> %s"
                  (xplan/jump-file "data\\ihtml\\" (match-string 2 cur_line) t))) ; create if necessary
        ;; getSimpleTPO([...path...])
        ((string-match "\\_<\\(get\\w*TPO\\|get_.+_template\\|Template\\|getMainFrame\\)(\\[\\([^]]+\\)\\]" cur_line)
         (message "xplan/jump: Template --> %s"
                  (xplan/jump-file "data\\ihtml\\" (match-string 2 cur_line) t))) ; create if necessary

        ;; <:include html_template:>
        ((string-match "<:include \\(.+\\):>" cur_line)
         (message "xplan/jump: include --> %s"
                  (xplan/jump-file "data\\ihtml\\" (match-string 1 cur_line) t))) ; create if necessary

        ;; $ADD_JAVASCRIPT
        ((string-match "$ADD_JAVASCRIPT(['\"]\\(.+\\)['\"])" cur_line)
         (message "xplan/jump: ADD_JAVASCRIPT --> %s"
                  (xplan/jump-file "data\\wwwroot\\js\\" (match-string 1 cur_line))))
        ;; Dependency.addJS
        ((string-match "Dependency.addJS(['\"]\\(.+\\)['\"])" cur_line)
         (message "xplan/jump: Dependency.addJS --> %s"
                  (xplan/jump-file "data\\wwwroot\\js\\"
                                   (concat (match-string 1 cur_line) ".js"))))

        ;; $ADD_CSS
        ((string-match "$ADD_CSS(['\"]\\(.+\\)['\"])" cur_line)
         (message "xplan/jump: ADD_CSS --> %s"
                  (xplan/jump-file "data\\wwwroot\\css\\" (match-string 1 cur_line))))

        ;; url --> protocol req handler
        ((string-match "/\\(sysadmin\\)/\\(supersolver\\)/\\([[:word:]_]+\\)" cur_line)
         (xplan/jump-url (match-string 1 cur_line)
                         (concat "req_" (match-string 2 cur_line) ".py")
                         (match-string 3 cur_line)))
        ((string-match "/\\(iqm\\+/rr\\|supersolver\\)/\\([[:word:]_]+\\)" cur_line)
         (xplan/jump-url (match-string 1 cur_line)
                         "protocol.py"
                         (match-string 2 cur_line)))

        ;; import class/function from module
        ;; TODO: import module from path
        ((string-match "from \\(xpt\\..+\\) import \\(.+\\)" cur_line)
         (let ((module (match-string 1 cur_line))
               (symbol (match-string 2 cur_line)))
           (message "xplan/jump: import --> %s :: %s"
                    (xplan/jump-file "src\\py\\" (xplan/get-file-from-module module))
                    (xplan/jump-method symbol "\\(def\\|class\\)"))))

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

;; html editing for xplan templates
(prelude-ensure-module-deps '(mmm-mode))

(require 'mmm-auto)
(mmm-add-group
 'html-xplan
 '((xplan-expr
    :submode python
    :face mmm-output-submode-face
    :front "<:"
    :back ":>"
    :insert ((?= xplan-expr nil @ "<:" @ " " _ " " @ ":>" @)))
   ))
(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-xplan)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-js)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-css)

(set-face-attribute 'mmm-default-submode-face nil
                    :background "#f0f0ff")
