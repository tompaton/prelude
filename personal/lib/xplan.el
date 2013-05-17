(setq xplan/TRUNK "2.99.999")

(defun xplan/normalize-path (source)
  "Convert list of path components to a path if necessary
and change backslashes to forward slashes."
  (with-temp-buffer
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
    (buffer-string)))

(defun xplan/branch-base ()
  "Get the current branch base path from the current file."
  (xplan/normalize-path
   (let ((file (or (buffer-file-name) "")))
     (cond ((string-match "\\(c:.xplanbase.version.2.[0-9]+.999.\\)" file)
            (match-string 1 file))
           (t
            (concat "c:\\xplanbase\\version\\" xplan/TRUNK "\\"))))))

(defun xplan/jump-file (path source &optional CREATE)
  "Jump to the given source file.

Normalizes the source filename and adds the xplan base folder and the specified path.
Returns the normalized filename (minus xplan base).
"
  (let ((xplan-base (xplan/branch-base))
        (normalized (xplan/normalize-path source)))
    (let ((file (concat xplan-base path normalized)))
      (if (or (file-exists-p file)
              CREATE)
          (find-file file)
        ;; indicate file doesn't exist in resulting message
        (setq normalized (concat normalized " (not found)"))))
    ;; return resulting filename
    (concat path normalized)))

(defun xplan/is-file (path source)
  "Does the source file exist in the xplan path."
  (let ((xplan-base (xplan/branch-base))
        (normalized (xplan/normalize-path source)))
    (file-exists-p (concat xplan-base path normalized))))

(defun xplan/get-file-from-module (module)
  (with-temp-buffer
    (insert module)
    (goto-char (point-min))
    (while (search-forward "." nil t)
      (replace-match "\\\\"))
    (concat (buffer-string) ".py")))

(defun xplan/jump-line-number (line)
  "Jump to the line number in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line))
  line)

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

(defun xplan/jump-url-handler (module file method)
  "Jump to the url handler for url 'module/method', in file (e.g. protocol.py.)"
  (setq method (concat "req_" method))
  ;; rr, iqm1, iqm2 are in insurance subfolder
  (cond ((string-match "\\(\\_<rr\\|rr_sg\\|rr_gb\\|iqm1\\|iqm2\\_>\\)" module)
         (setq module (concat "insurance\\" (match-string 1 module)))))
  (message "xplan/jump: url --> %s :: %s"
           (xplan/jump-file "src\\py\\xpt\\" (concat module "\\" file))
           (xplan/jump-method method)))

(defun xplan/jump-url (url)
  "Jump to the handler for 'url'"
  (interactive "sUrl: ")
  (let ((bits (split-string url "/")))
    (if (string= "" (car bits))
        (setq bits (cdr bits)))
    (cond ((= (length bits) 2)
           (xplan/jump-url-handler (nth 0 bits)
                                   "protocol.py"
                                   (nth 1 bits)))
          ((string= (nth 0 bits) "iqm+")
           (xplan/jump-url-handler (concat "insurance/" (nth 1 bits))
                                   "protocol.py"
                                   (nth 2 bits)))
          (t
           (xplan/jump-url-handler (nth 0 bits)
                                   (concat "req_" (nth 1 bits) ".py")
                                   (nth 2 bits))))))

(defun xplan/jump ()
     "Jump to the appropriate source file/line based on the current line

Follow python imports, urls to request handlers, rpc calls etc."
     (interactive)
     (let ((cur_line (thing-at-point 'line)))
       (cond

        ;; pyflakes warning/error
        ((string-match "^\\([a-zA-Z0-9_\\./]+\\):\\([0-9]+\\):" cur_line)
         (let ((file (match-string 1 cur_line))
               (line (string-to-number (match-string 2 cur_line))))
           (message "xplan/jump: file:line:msg --> %s (%d)"
                    (xplan/jump-file "" file)
                    (xplan/jump-line-number line))))

        ;; Traceback error line
        ((string-match "^[ ]+File \\\"\\(.+\\)\\\", line \\([0-9]+\\)," cur_line)
         (let ((file (match-string 1 cur_line))
               (line (string-to-number (match-string 2 cur_line))))
           (message "xplan/jump: traceback %s (%d)"
                    (find-file file)
                    (xplan/jump-line-number line))))

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
        ;; tpo.setFile('name', [...path...])
        ((string-match "\\_<\\(setFile\\)(['\"][[:word:]_]+['\"],\\W*\\[\\([^]]+\\)\\]" cur_line)
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
         (xplan/jump-url-handler (match-string 1 cur_line)
                                 (concat "req_" (match-string 2 cur_line) ".py")
                                 (match-string 3 cur_line)))
        ((string-match "/\\(iqm\\+/rr\\|supersolver\\)/\\([[:word:]_]+\\)" cur_line)
         (xplan/jump-url-handler (match-string 1 cur_line)
                                 "protocol.py"
                                 (match-string 2 cur_line)))

        ;; import class/function from module
        ;; import module from path
        ((string-match "from \\(xpt\\..+\\) import \\(.+\\)" cur_line)
         (let ((module (match-string 1 cur_line))
               (symbol (match-string 2 cur_line)))
           (let ((module2 (concat module "." symbol)))
             (if (xplan/is-file "src\\py\\" (xplan/get-file-from-module module2))
                 (message "xplan/jump: import --> %s"
                          (xplan/jump-file "src\\py\\" (xplan/get-file-from-module module2)))
               (message "xplan/jump: import --> %s :: %s"
                        (xplan/jump-file "src\\py\\" (xplan/get-file-from-module module))
                        (xplan/jump-method symbol "\\(def\\|class\\)"))))))

        ;; TODO: fall through to jump to tag, matching () etc.?

        (t
         (message "xplan/jump: match not found")))))

(defun xplan/jump-branch (branch)
  "Jump to the current file in the other branch

branch can be 'major.minor.patch', or just 'major.minor' or 'minor'."
  (interactive "sBranch: ")
  (let ((filename (buffer-file-name))
        (branch_bits (split-string branch "\\.")))
    (cond ((= (length branch_bits) 1)  ; just minor
           (setq branch (concat "2." branch ".999")))
          ((= (length branch_bits) 2)  ; major.minor
           (setq branch (concat branch ".999"))))
    (string-match "\\(.+\\)2.[0-9]+.999\\(.+\\)" filename)
    (find-file (concat (match-string 1 filename)
                       branch
                       (match-string 2 filename)))))

(defun xplan/non-trunk-file-p (file)
  "return t if file is in a version/2.x.999 branch that isn't trunk"
  (if (string-match ".+\\(2.[0-9]+.999\\).+" file)
      (not (string-equal (match-string 1 file) xplan/TRUNK))))

(defun xplan/kill-non-trunk-buffers ()
  "Kill all buffers that are not in the trunk branch.
Doesn't mess with non-xplan source buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      (when file
        (when (xplan/non-trunk-file-p file)
          (kill-buffer buffer))))))

;; html editing for xplan templates
(prelude-ensure-module-deps '(mmm-mode))

(require 'mmm-auto)
(mmm-add-group
 'html-xplan
 '((xplan-expr
    :submode python
    :face mmm-output-submode-face
    :front "<:"
    :include-front t
    :back ":>"
    :include-back t)))

(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-xplan)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-js)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-css)

(setq mmm-global-mode 'maybe)
(setq mmm-parse-when-idle t)
(setq mmm-submode-decoration-level 2)  ; high
;; don't need javascript background to be special
(set-face-attribute 'mmm-code-submode-face nil
                    :background nil)
;; light shading on embedded python
(set-face-attribute 'mmm-output-submode-face nil
                    :background "#EEEEFF")

