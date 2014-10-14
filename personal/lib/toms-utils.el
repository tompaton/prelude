;; remove extra blanks when joining lines
(defun tom/kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

;; delete trailing whitespace from current line only
(defun tom/delete-trailing-whitespace-current-line ()
  (interactive)
  (save-excursion
    (let ((line (bounds-of-thing-at-point 'line)))
      (delete-trailing-whitespace (car line) (cdr line)))))

(defun tom/mark-current-line ()
  "Mark (select) the current line. (e.g. for subsequent fill-paragraph)"
  (interactive)
  (let ((line (bounds-of-thing-at-point 'line)))
    (set-mark (car line))
    (goto-char (1- (cdr line)))))

;; toggle quotes
(defun tom/toggle-quotes ()
  "Toggle single quoted string to double or vice versa, and
  flip the internal quotes as well.  Best to run on the first
  character of the string."
  (interactive)
  (save-excursion
    (re-search-backward "[\"']")
    (let* ((start (point))
           (old-c (char-after start))
           new-c)
      (setq new-c 
            (case old-c
              (?\" "'")
              (?\' "\"")))
      (setq old-c (char-to-string old-c))
      (delete-char 1)
      (insert new-c)
      (re-search-forward old-c)
      (backward-char 1)
      (let ((end (point)))
        (delete-char 1)
        (insert new-c)
        (replace-string new-c old-c nil (1+ start) end)))))

;; toggle parens
(defun tom/toggle-parens ()
  "Toggle tuple/list parens."
  (interactive)
  (save-excursion
    (re-search-backward "[\[\(]")
    (let* ((start (point))
           (old-c1 (char-after start))
           old-c2 new-c1 new-c2)
      (setq old-c2
            (case old-c1
              (?\[ "]")
              (?\( ")")))
      (setq new-c1 
            (case old-c1
              (?\[ "(")
              (?\( "[")))
      (setq new-c2
            (case old-c1
              (?\[ ")")
              (?\( "]")))
      (setq old-c1 (char-to-string old-c1))
      (delete-char 1)
      (insert new-c1)
      (re-search-forward old-c2)
      (backward-char 1)
      (delete-char 1)
      (insert new-c2))))

;; split line on commas and indent (e.g. split python function params to separate lines)
(defun tom/split-line-on-comma-and-indent ()
  "Split the current line at each comma and reindent.
  Use for splitting python function parameters."
  (interactive)
  (save-excursion
    (let ((line (bounds-of-thing-at-point 'line)))
      (goto-char (car line))
      (while (search-forward "," (cdr line) t)
        (replace-match ",\n" nil t))
      (indent-region (car line) (cdr line)))))

;; find in all buffers
(defun tom/multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

;; Toggle window dedication
(defun tom/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; cycle buffer through list of major modes
(defun tom/cycle-modes (modes)
  (let ((next-mode (cadr (memq major-mode modes))))
    (unless next-mode
      (setq next-mode (car modes)))
    (funcall next-mode)))

;; save fingers, repeat M-. instead of M-*
(defun tom/find-tag ()
  "Call `find-tag' with current word first time and after that call
 find-tag with NEXT-P set to t."
  (interactive)
  (if (eq last-command 'tom/find-tag)
      (find-tag nil t)
    (find-tag (current-word) current-prefix-arg)))

(defun tom/replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))

(defun tom/yank-and-replace-smart-quotes ()
  "Paste and replace smart quotes"
  (interactive)
  (yank)
  (tom/replace-smart-quotes (mark) (point)))

(defun tom/join-lines ()
  "Join lines, remove trailing \ and leading comment if necessary"
  (interactive)
  (save-excursion
    ;; remove \ at the end of the current line
    (move-end-of-line 1)
    (backward-char)
    (if (looking-at "\\\\")
        (delete-char 1))
    ;; joining comments?
    (if ;;(eq (face-at-point) font-lock-comment-face)
        (er--point-is-in-comment-p) ; in expand-region-core.el
        (progn
         (next-line)
         ;; remove comment at start of next line)
         (prelude-move-beginning-of-line 1)
         (while (looking-at "#")
             (delete-char 1))
         (while (looking-at ";")
             (delete-char 1))
         (if (looking-at "//")
             (delete-char 2)))))
  ;; now join
  (join-line -1))

(defun tom/insert-missing-py-import ()
  "Get current word/region, find an import for that symbol in
  another buffer and copy it to the top of this buffer."
  (interactive)
  (let (symbol)
    (setq symbol
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    (save-excursion
      (let ((import (tom/find-py-import symbol (buffer-list))))
        (if import
          (progn
            ;; insert at top of current file (below other imports)
            (goto-char (point-max))
            (re-search-backward "^\\(import\\|from\\)")
            (forward-line 1)
            (insert import)
            (message "Inserted: %s" (s-trim-right import)))
          (message "Import not found for: %s" symbol)
          )))))

;; TODO: might be better to do this by finding all matches,
;; and selecting the most common one
(defun tom/find-py-import (symbol buffers)
  "recursively search buffers for a symbol import"
  (let ((buffer (car buffers)))
    (when buffer
      (let ((import (tom/extract-py-import symbol buffer)))
        (if import
            import ; found
          ;; check next buffer
          (tom/find-py-import symbol (cdr buffers)))))))

(defun tom/extract-py-import (symbol buffer)
  "get matching for .. import .. symbol line from buffer"
  (let ((file (buffer-file-name buffer)))
    (when file
      (when (string-match "\.py" file)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (when (ignore-errors
                    ;; look for ... import ... symbol
                    (re-search-forward (concat "^.*\\bimport\\b.\\b"
                                               symbol
                                               "\\b\\.**$")))
              (let ((line (bounds-of-thing-at-point 'line)))
                (buffer-substring-no-properties (car line) (cdr line))))
            ))))))

(defun tom/un-camelcase-word-at-point ()
  "un-camelcase the word at point, replacing uppercase chars with
the lowercase version preceded by an underscore.

The first char, if capitalized (eg, PascalCase) is just
downcased, no preceding underscore.

http://stackoverflow.com/a/15254151/3715
"
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))
