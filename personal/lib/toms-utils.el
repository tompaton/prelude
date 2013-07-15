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

;; custom powerline theme
(require 'powerline)
(defun tom/powerline-theme ()
  "Setup the default mode-line, based on powerline-default-theme.
   - emphasize buffer id, which-func-mode, major and line/col."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-minor-modes face2 'l)
                                     (powerline-narrow face2 'l)
                                     (powerline-raw " " face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-buffer-size face1 'l)
                                     (powerline-raw " " face1)
                                     (powerline-raw "%6p" face1 'r)
                                     (powerline-raw mode-line-mule-info face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw "%4l" mode-line 'l)
                                     (powerline-raw ":" mode-line 'l)
                                     (powerline-raw "%3c" mode-line 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))
