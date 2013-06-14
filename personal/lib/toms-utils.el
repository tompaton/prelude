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
