;; remove extra blanks when joining lines
(defun tom/kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

;; toggle between start of line and start of indentation
(defun tom/back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

;; delete trailing whitespace from current line only
(defun tom/delete-trailing-whitespace-current-line ()
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (let ((end (point)))
      (forward-line 0)
      (let ((start (point)))
        (delete-trailing-whitespace start end)))))

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
    (move-end-of-line nil)
    (let ((end (point)))
      (forward-line 0)
      (let ((start (point)))
        (while (search-forward "," end t)
          (replace-match ",\n" nil t))
        (indent-region start end)))))

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
