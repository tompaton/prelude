;; start with 2 panes for widescreen monitor
(split-window-horizontally)

;; alternate python mode
;; (add-to-list 'load-path "~/.emacs.d/personal/lib/python.el/")
;; (require 'python)

;; django mode
;; (add-to-list 'load-path "~/.emacs.d/personal/lib/python-django.el/")
;; (require 'python-django)

;; (global-set-key (kbd "C-x j") 'python-django-open-project)

(prelude-ensure-module-deps '(pony-mode
                              mmm-mode))

(require 'pony-mode)

(require 'mmm-auto)

(mmm-add-group
 'html-django
 '((django-expr
    :submode python-mode
    :face mmm-declaration-submode-face
    :front "{%"
    :back "%}"
    :include-front t
    :include-back t)
   (my-django-var
    :submode python
    :face mmm-output-submode-face
    :front "{{"
    :back "}}"
    :include-front t
    :include-back t)))

(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-django)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-js)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-css)

(setq mmm-global-mode 'maybe)
(setq mmm-parse-when-idle t)
(setq mmm-submode-decoration-level 2)

(set-face-attribute 'mmm-default-submode-face nil :background "#EEEEFF")
(set-face-attribute 'mmm-code-submode-face nil :background nil)
(set-face-attribute 'mmm-output-submode-face nil :background"#EEEEFF")
(set-face-attribute 'mmm-declaration-submode-face nil :background"#EEFFFF")

;; auto-close django template tags
(defvar django-closable-tags
  '("for" "block" "comment" "filter" "ifchanged" "ifequal"
    "ifnotequal" "spaceless" "if"))

(defvar django-tag-re
  (concat "{%\\s *\\(end\\)?\\("
          (mapconcat 'identity django-closable-tags "\\|")
          "\\)[^%]*%}"))

(defun django-find-open-tag ()
  (if (search-backward-regexp django-tag-re nil t)
      (if (match-string 1) ; If it's an end tag
          (if (not (string= (match-string 2) (django-find-open-tag)))
              (error "Unmatched Django tag")
            (django-find-open-tag))
        (match-string 2)) ; Otherwise, return the match
    nil))

(defun django-close-tag ()
  (interactive)
  (let ((open-tag (save-excursion (django-find-open-tag))))
    (if open-tag
        (insert "{% end" open-tag " %}")
      (error "Nothing to close"))))

;; C-c ? is sgml-tag-help, change so it's like C-c / (sgml-close-tag)
(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map
       "\C-c?" 'django-close-tag)))
