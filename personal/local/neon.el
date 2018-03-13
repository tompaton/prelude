;; start with 3 panes for QHD monitor
(split-window-horizontally)
(split-window-horizontally)
(balance-windows)

;; alternate python mode
;; (add-to-list 'load-path "~/.emacs.d/personal/lib/python.el/")
;; (require 'python)

;; django mode
;; (add-to-list 'load-path "~/.emacs.d/personal/lib/python-django.el/")
;; (require 'python-django)

;; (global-set-key (kbd "C-x j") 'python-django-open-project)

(prelude-ensure-module-deps '(pony-mode
                              mmm-mode
                              go-mode))

(require 'pony-mode)

(require 'mmm-auto)

(mmm-add-group
 'html-django
 '(
   ;; custom map tag in gps2 project contains javascript
   (map-tag
    :submode js-mode
    :face mmm-code-submode-face
    :front "{% map .+ %}"
    :back "{% endmap %}")
   
   ;; normal django tags and expressions
   (django-expr
    :submode python-mode
    :face mmm-declaration-submode-face
    :front "{%"
    :back "%}"
    :include-front t
    :include-back t)
   (django-var
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

;; tags for gps2
(setq tags-table-list
      '("/home/tom/dev/django_projects/gps2-trunk/TAGS"))

(defun recode-buffer-for-cyrillic ()
  "convert russian translated file from windows-1251-dos to iso-latin-1-unix"
  (interactive)
  (recode-region (point-min) (point-max) 'windows-1251-dos 'iso-latin-1-unix))

;; these choke a bit on gps2 models.py
(global-color-identifiers-mode -1)
(global-company-mode -1)

(custom-set-variables
 '(prelude-flyspell nil))

(flycheck-define-checker javascript-flow
  "A JavaScript syntax and style checker using Flow.

See URL `http://flowtype.org/'."
  :command ("/home/tom/dev/flow/flow" source-original)
  :error-patterns
  ((error line-start
          (file-name)
          ":"
          line
          ":"
          (minimal-match (one-or-more not-newline))
          ": "
          (message (minimal-match (and (one-or-more anything) "\n")))
          line-end))
  :modes js-mode)

(flycheck-add-next-checker 'javascript-gjslint 'javascript-flow)

(require 'repdet)
(global-set-key [f11] 'repdet-use-as-macro)
(global-set-key [f12] 'call-last-kbd-macro)


(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(let* ((base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline))))
                          `(org-level-7 ((t (,@headline))))
                          `(org-level-6 ((t (,@headline))))
                          `(org-level-5 ((t (,@headline))))
                          `(org-level-4 ((t (,@headline :height 1.1))))
                          `(org-level-3 ((t (,@headline :height 1.25))))
                          `(org-level-2 ((t (,@headline :height 1.5))))
                          `(org-level-1 ((t (,@headline :height 1.75))))
                          `(org-document-title ((t (,@headline :height 1.5 :underline nil))))))
