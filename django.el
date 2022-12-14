;;; django.el --- mode for editing Django templates -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benjamin Moody

;; Author: Benjamin Moody <bmoody@mit.edu>
;; Keywords: data, hypermedia, languages

;; This file is not part of Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides a minor mode for editing Django template files.  It's
;; a *minor* mode because it's meant to wrap around whatever major
;; mode you're using for the underlying data format.  For example, if
;; you're editing HTML templates then you will want to activate
;; `html-mode' or `mhtml-mode' and then activate `django-mode'.
;;
;; This mode provides several features that augment the behavior of
;; the existing major mode, and which work more or less independently:
;;
;;  - Fontification of template tokens (always enabled; works in any
;;    mode so long as `font-lock-add-keywords' is supported.)
;;
;;  - Syntactification of template tokens (optional; works by setting
;;    a custom `syntax-propertize-function'; should work in most modes
;;    but may interfere with parsing in complex modes.)
;;
;;  - Indentation based on template block tags (optional; works by
;;    setting a custom `indent-line-function'; likely to break in
;;    modes that have complicated contextual indentation rules.)
;;    Indentation will work best if Django block tags are placed on
;;    their own line.
;;
;; To enable this mode project-wide, you may want to put something
;; like this in your .dir-locals.el:
;;
;;    ((html-mode (eval . (django-mode 1))))

;;; Code:

(require 'subr-x)

(defgroup django nil
  "Minor mode for editing Django template files."
  :group 'languages)

(defcustom django-syntax-mode t
  "In Django mode, set syntax properties for Django tokens.

If set to non-nil, then variable `django-mode' will modify the
syntax properties of Django template tokens, so that they can be
interpreted correctly by standard editing commands such as
`forward-sexp'.  This may not work correctly with complex major
modes that implement their own parsing without using syntax
tables."
  :type 'boolean
  :safe 'booleanp)

(defcustom django-indent-mode t
  "In Django mode, indent template block structures.

If set to non-nil, then `django-mode' will configure indentation
commands to indent the contents of template block structures,
such as \"{% if %} ... {% endif %}\".

If set to 'django-only, then indentation is based only on
template tags, ignoring the major mode's normal indentation
behavior.

If set to 'always, then indentation is based on the template tag
structure in combination with the major mode's normal indentation
rules (if any).  This will work better for some major modes than
others.

If set to t or any other value, then template tags are used for
indentation only if the major mode defines a local
`indent-line-function' (which is typically the case for
programming languages and structured markup languages.)  For
plain text modes, template tags are ignored and indentation
commands behave as normal."
  :type '(choice (const :tag "Normal indentation for mode" nil)
                 (const :tag "Django tags only" django-only)
                 (const :tag "Hybrid in structured modes" t)
                 (const :tag "Hybrid in all modes" always))
  :safe 'symbolp)

;; Regexps for matching tokens

(defvar django--comment-regexp "\\({#\\).*?\\(#}\\)")

(defvar django--variable-regexp "\\({{\\).*?\\(}}\\)")

;; Django parses tags by searching for "{%.*?%}" and then applying
;; split() to the contents, so in theory a tag name can contain any
;; non-whitespace characters.  For simplicity, we assume here that tag
;; names can't contain percent signs.
(defvar django--tag-regexp "\\({%\\)[ \t]*\\([^ \t\n%]*\\).*?\\(%}\\)")

(defvar django--block-comment-tag-regexp
  (concat "{%[ \t]*\\(?:end\\)?comment\\(?:[ \t].*?\\)?%}"))

(defvar django--endcomment-tag-regexp
  (concat "\\({%\\)[ \t]*\\(endcomment\\)\\(?:[ \t].*?\\)?\\(%}\\)"))

;(defvar django--endverbatim-tag-regexp
;  (concat "\\({%\\)[ \t]*\\(endverbatim\\)\\(?:[ \t].*?\\)?\\(%}\\)"))

(defvar django--token-regexp
  (concat "\\(?:"
	  django--tag-regexp
	  "\\|"
	  django--variable-regexp
	  "\\|"
	  django--comment-regexp
	  "\\)"))

(defun django--match-tag-name ()
  (match-string-no-properties 2))

;; Font Lock

(defface django-comment-face
  '((t :inherit 'font-lock-comment-face))
  "Face for Django template comments")

(defface django-comment-delimiter-face
  '((t :inherit 'font-lock-comment-delimiter-face))
  "Face for {# and #} in Django template comments")

(defface django-tag-face
  '((((class color) (background light))
     :background "#a1e5cf" :distant-foreground "black")
    (((class color) (background dark))
     :background "#2a463d" :distant-foreground "gray85")
    (t :underline t))
  "Face for Django template tags")

(defface django-tag-name-face
  '((((class color) (background light)) :foreground "#2a463d")
    (((class color) (background dark)) :foreground "#a1e5cf"))
  "Face for names in Django template tags")

(defface django-tag-delimiter-face
  '((((class color) (background light)) :foreground "#5a8878")
    (((class color) (background dark)) :foreground "#73ad99"))
  "Face for {% and %} in Django template tags")

(defface django-variable-face
  '((((class color) (background light))
     :background "#ebccff" :distant-foreground "black")
    (((class color) (background dark))
     :background "#3f2d4d" :distant-foreground "gray85")
    (t :underline t))
  "Face for Django template variables")

(defface django-variable-delimiter-face
  '((((class color) (background light)) :foreground "#7d5a98")
    (((class color) (background dark)) :foreground "#9d71bf"))
  "Face for {{ and }} in Django template variables")

(defun django--search-forward-tag (limit)
  "Search for the next template tag in the buffer.
This is used as a font-lock matcher function (see
`font-lock-keywords').  LIMIT is the upper limit for the search."

  ;; Avoid highlighting tags inside {% comment %}: if comment type (7)
  ;; is "c" style (2), then skip to {% endcomment %}.
  ;;
  ;; FIXME: Should do something similar for verbatim but we can't use
  ;; syntax-ppss for that.
  (if (and django-syntax-mode
           (eq (nth 7 (syntax-ppss)) 2))
      (re-search-forward django--endcomment-tag-regexp limit t)
    (re-search-forward django--tag-regexp limit t)))

(defun django--search-forward-variable (limit)
  "Search for the next template variable in the buffer.
This is used as a font-lock matcher function (see
`font-lock-keywords').  LIMIT is the upper limit for the search."
  (and (or (not django-syntax-mode)
	   (not (eq (nth 7 (syntax-ppss)) 2))
	   (re-search-forward django--endcomment-tag-regexp limit t))
       (re-search-forward django--variable-regexp limit t)))

(defvar django--font-lock-keywords
  `((,django--comment-regexp
     (0 'django-comment-face t)
     (1 'django-comment-delimiter-face prepend)
     (2 'django-comment-delimiter-face prepend))

    (django--search-forward-tag
     (0 'django-tag-face prepend)
     (1 'django-tag-delimiter-face prepend)
     (2 'django-tag-name-face prepend)
     (3 'django-tag-delimiter-face prepend))

    (,django--block-comment-tag-regexp
     (0 'django-comment-face t))

    (django--search-forward-variable
     (0 'django-variable-face prepend)
     (1 'django-variable-delimiter-face prepend)
     (2 'django-variable-delimiter-face prepend))))

;; Indentation rules

(defcustom django-basic-offset 2
  "Default indentation offset for Django template block tags."
  :type 'integer
  :safe 'integerp)

(defcustom django-tag-offset-alist
  '(("comment" . 0)
    ("verbatim" . 0))
  "Indentation offsets for Django template block tags."
  :type '(alist :key-type string :value-type integer)
  :safe 'listp)

(defcustom django-default-block-tag-sequences
  '(("comment" "endcomment")
    ("verbatim" "endverbatim")
    ("block" "endblock")
    ("for" "empty" "endfor")
    ("with" "endwith")
    ("if" "elif" "else" "endif")
    ("ifchanged" "endifchanged")
    ("filter" "endfilter")
    ("autoescape" "endautoescape")
    ("spaceless" "endspaceless")
    ("blocktranslate" "plural" "endblocktranslate")
    ("localize" "endlocalize")
    ("localtime" "endlocaltime"))
  "Default lists of Django template tags that start and end blocks.

Each element of this list should be a list of two or more
strings, where the first element is an \"opening\" tag (like
\"if\"), the last element is a \"closing\" tag (like \"endif\"),
and other elements are tags that can appear in between (like
\"else\").

This variable is meant to be used for Django built-in tags; for
project-specific tags, you may want to set
`django-block-tag-sequences'."
  :type '(repeat (repeat string))
  :safe 'listp)

(defcustom django-block-tag-sequences '()
  "Lists of Django template tags that start and end blocks.

Each element of this list should be a list of two or more
strings, where the first element is an \"opening\" tag (like
\"if\"), the last element is a \"closing\" tag (like \"endif\"),
and other elements are tags that can appear in between (like
\"else\").

This variable is meant to be used for project-specific tags; for
Django built-in tags, you may want to set
`django-default-block-tag-sequences'."
  :type '(repeat (repeat string))
  :safe 'listp)

;; Syntax propertization

(defvar django--syntax-table
  (let ((table (make-syntax-table)))
    ;; use "c" comment style to avoid conflicts with most major modes
    (modify-syntax-entry ?{ "(}1c" table)
    (modify-syntax-entry ?} "){4c" table)
    (modify-syntax-entry ?# ". 23" table)
    ;; ' and " typically behave as string quotes inside django tokens,
    ;; but we don't want to interfere with quotes outside the token
    ;(modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry (cons ?$ ?&) "." table)
    (modify-syntax-entry (cons ?* ?/) "." table)
    (modify-syntax-entry (cons ?: ?@) "." table)
    (modify-syntax-entry ?| "." table)
    table))

(defvar django--block-comment-syntax-table
  (let ((table (make-syntax-table)))
    ;; these comments aren't really nestable, but mark them as such so
    ;; they don't conflict with {# #} comments
    (modify-syntax-entry ?{ "(}1cn" table)
    (modify-syntax-entry ?} "){4cn" table)
    (modify-syntax-entry ?% ". 23" table)
    table))

(defun django--syntax-unpropertize (start end)
  "Remove Django syntax properties from a region of text."
  (while (< start end)
    (let ((next (next-single-property-change start 'syntax-table nil end)))
      (when (memq (get-text-property start 'syntax-table)
		  (list django--syntax-table
                        django--block-comment-syntax-table))
	(remove-text-properties start next '(syntax-table nil)))
      (setq start next))))

(defvar django--transparent-properties '(local-map mhtml-submode))

(defun django--syntax-propertize (base-propertize-function start end)
  "Apply Django syntax properties to a region of text.

This function is used as local advice for
`syntax-propertize-function'.  BASE-PROPERTIZE-FUNCTION is the
original value of `syntax-propertize-function' defined by the
major mode; START and END are the boundaries of the region to be
propertized."
  (django--syntax-unpropertize start end)
  (save-restriction
    (setq end (min end (point-max)))
    (widen)
    (goto-char start)
    (forward-line 0)
    ;(if (and (< 1 (point)) (get-text-property (1- (point)) 'django-verbatim))
    ;    (add-text-properties start end '(django-verbatim t))
    ;  (remove-text-properties start end '(django-verbatim t)))
    (while (progn (skip-chars-forward "^{" end)
		  (< (point) end))
      (if (not (looking-at django--token-regexp))
          (forward-char 1)
	(let ((token-start (point))
	      (token-end (match-end 0))
	      (tag-name (django--match-tag-name)))

	  ;; Propertize up to TOKEN-START
	  (when (< start token-start)
	    (funcall base-propertize-function start token-start))

          ;; Copy transparent properties forward
          (dolist (prop django--transparent-properties)
            (if-let ((value (get-pos-property token-start prop)))
                (add-text-properties token-start token-end `(,prop ,value))
              (remove-text-properties token-start token-end `(,prop nil))))

	  ;; Propertize between TOKEN-START and TOKEN-END
          (add-text-properties token-start token-end
			       `(syntax-table ,django--syntax-table))
          (cond
           ;((equal tag-name "verbatim")
           ; (when (< token-end end)
           ;   (add-text-properties token-end end '(django-verbatim t))))
           ;((equal tag-name "endverbatim")
           ; (when (< token-start end)
           ;   (remove-text-properties token-start end '(django-verbatim t))))
           ((equal tag-name "comment")
	    (add-text-properties
	     token-start (+ token-start 2)
             `(syntax-table ,django--block-comment-syntax-table)))
           ((equal tag-name "endcomment")
	    (add-text-properties
	     (- token-end 2) token-end
             `(syntax-table ,django--block-comment-syntax-table))))

	  ;; Jump to TOKEN-END and keep going
	  (goto-char token-end)
	  (setq start (max start token-end))))))

  ;; Text is now propertized up to START and no django tokens between
  ;; START and END.  Propertize the remainder of text between START
  ;; and END.
  (when (< start end)
    (funcall base-propertize-function start end)))

;; Indentation

(defun django--block-context ()
  (save-excursion
    (let ((sequences (append django-default-block-tag-sequences
		             django-block-tag-sequences))
	  (count 1) openers closers raw-block tag tag-start tag-end)

      (dolist (sequence sequences)
        (push (car sequence) openers)
        (push (car (last sequence)) closers))

      (while (and (> count 0)
                  ;; XXX this can be fooled by things that look like
                  ;; tokens inside of other tokens
                  (re-search-backward django--tag-regexp nil t))
        (setq tag-start (match-beginning 0)
	      tag-end (match-end 0)
	      tag (django--match-tag-name))
        (cond
         (raw-block (if (equal tag raw-block)
                        (setq raw-block nil
                              count (1- count))))
         ((equal tag "endcomment") (setq raw-block "comment"
                                         count (1+ count)))
         ((equal tag "endverbatim") (setq raw-block "verbatim"
                                          count (1+ count)))
         ((member tag closers) (setq count (1+ count)))
         ((member tag openers) (setq count (1- count)))))

      (when (= count 0)
	(list tag
	      tag-start
	      tag-end
	      (car-safe (last (assoc tag sequences))))))))

(defun django--indent-offset-table ()
  "Build a hash table of indentation offsets for Django tags.

Given the variables `django-default-block-tag-sequences',
`django-block-tag-sequences', `django-basic-offset', and
`django-tag-offset-alist', convert indentation rules into a
hash table where each key is a tag name and each value is a cons
cell (OFFSET-BEFORE . OFFSET-AFTER).

For example, if `django-basic-offset' is 2, then the \"if\"
tag has the value (0 . 2), indicating that the line before the
\"if\" is indented by 0 spaces and the line after the \"if\" is
indented by 2 spaces.  \"elif\" and \"else\" both have the
value (2 . 2), and \"endif\" has the value (2 . 0)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (sequences (list django-default-block-tag-sequences
			     django-block-tag-sequences))
      (dolist (sequence sequences)
	(when (<= 2 (length sequence))
          (let ((offset (or (assoc-default (car sequence)
                                           django-tag-offset-alist)
                            django-basic-offset)))
            (puthash (pop sequence) (cons 0 offset) table)
            (while (cdr sequence)
              (puthash (pop sequence) (cons offset offset) table))
            (puthash (car sequence) (cons offset 0) table)))))
    table))

(defun django--line-indent-offsets (table)
  "Determine the indentation offsets for the line at point.

TABLE is a hash table of offsets produced by
`django--indent-offset-table'.  Return a cons
cell (OFFSET-BEFORE . OFFSET-AFTER), where OFFSET-BEFORE is the
preferred indentation offset for the previous line relative to
this one, and OFFSET-AFTER is the preferred indentation offset
for the next line relative to this one."
  (let ((delta-initial 0)
	(delta-overall 0)
	(initial t))
    (while (progn (skip-chars-forward " \t")
		  (not (eolp)))
      (cond
       ((looking-at django--tag-regexp)
	(goto-char (match-end 0))
	(when-let ((offsets (gethash (django--match-tag-name) table)))
	  (setq delta-overall (- delta-overall (car offsets)))
	  (when initial
	    (setq delta-initial (min delta-initial delta-overall)))
	  (setq delta-overall (+ delta-overall (cdr offsets)))))
       ((looking-at django--comment-regexp)
	(goto-char (match-end 0)))
       ((looking-at django--variable-regexp)
	(setq initial nil)
	(goto-char (match-end 0)))
       (t (setq initial nil)
          (forward-char 1))))
    (cons (- delta-initial)
	  (- delta-overall delta-initial))))

(defun django--indent-line (&optional base-indent-function)
  "Indent a line in Django mode.

This function is used as local advice for `indent-line-function'.
BASE-INDENT-FUNCTION is the original value of
`indent-line-function' defined by the major mode, or nil if no
indentation function is defined."
  ;; If no non-blank lines before this one in the buffer, then indent
  ;; normally.
  (if (save-excursion (forward-line 0)
                      (skip-chars-backward " \t\n\r\f")
                      (bobp))
      (when base-indent-function
	(funcall base-indent-function))

    (let ((table (django--indent-offset-table))
	  prev-line-offsets
	  prev-line-column
	  cur-line-offsets
	  closer-pos
	  (min-column 0) max-column column)
      (save-excursion
	(forward-line 0)
	(setq cur-line-offsets (django--line-indent-offsets table))
	(forward-line 0)
        (skip-chars-backward " \t\n\r\f")
	(back-to-indentation)
	(setq prev-line-column (current-column)
	      prev-line-offsets (django--line-indent-offsets table)))

      (when (/= 0 (car cur-line-offsets))
        ;; current line begins with closer(s)
	(save-excursion
	  (back-to-indentation)
	  (setq closer-pos (point))
	  (while (or (and (looking-at django--tag-regexp)
                          (pcase (gethash (django--match-tag-name) table)
                            (`(0 . ,_) nil)
                            (`(,_ . ,_) (setq closer-pos (point)))))
		     (looking-at django--comment-regexp))
	    (goto-char (match-end 0))
	    (skip-chars-forward " \t"))
	  (goto-char closer-pos)
	  (if-let ((context (django--block-context)))
	      ;; align with the opener
	      (setq column (progn (goto-char (nth 1 context))
				  (current-column)))
	    ;; no opener, go to column 0
	    (setq column 0))))

      (when (< 0 (cdr prev-line-offsets))
	;; previous line contains opener(s)
	(setq min-column (+ prev-line-column (cdr prev-line-offsets))))

      (when (/= (car prev-line-offsets) (cdr prev-line-offsets))
	;; previous line contains closer(s)
	(setq max-column prev-line-column))

      (unless column
        (setq column (if base-indent-function
	                 (save-excursion
	                   (funcall base-indent-function)
	                   (back-to-indentation)
                           (current-column))
                       prev-line-column)
	      column (min column (or max-column column))
	      column (max column min-column 0)))

      (if (<= (point) (save-excursion (back-to-indentation) (point)))
	  (indent-line-to (max 0 column))
	(save-excursion (indent-line-to (max 0 column)))))))

;; Miscellaneous

(defun django-close-block ()
  "Close the current Django block element.
For example, following \"{% if foo %}\", insert \"{% endif %}\"."
  (interactive "*")

  ;; If we are currently inside a template token then move past it
  (let ((pos (point)))
    (forward-line 0)
    (while (< (point) pos)
      (cond
       ((looking-at django--token-regexp) (goto-char (match-end 0)))
       ((= (skip-chars-forward "^{" pos) 0) (forward-char 1)))))

  (let ((context (django--block-context)))
    (if context
        (insert "{% " (nth 3 context) " %}")
      (error "No opening tag found"))))

;; Minor mode

(defvar django-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c]" 'django-close-block)
    map))

(defun django--hack-local-variable (variable)
  (unless (local-variable-p variable)
    (let ((x (assq variable file-local-variables-alist)))
      (when x
	(set (make-local-variable variable) (cdr x))))))

;;;###autoload
(define-minor-mode django-mode
  "Parse and highlight Django template syntax."
  nil "/Dj" django-mode-map
  (cond
   (django-mode                      ; turning django mode on
    (font-lock-add-keywords nil django--font-lock-keywords)

    (django--hack-local-variable 'django-indent-mode)
    (remove-function (local 'indent-line-function)
		     #'django--indent-line)
    (when django-indent-mode
      (cond
       ((eq django-indent-mode 'django-only)
	(add-function :override (local 'indent-line-function)
		      #'django--indent-line))
       ((not (eq indent-line-function (default-value 'indent-line-function)))
	(add-function :around (local 'indent-line-function)
		      #'django--indent-line))
       ((eq django-indent-mode 'always)
	(add-function :override (local 'indent-line-function)
		      #'django--indent-line))))

    (django--hack-local-variable 'django-syntax-mode)
    (remove-function (local 'syntax-propertize-function)
		     #'django--syntax-propertize)
    (when django-syntax-mode
      (setq-local parse-sexp-lookup-properties t)
      (setq-local font-lock-keywords-only nil)
      (unless syntax-propertize-function
	(setq-local syntax-propertize-function #'ignore))
      (add-function :around (local 'syntax-propertize-function)
		    #'django--syntax-propertize)
      (syntax-ppss-flush-cache (point-min))))

   (t                                   ; turning django mode off
    (font-lock-remove-keywords nil django--font-lock-keywords)
    (remove-function (local 'indent-line-function)
		     #'django--indent-line)

    (when (advice-function-member-p #'django--syntax-propertize
                                    syntax-propertize-function)
      (remove-function (local 'syntax-propertize-function)
		       #'django--syntax-propertize)
      (with-silent-modifications
        (django--syntax-unpropertize (point-min) (point-max)))
      (syntax-ppss-flush-cache (point-min)))))

  (font-lock-flush))

(provide 'django)

;;; django.el ends here
