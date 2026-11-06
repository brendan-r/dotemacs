;;; br-org-math.el --- Convert Org math delimiters -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'cl-lib)

(defun br/org-math--safe-dollar-inline-p (content begin end)
  "Return non-nil when CONTENT can safely use $...$ at BEGIN..END.
BEGIN and END delimit the original Org LaTeX fragment, not its contents."
  (and (not (equal content ""))
       ;; Org disallows whitespace directly inside a single-dollar pair.
       ;; Do not trim: trailing `\\ ' is a meaningful TeX command.
       (not (string-match-p "\\`[[:space:]]\\|[[:space:]]\\'" content))
       (<= (cl-count ?\n content) 2)
       ;; Org explicitly excludes a dash after a closing single dollar.
       (not (eq (char-after end) ?-))
       (let ((candidate (concat "$" content "$"))
             (before (char-before begin))
             (after (char-after end)))
         ;; Ask Org's own fragment parser to check both delimiters and
         ;; their immediate context (currency, punctuation, adjacency).
         (with-temp-buffer
           (set-syntax-table org-mode-syntax-table)
           (when before (insert before))
           (let ((start (point)))
             (insert candidate)
             (when after (insert after))
             (goto-char start)
             (let ((parsed (org-element-latex-fragment-parser)))
               (and parsed
                    (= (- (org-element-property :end parsed)
                          (org-element-property :post-blank parsed))
                       (+ start (length candidate))))))))))

(defun br/org-math-convert (style begin end)
  "Convert complete Org math fragments within BEGIN..END to STYLE.
STYLE is `dollars' or `brackets'.  Return (CONVERTED . SKIPPED).
Leave fragments crossing region boundaries and literal Org areas alone."
  (unless (derived-mode-p 'org-mode) (user-error "This command requires Org mode"))
  (unless (memq style '(dollars brackets)) (error "Unknown math style"))
  (let (edits (skipped 0))
    ;; Org splits a display around an embedded LaTeX environment into separate
    ;; elements.  Recognize this structure and report it without changing it.
    (when (eq style 'dollars)
      (org-element-map (org-element-parse-buffer) 'latex-environment
        (lambda (environment)
          (save-excursion
            (let* ((start (org-element-property :post-affiliated environment))
                   (finish (+ start (length (org-element-property :value environment))))
                   (open (progn (goto-char start)
                                (skip-chars-backward " \t\n\r")
                                (when (and (>= (- (point) 2) (point-min))
                                           (equal (buffer-substring-no-properties (- (point) 2) (point)) "\\["))
                                  (- (point) 2))))
                   (close (progn (goto-char finish)
                                 (skip-chars-forward " \t\n\r")
                                 (when (looking-at-p (regexp-quote "\\]")) (+ (point) 2)))))
              (when (and open close (>= open begin) (<= close end))
                (cl-incf skipped)))))))
    (org-element-map (org-element-parse-buffer) 'latex-fragment
      (lambda (fragment)
        (let* ((raw (org-element-property :value fragment))
               (start (org-element-property :begin fragment))
               (finish (+ start (length raw)))
               (opening (cond ((string-prefix-p "\\[" raw) 'bracket-display)
                              ((string-prefix-p "\\(" raw) 'bracket-inline)
                              ((string-prefix-p "$$" raw) 'dollar-display)
                              ((string-prefix-p "$" raw) 'dollar-inline)))
               (target (if (eq style 'dollars)
                           (pcase opening ('bracket-display '("$$" "$$" 2))
                             ('bracket-inline '("$" "$" 2)))
                         (pcase opening ('dollar-display '("\\[" "\\]" 2))
                           ('dollar-inline '("\\(" "\\)" 1))))))
          (when (and target (>= start begin) (<= finish end))
            (let* ((n (nth 2 target)) (content (substring raw n (- n)))
                   (replacement (concat (nth 0 target) content (nth 1 target)))
                   ;; A closing delimiter inside the body changes the parse.
                   (unsafe (or (and (eq opening 'bracket-display)
                                                (string-match-p (regexp-quote "\\begin{") content))
                               (string-match-p (regexp-quote (nth 1 target)) content)
                               (and (eq opening 'bracket-inline)
                                    (not (br/org-math--safe-dollar-inline-p content start finish))))))
              (if unsafe (cl-incf skipped)
                (push (list start finish replacement) edits)))))))
    (atomic-change-group
      (save-excursion
        (dolist (edit edits)
          (goto-char (nth 0 edit))
          (delete-region (nth 0 edit) (nth 1 edit))
          (insert (nth 2 edit)))))
    (cons (length edits) skipped)))

(defun br/org-convert-math-delimiters (style)
  "Convert Org math delimiters to STYLE in the region or whole buffer.
Ask before converting the entire buffer when no region is active.
Only fully selected fragments are changed; unsafe conversions are skipped."
  (interactive
   (list (intern (completing-read "Convert math to: " '("dollars" "brackets") nil t))))
  (unless (derived-mode-p 'org-mode) (user-error "This command requires Org mode"))
  (let* ((region (use-region-p))
         (begin (and region (region-beginning)))
         (end (and region (region-end))))
    (unless (or region (y-or-n-p "Convert math in the entire buffer? "))
      (user-error "Conversion cancelled"))
    (save-restriction
      (unless region (widen))
      (let* ((result (br/org-math-convert style (or begin (point-min)) (or end (point-max))))
             (converted (car result))
             (skipped (cdr result))
             (total (+ converted skipped)))
        ;; `message' displays the result and records it in *Messages*.
        (if (zerop total)
            (message "No math fragments needed conversion")
          (message "Converted %d of %d math fragments; %d skipped to preserve syntax"
                   converted total skipped))))))

(provide 'br-org-math)
;;; br-org-math.el ends here
