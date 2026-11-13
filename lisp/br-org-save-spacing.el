;;; br-org-save-spacing.el --- Bound Org save formatting cost -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)

(defvar br-org-save-spacing-max-lines nil
  "Skip formatting at this many lines; nil disables the limit.")

(defun br-org-save-spacing--too-large-p ()
  "Return non-nil if the widened buffer meets the line limit."
  (and br-org-save-spacing-max-lines
       (save-excursion
         (goto-char (point-min))
         (or (<= br-org-save-spacing-max-lines 0)
             (and (= (forward-line (1- br-org-save-spacing-max-lines)) 0)
                  (not (eobp)))))))

(defun br-org-save-spacing-format (&optional ignore-limit)
  "Normalize Org spacing unless the buffer meets the configured line limit.
Leave planning lines and property drawers attached to their headings.
Adjacent headings use the two-blank-line rule before headings.
When IGNORE-LIMIT is non-nil, bypass the automatic save line limit.
Interactive calls always bypass the limit."
  (interactive (list t))
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-match-data
        (save-restriction
          (widen)
          (unless (and (not ignore-limit)
                       (br-org-save-spacing--too-large-p))
            (let ((tree (org-element-parse-buffer)) edits)
              (cl-labels ((gap (a b text)
                            (unless (equal (buffer-substring-no-properties a b) text)
                              (push (list a b text) edits))))
                (org-element-map tree 'headline
                  (lambda (headline)
                    (let ((begin (org-element-property :begin headline)))
                      (goto-char begin)
                      (unless (= begin (point-min))
                        (while (and (> (point) (point-min))
                                    (progn (forward-line -1)
                                           (looking-at-p "[ \t]*$"))))
                        (unless (looking-at-p "[ \t]*$") (forward-line 1))
                        (gap (point) begin "\n\n"))
                      (goto-char begin)
                      (forward-line 1)
                      (let ((start (point)))
                        (while (and (not (eobp)) (looking-at-p "[ \t]*$"))
                          (forward-line 1))
                        (let* ((end (point))
                               (section (car (org-element-contents headline)))
                               (first (and (eq (org-element-type section) 'section)
                                           (car (org-element-contents section)))))
                          (when (and first (= end (org-element-property :begin first))
                                     (not (memq (org-element-type first)
                                                '(planning property-drawer))))
                            (gap start end "\n")))))))
                (org-element-map tree 'section
                  (lambda (section)
                    (let ((children (org-element-contents section)))
                      (while (cdr children)
                        (let ((a (car children)) (b (cadr children)))
                          (when (and (memq (org-element-type a) '(paragraph plain-list))
                                     (eq (org-element-type b) 'paragraph))
                            (let* ((end (org-element-property :end a))
                                   (start (save-excursion
                                            (goto-char end)
                                            (forward-line (- (org-element-property :post-blank a)))
                                            (point))))
                              (gap start (org-element-property :begin b) "\n"))))
                        (setq children (cdr children))))))
                (atomic-change-group
                  (dolist (edit (sort edits (lambda (a b) (> (car a) (car b)))))
                    (goto-char (nth 0 edit))
                    (delete-region (nth 0 edit) (nth 1 edit))
                    (insert (nth 2 edit))))))))))))

(defun br-org-save-spacing-enable ()
  "Enable bounded formatting when saving this Org buffer."
  (add-hook 'before-save-hook #'br-org-save-spacing-format nil t))
(add-hook 'org-mode-hook #'br-org-save-spacing-enable)

(provide 'br-org-save-spacing)
;;; br-org-save-spacing.el ends here
