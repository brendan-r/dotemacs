;;; my-pandoc-clipboard.el --- Copy Org/Markdown via Pandoc -*- lexical-binding: t; -*-

;;; Commentary:
;; Convert the region or whole buffer without editing the source.
;; Requires the Pandoc CLI and a graphical Emacs clipboard backend.

;;; Code:

(require 'subr-x)

;; Copy markup through Pandoc: select text, convert it, then copy the result.
(defun my-pandoc--input (selected)
  "Return plain text from the region if SELECTED, otherwise the whole buffer."
  (if selected
      (buffer-substring-no-properties (region-beginning) (region-end))
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun my-pandoc--run (program input from to)
  "Run PROGRAM on INPUT from format FROM to TO using UTF-8 and long lines.
Return (STATUS OUTPUT DIAGNOSTICS), keeping stderr separate from output.
Clean up temporary resources even if the process cannot be started."
  (let ((stderr-file (make-temp-file "emacs-pandoc-"))
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (with-temp-buffer
          (insert input)
          (let ((status (call-process-region
                         (point-min) (point-max) program t
                         (list t stderr-file) nil
                         "--from" from "--to" to "--wrap=none")))
            (list status (buffer-string)
                  (with-temp-buffer
                    (insert-file-contents stderr-file)
                    (string-trim (buffer-string))))))
      (delete-file stderr-file))))

(defun my-pandoc--convert (input from to)
  "Convert INPUT from FROM to TO, or report a useful Pandoc error.
Return nonempty output.  Pandoc determines which syntax is accepted."
  (let* ((program (or (executable-find "pandoc")
                      (user-error "Pandoc not found; install it and add its directory to Emacs exec-path")))
         (direction (format "%s -> %s" from to))
         (response
          (condition-case err
              (my-pandoc--run program input from to)
            (error (user-error "Could not start Pandoc (%s): %s"
                               direction (error-message-string err)))))
         (status (nth 0 response))
         (output (nth 1 response))
         (diagnostics (nth 2 response)))
    (unless (and (integerp status) (zerop status))
      (user-error "Pandoc conversion failed (%s; status %s): %s"
                  direction status
                  (if (string-empty-p diagnostics)
                      "no diagnostic output" diagnostics)))
    (when (string-blank-p output)
      (user-error "Pandoc conversion (%s) produced empty output%s"
                  direction (if (string-empty-p diagnostics) ""
                              (concat ": " diagnostics))))
    output))

(defun my-pandoc--copy (text direction)
  "Copy TEXT to the clipboard, then the kill ring, reporting DIRECTION on error."
  (condition-case err
      (gui-set-selection 'CLIPBOARD text)
    (error (user-error "Conversion succeeded (%s), but clipboard copying failed: %s"
                       direction (error-message-string err))))
  ;; `kill-new' should not write to the clipboard a second time.
  (let ((interprogram-cut-function nil))
    (kill-new text)))

(defun my-pandoc-to-clipboard (from to)
  "Convert the active region or whole buffer from FROM to TO via Pandoc.
Copy UTF-8, unwrapped output to the clipboard and kill ring, preserving
source buffer state.  Signal `user-error' on empty input, missing Pandoc,
conversion failure, or unavailable clipboard access."
  (let* ((selected (and (use-region-p) (/= (region-beginning) (region-end))))
         (source (if selected "selected region" "buffer"))
         (input (my-pandoc--input selected))
         (direction (format "%s -> %s" from to)))
    (when (string-blank-p input)
      (user-error "Cannot convert %s: %s contains no non-whitespace text"
                  direction source))
    (unless (and (display-graphic-p) (fboundp 'gui-set-selection))
      (user-error "System clipboard unavailable; run this command in graphical Emacs"))
    (my-pandoc--copy (my-pandoc--convert input from to) direction)
    (message "Copied %s conversion of %s to clipboard and kill ring"
             direction source)))

(defun my-org-to-markdown-clipboard ()
  "Copy Org text as Pandoc Markdown to the system clipboard and kill ring.
Convert the active, nonempty region, or the entire buffer (ignoring
narrowing) when no text is selected.  Preserve the source buffer state.
Use UTF-8 and long lines without automatic paragraph wrapping.
Requires pandoc in Emacs `exec-path' and a graphical clipboard backend.
Report empty input, process/conversion failure, empty output, or clipboard
failure with `user-error'.  Syntax errors are reported only when Pandoc
rejects the input; this command does not perform strict validation."
  (interactive)
  (my-pandoc-to-clipboard "org" "markdown"))

(defun my-markdown-to-org-clipboard ()
  "Copy Pandoc Markdown as Org text to the system clipboard and kill ring.
Convert the active, nonempty region, or the entire buffer (ignoring
narrowing) when no text is selected.  Preserve the source buffer state.
Use UTF-8 and long lines without automatic paragraph wrapping.
Requires pandoc in Emacs `exec-path' and a graphical clipboard backend.
Report empty input, process/conversion failure, empty output, or clipboard
failure with `user-error'.  Syntax errors are reported only when Pandoc
rejects the input; this command does not perform strict validation."
  (interactive)
  (my-pandoc-to-clipboard "markdown" "org"))

(provide 'my-pandoc-clipboard)

;;; my-pandoc-clipboard.el ends here
