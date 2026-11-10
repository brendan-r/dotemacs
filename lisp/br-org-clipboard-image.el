;;; br-org-clipboard-image.el --- Paste clipboard images in Org -*- lexical-binding: t; -*-

(require 'org)
(require 'cua-base)
(require 'subr-x)

(defconst br/org-clipboard-image--script
  "ObjC.import('AppKit');
function run(args) {
  var pb = $.NSPasteboard.generalPasteboard;
  var data = pb.dataForType($.NSPasteboardTypePNG);
  if (data.isNil()) {
    var tiff = pb.dataForType($.NSPasteboardTypeTIFF);
    if (tiff.isNil()) return 'no-image';
    var image = $.NSBitmapImageRep.imageRepWithData(tiff);
    if (image.isNil()) throw new Error('Cannot decode clipboard image');
    data = image.representationUsingTypeProperties($.NSBitmapImageFileTypePNG, $({}));
  }
  if (data.isNil() || !data.writeToFileAtomically($(args[0]), true))
    throw new Error('Cannot save clipboard image as PNG');
  return 'image';
}"
  "JXA script that copies a macOS clipboard image as PNG, without changing it.")

(defun br/org-clipboard-image--capture-macos (file)
  "Save the macOS clipboard image to FILE; return non-nil if present.
Signal an error if reading or saving fails."
  (with-temp-buffer
    (let ((status (call-process "/usr/bin/osascript" nil t nil
                                "-l" "JavaScript" "-e"
                                br/org-clipboard-image--script "--" file)))
      (unless (and (integerp status) (zerop status))
        (error "Could not read clipboard image: %s" (string-trim (buffer-string))))
      (pcase (string-trim (buffer-string))
        ("image" t)
        ("no-image" nil)
        (_ (error "Unexpected clipboard image response: %s" (buffer-string)))))))

(defun br/org-clipboard-image--capture-x11 (file)
  "Save a PNG from the X11 clipboard to FILE; return non-nil if present.
Use xclip when available.  Leave ordinary text paste alone when xclip
is missing or the clipboard does not offer image/png."
  (when-let ((xclip (executable-find "xclip")))
    (with-temp-buffer
      (let ((coding-system-for-read 'no-conversion)
            (coding-system-for-write 'no-conversion))
        (set-buffer-multibyte nil)
        (when (and (eq 0 (call-process xclip nil (list t nil) nil
                                        "-selection" "clipboard" "-o" "-t" "TARGETS"))
                   (member "image/png" (split-string (buffer-string) "[\r\n]+" t)))
          (erase-buffer)
          (unless (eq 0 (call-process xclip nil (list t nil) nil
                                       "-selection" "clipboard" "-o" "-t" "image/png"))
            (error "Could not read PNG image from the X11 clipboard"))
          (unless (and (>= (buffer-size) 8)
                       (equal (buffer-substring-no-properties 1 9)
                              (unibyte-string 137 80 78 71 13 10 26 10)))
            (error "X11 clipboard did not return a valid PNG"))
          (write-region (point-min) (point-max) file nil 'silent)
          t)))))

(defun br/org-clipboard-image--capture (file)
  "Save a clipboard image to FILE; return non-nil if one was available."
  (cond ((eq system-type 'darwin) (br/org-clipboard-image--capture-macos file))
        ((and (eq system-type 'gnu/linux) (eq (window-system) 'x))
         (br/org-clipboard-image--capture-x11 file))))

(defun br/org-clipboard-image--available-name (file)
  "Return FILE or the first available name with an _N suffix."
  (let ((base (file-name-sans-extension file))
        (ext (file-name-extension file t))
        (candidate file)
        (n 0))
    (while (file-exists-p candidate)
      (setq n (1+ n)
            candidate (format "%s_%d%s" base n (or ext ""))))
    candidate))

(defun br/org-clipboard-image--insert (temp-file)
  "Prompt for a path, save TEMP-FILE there, and insert its Org image link."
  (barf-if-buffer-read-only)
  (let* ((base (if buffer-file-name
                   (file-name-directory buffer-file-name)
                 default-directory))
         (default (br/org-clipboard-image--available-name
                   (expand-file-name "media/clipboard.png" base)))
         (file (expand-file-name
                (read-file-name "Save clipboard image: "
                                (file-name-directory default) default nil
                                (file-name-nondirectory default)))))
    (unless (string-equal (downcase (or (file-name-extension file) "")) "png")
      (user-error "Clipboard image destination must end in .png"))
    (when (file-remote-p file)
      (user-error "Clipboard image destination must be local"))
    (setq file (br/org-clipboard-image--available-name file))
    (make-directory (file-name-directory file) t)
    ;; `copy-file' without OK-IF-ALREADY-EXISTS never overwrites a file,
    ;; including one created after the availability check.
    (copy-file temp-file file nil)
    (let ((start (point)))
      (insert (concat "[[file:" (org-link-escape (file-relative-name file base)) "]]"))
      (org-display-inline-images nil nil start (point)))
    (message "Saved clipboard image to %s" file)))

(defun br/org-clipboard-image--paste (original &rest args)
  "Paste an image in Org, or call ORIGINAL with ARGS for ordinary pastes."
  (if (and (derived-mode-p 'org-mode) (display-graphic-p)
           (or (eq system-type 'darwin)
               (and (eq system-type 'gnu/linux) (eq (window-system) 'x))))
      (let ((temp-file (make-temp-file "org-clipboard-" nil ".png")))
        (unwind-protect
            (if (br/org-clipboard-image--capture temp-file)
                (br/org-clipboard-image--insert temp-file)
              (apply original args))
          (delete-file temp-file)))
    (apply original args)))

(advice-add 'cua-paste :around #'br/org-clipboard-image--paste)

(provide 'br-org-clipboard-image)
;;; br-org-clipboard-image.el ends here
