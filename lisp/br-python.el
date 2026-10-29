;;; br-python.el --- Project selection for the built-in Python REPL -*- lexical-binding: t; -*-

;; Execution, expression display, multiline input and tracebacks belong to
;; python.el.  This file selects project interpreters/processes, source units
;; and Quarto chunks, and configures their language clients.  See docs/python-repl-research.org for the upstream comparison.
(require 'python)
(require 'project)
(require 'eglot)
(require 'cl-lib)
(require 'subr-x)

(defvar-local br/python-executable nil
  "Python executable override, absolute or relative to the project root.
Set under nil in .dir-locals.el to apply to Python and Quarto alike.")
(defvar-local br/python--process nil)
(defvar-local br/python--source nil)
(defvar-local br/python--ready nil)
(defvar polymode-eval-region-function)
(declare-function pm-innermost-span "polymode-core")
(declare-function pm-span-buffer "polymode-core")
(declare-function flycheck-mode "flycheck")
(declare-function pm-map-over-spans "polymode-core")
(declare-function lsp-deferred "lsp-mode")
(declare-function lsp-rename "lsp-mode")

;; Display output without taking keyboard focus, including frames-only-mode.
;; Explicit switching still uses python-shell-switch-to-shell.
(add-to-list 'display-buffer-alist
             '((lambda (buffer _action)
                 (and (display-graphic-p)
                      (with-current-buffer buffer
                        (derived-mode-p 'inferior-python-mode))))
               (display-buffer-reuse-window display-buffer-pop-up-frame)
               (reusable-frames . t)
               (inhibit-same-window . t)
               (inhibit-switch-frame . t)
               (pop-up-frame-parameters . ((no-focus-on-map . t)))))

(defun br/python-project-find (dir)
  "Recognize non-VC Python projects after the normal project finders."
  (when-let* ((root (locate-dominating-file
                    dir (lambda (d)
                          (cl-some (lambda (f) (file-exists-p (expand-file-name f d)))
                                   '("pyproject.toml" "pyrightconfig.json" ".venv"))))))
    (cons 'br-python root)))
(cl-defmethod project-root ((project (head br-python))) (cdr project))
(add-hook 'project-find-functions #'br/python-project-find t)

(defun br/python-root ()
  "Use the project root, otherwise the source file's directory."
  (when (file-remote-p default-directory)
    (user-error "This Python process picker supports local projects only"))
  (file-name-as-directory
   (file-truename (if-let* ((project (project-current)))
                     (project-root project) default-directory))))

(defun br/python-interpreter ()
  "Resolve an explicit interpreter, project .venv, or system Python.
Never resolve the Python symlink inside a virtual environment."
  (let* ((root (br/python-root))
         (configured (and (listp eglot-workspace-configuration)
                          (plist-get (plist-get eglot-workspace-configuration :python)
                                     :pythonPath)))
         (override (or br/python-executable configured))
         (python (cond (override (expand-file-name override root))
                       ((file-exists-p (expand-file-name ".venv" root))
                        (expand-file-name ".venv/bin/python" root))
                       (t (or (executable-find "python3") (executable-find "python"))))))
    (when (and br/python-executable configured
               (not (equal (expand-file-name br/python-executable root)
                           (expand-file-name configured root))))
      (user-error "Conflicting br/python-executable and Eglot pythonPath overrides"))
    (unless (and python (file-executable-p python))
      (user-error "No executable Python at %s; repair .venv or set br/python-executable" python))
    python))

(defun br/python-workspace-config (_server)
  "Give BasedPyright the same interpreter as this project's REPLs."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (list :python (list :pythonPath (br/python-interpreter)))))

(defun br/python--context ()
  "Identify the project and interpreter, including environment replacement."
  (let* ((python (br/python-interpreter))
         (cfg (expand-file-name "../pyvenv.cfg" (file-name-directory python))))
    (list (br/python-root) python
          (mapcar (lambda (file)
                    (let ((a (file-attributes file)))
                      (list (nth 1 a) (nth 5 a) (nth 6 a) (nth 10 a))))
                  (list python cfg)))))

(defun br/python--ipython-p (python)
  "Test IPython in PYTHON, without falling back to a global IPython."
  (eq 0 (call-process python nil nil nil "-c"
                     "import importlib.util,sys; sys.exit(importlib.util.find_spec('IPython') is None)")))

(defun br/python-select-process ()
  "Choose a compatible running REPL or start a new Python/IPython REPL.
Remember the choice in this source buffer.
Always prompt when called explicitly."
  (interactive)
  (let* ((source (or (buffer-base-buffer) (current-buffer)))
         (context (br/python--context))
         (python (nth 1 context))
         (existing (cl-loop for p in (process-list)
                            when (and (process-live-p p)
                                      (equal context (process-get p 'br/python-context)))
                            collect (cons (buffer-name (process-buffer p)) p)))
         (choices (append existing
                          (when (br/python--ipython-p python) '(("New IPython" . ipython)))
                          '(("New Python" . python))))
         (choice (cdr (assoc (completing-read
                             (format "REPL for %s: " (abbreviate-file-name (car context)))
                             choices nil t nil nil (caar choices)) choices)))
         (process (if (processp choice) choice
                    (br/python--start choice context))))
    (with-current-buffer source (setq br/python--process process))
    (setq-local python-shell-buffer-name (process-name process))
    (with-current-buffer (process-buffer process) (setq br/python--source source))
    process))

(defun br/python--start (kind context)
  "Start KIND with native `run-python' using CONTEXT."
  (let* ((default-directory (car context))
         (python-shell-interpreter (nth 1 context))
         (prefix (expand-file-name "../" (file-name-directory python-shell-interpreter)))
         (python-shell-virtualenv-root
          (when (file-exists-p (expand-file-name "pyvenv.cfg" prefix)) prefix))
         ;; Same basic-REPL setting as Doom; IPython uses its comint-compatible UI.
         (python-shell-process-environment
          (append '("PYTHON_BASIC_REPL=1" "PYTHONHOME") python-shell-process-environment))
         (python-shell-prompt-detect-enabled nil)
         (python-shell-interpreter-args
          (if (eq kind 'ipython)
              ;; Import readline before python.el disables terminal echo.  On
              ;; macOS libedit, importing it later can turn command echo back on.
              (combine-and-quote-strings
               '("-m" "IPython" "-i" "--simple-prompt" "--no-autoindent"
                 "-c" "import readline"))
            "-i"))
         (python-shell-buffer-name
          (cl-loop for n from 1
                   for name = (format "%s:%s:%d"
                                      (if (eq kind 'ipython) "IPython" "Python")
                                      (file-name-nondirectory
                                       (directory-file-name default-directory)) n)
                   unless (get-buffer (format "*%s*" name)) return name))
         (process (run-python nil nil nil)))
    (process-put process 'br/python-context context)
    (with-current-buffer (process-buffer process)
      (setq-local header-line-format
                  (format "%s | %s" (car context) (nth 1 context))))
    process))

(defun br/python--ensure-process (&rest _)
  "Choose a process before native sending and wait for native initialization."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (let* ((source (or (buffer-base-buffer) (current-buffer)))
           (process (buffer-local-value 'br/python--process source)))
      (unless (and (process-live-p process)
                   (equal (br/python--context) (process-get process 'br/python-context)))
        (setq process (br/python-select-process)))
      (setq-local python-shell-buffer-name (process-name process))
      (with-current-buffer (process-buffer process) (setq br/python--source source))
      ;; run-python returns before python.el has installed its evaluation helpers.
      ;; The public first-prompt hook marks completion, avoiding a first-send race.
      (let ((deadline (+ (float-time) 10)))
        (while (and (process-live-p process)
                    (not (buffer-local-value 'br/python--ready (process-buffer process)))
                    (< (float-time) deadline))
          (accept-process-output process 0.05))
        (unless (and (process-live-p process)
                     (buffer-local-value 'br/python--ready (process-buffer process)))
          (user-error "Python did not initialize; inspect %s" (process-buffer process))))
      ;; ns-win selects every new frame in after-make-frame-functions,
      ;; even when display-buffer requests inhibit-switch-frame.
      (save-selected-window
        (display-buffer (process-buffer process))))))
(add-hook 'python-shell-first-prompt-hook (lambda () (setq br/python--ready t)))

(defun br/python-switch (&optional choose)
  "Switch between source and REPL; with CHOOSE prompt for another process."
  (interactive "P")
  (if (derived-mode-p 'inferior-python-mode)
      (if (buffer-live-p br/python--source) (pop-to-buffer br/python--source)
        (user-error "No live source buffer associated with this shell"))
    (when choose (br/python-select-process))
    (br/python--ensure-process)
    (python-shell-switch-to-shell)))

(defun br/python--check-region (beg end &rest _)
  "Keep native Python execution inside a single Quarto chunk."
  (when (and (derived-mode-p 'python-mode 'python-ts-mode)
             (bound-and-true-p polymode-mode))
    (let ((span (pm-innermost-span beg)))
      (unless (and (eq (car span) 'body) (<= (nth 1 span) beg end (nth 2 span)))
        (user-error "Select code within a single Python chunk"))))
  (br/python--ensure-process))
(advice-add 'python-shell-send-region :before #'br/python--check-region)

(defun br/python-send-buffer ()
  "Send the buffer, or the current Python chunk in a Quarto document."
  (interactive)
  (if (bound-and-true-p polymode-mode)
      (let ((span (pm-innermost-span)))
        (unless (and (derived-mode-p 'python-mode 'python-ts-mode) (eq (car span) 'body))
          (user-error "Place point inside a Python chunk"))
        (python-shell-send-region (nth 1 span) (nth 2 span)))
    (python-shell-send-buffer)))

(defun br/python-send-region-or-buffer ()
  "Preserve Elpy's region-or-buffer command with native Python execution."
  (interactive)
  (if (use-region-p) (call-interactively #'python-shell-send-region)
    (br/python-send-buffer)))

(defun br/python--code-spans ()
  "Return Python body bounds, or the accessible bounds of a Python buffer."
  (if (bound-and-true-p polymode-mode)
      (let (spans)
        (save-excursion
          (save-current-buffer
            (pm-map-over-spans
             (lambda (span)
               (when (and (eq (car span) 'body)
                          (derived-mode-p 'python-mode 'python-ts-mode))
                 (push (cons (nth 1 span) (nth 2 span)) spans))))))
        (nreverse spans))
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (list (cons (point-min) (point-max))))))

(defun br/python--unit-bounds ()
  "Bound one executable unit using python.el navigation, within a code span.
Include decorators and associated clauses; never parse or execute Python here."
  (save-excursion
    (python-util-forward-comment 1)
    (unless (eobp)
      (python-nav-beginning-of-statement)
      (while (python-info-dedenter-statement-p)
        (let ((opening (python-info-dedenter-opening-block-position)))
          (unless (and opening (< opening (point)))
            (user-error "Cannot find the opening Python block"))
          (goto-char opening)))
      ;; A def header includes its preceding decorator statements.
      (when (python-info-looking-at-beginning-of-defun)
        (let ((again t))
          (while (and again (not (bobp)))
            (let ((here (point)))
              (python-nav-backward-statement)
              (unless (looking-at-p "[ \t]*@")
                (goto-char here)
                (setq again nil))))))
      (let ((beg (line-beginning-position))
            (indent (current-indentation)))
        (while (looking-at-p "[ \t]*@")
          (python-nav-end-of-statement)
          (python-util-forward-comment 1))
        (if (python-info-statement-starts-block-p)
            (let (end)
              (while
                  (progn
                    (python-nav-end-of-block)
                    (setq end (point))
                    (python-util-forward-comment 1)
                    (and (not (eobp))
                         (= indent (current-indentation))
                         (python-info-dedenter-statement-p))))
              (goto-char end))
          (python-nav-end-of-statement))
        (cons beg (point))))))

(defun br/python--next-code (pos spans)
  "Find the first Python code at or after POS in SPANS, or end of buffer."
  (or (cl-loop for (beg . end) in spans
               when (< pos end)
               thereis (save-excursion
                         (save-restriction
                           (narrow-to-region beg end)
                           (goto-char (max pos beg))
                           (python-util-forward-comment 1)
                           (unless (eobp) (point)))))
      (point-max)))

(defun br/python-send-at-point ()
  "Send a selection or complete Python unit, then advance to the next.
Use python.el for execution.  Advance after submission, not after completion.
Skip comments and Quarto prose; stay put on cancellation or a local send error."
  (interactive)
  (let* ((spans (br/python--code-spans))
         (pos (if (use-region-p) (region-beginning) (br/python--next-code (point) spans)))
         (span (cl-find-if (lambda (s) (<= (car s) pos (1- (cdr s)))) spans))
         (code-buffer (if (and span (bound-and-true-p polymode-mode))
                          (pm-span-buffer (pm-innermost-span pos))
                        (current-buffer)))
         (bounds (if (use-region-p) (cons (region-beginning) (region-end))
                   (when span
                     (with-current-buffer code-buffer
                       (save-excursion
                         (save-restriction
                           (narrow-to-region (car span) (cdr span))
                           (goto-char pos)
                           (br/python--unit-bounds))))))))
    (if (not bounds)
        (message "No more Python code")
      (unless (and span (<= (cdr bounds) (cdr span)))
        (user-error "Select code within a single Python chunk"))
      (with-current-buffer code-buffer
        (save-excursion
          (python-shell-send-region (car bounds) (cdr bounds) nil t)))
      (deactivate-mark)
      (goto-char (with-current-buffer code-buffer
                   (br/python--next-code (cdr bounds) spans))))))

(defun br/python-quarto-p (&rest _)
  "Whether this is a Python inner buffer of a Quarto document."
  (and (derived-mode-p 'python-mode 'python-ts-mode)
       (buffer-base-buffer)
       (string-equal (file-name-extension
                      (or (buffer-file-name (buffer-base-buffer)) "")) "qmd")))

;; Eglot also auto-attaches when a project server is already running.
;; Keep that path out of Quarto, not just our own eglot-ensure hook.
(advice-add 'eglot--maybe-activate-editing-mode :before-until #'br/python-quarto-p)

(defun br/python-rename ()
  "Rename using the Python buffer's language client."
  (interactive)
  (call-interactively (if (bound-and-true-p lsp-mode) #'lsp-rename #'eglot-rename)))

(defun br/python-setup ()
  "Initialize source tooling after directory-local variables are available."
  (when (and buffer-file-name (not (buffer-base-buffer))
             (derived-mode-p 'python-mode 'python-ts-mode))
    (setq-local fill-column 79 comment-inline-offset 2)
    (unless (file-remote-p default-directory)
      (setq-local python-shell-interpreter (br/python-interpreter))
      (when (bound-and-true-p flycheck-mode) (flycheck-mode -1))
      (if (executable-find "basedpyright-langserver") (eglot-ensure)
        (message "Python: install basedpyright-langserver for Eglot; REPL remains available")))))
(add-hook 'hack-local-variables-hook #'br/python-setup)

(dolist (map (list python-mode-map python-ts-mode-map))
  (define-key map (kbd "C-c C-z") #'br/python-switch)
  (define-key map (kbd "C-c C-p") #'br/python-select-process)
  (define-key map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key map (kbd "C-c C-c") #'br/python-send-region-or-buffer)
  (define-key map (kbd "C-c C-b") #'br/python-send-buffer)
  (define-key map (kbd "C-<return>") #'br/python-send-at-point)
  (define-key map (kbd "C-M-x") #'python-shell-send-defun)
  (define-key map (kbd "C-c C-e") #'br/python-rename)
  (define-key map (kbd "C-c C-o") #'imenu)
  (define-key map (kbd "C-c C-d") #'eldoc-doc-buffer)
  (define-key map (kbd "C-c C-v") #'flymake-show-buffer-diagnostics)
  (define-key map (kbd "C-c C-n") #'flymake-goto-next-error))
(define-key inferior-python-mode-map (kbd "C-c C-z") #'br/python-switch)

(defun br/python-polymode-setup ()
  "Use native Python region execution in Polymode Python chunks."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (setq-local polymode-eval-region-function #'python-shell-send-region)
    (when (bound-and-true-p flycheck-mode) (flycheck-mode -1))
    (when (br/python-quarto-p)
      (remove-hook 'flymake-diagnostic-functions #'python-flymake t)
      (require 'lsp-pyright)
      ;; Polymode masks full documents correctly; incremental edits in the
      ;; host mode can otherwise leak prose into the Python document.
      (setq-local lsp-document-sync-method 1)
      (lsp-deferred))))
(with-eval-after-load 'polymode
  (add-hook 'polymode-init-inner-hook #'br/python-polymode-setup))

(provide 'br-python)
;;; br-python.el ends here
