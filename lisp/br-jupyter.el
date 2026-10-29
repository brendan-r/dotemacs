;;; br-jupyter.el --- Local Jupyter setup checks -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ob)

(defun br/jupyter-setup-warning (reason)
  "Explain how to repair local Jupyter support after REASON."
  (let ((text
         (format
          (concat "Jupyter support is not ready: %s\n"
                  "In a terminal, run:\n  sh %s\n"
                  "This installs a private Python/Jupyter environment and builds "
                  "the Emacs ZeroMQ module; it needs Python 3.12+, a C/C++ compiler, "
                  "make, and internet access.\n"
                  "Then run M-x br/jupyter-initialize (no Emacs restart needed).\n"
                  "See scripts/jupyter/jupyter.org for machine prerequisites.")
          reason
          (shell-quote-argument
           (expand-file-name "scripts/jupyter/setup-jupyter.sh" user-emacs-directory)))))
    (display-warning 'jupyter text :warning)
    ;; Keep the repair instructions in Messages even if warnings are deferred.
    (message "%s" text))
  nil)

;;;###autoload
(defun br/jupyter-initialize ()
  "Enable local Jupyter kernels after checking external dependencies.
Safe to call again after running scripts/jupyter/setup-jupyter.sh.  Missing native
modules are reported before zmq can attempt its automatic download/build."
  (interactive)
  (let* ((managed (expand-file-name "~/.local/share/emacs/jupyter/bin/jupyter"))
         ;; Prefer our known environment when present, including in GUI Emacs.
         (cli (if (file-executable-p managed) managed (executable-find "jupyter")))
         (library (locate-library "zmq.el"))
         (module (and library module-file-suffix
                      (expand-file-name (concat "emacs-zmq" module-file-suffix)
                                        (file-name-directory library))))
         (missing
          (delq nil (list (unless cli "Jupyter CLI is missing")
                          (unless module-file-suffix
                            "this Emacs was built without dynamic module support; install a module-enabled Emacs")
                          (unless library "the Emacs zmq package is missing; install the jupyter package with M-x package-install")
                          (when (and module (not (file-exists-p module)))
                            "the Emacs ZeroMQ native module has not been built")))))
    (if missing
        (br/jupyter-setup-warning (mapconcat #'identity missing "; "))
      (condition-case err
          (progn
            (require 'jupyter-env)
            (setq jupyter-executable cli)
            ;; Check the command now, before installing Org's discovery hook.
            (with-temp-buffer
              (unless (eq 0 (call-process cli nil t nil "kernelspec" "list" "--json"))
                (error "Jupyter kernelspec discovery failed: %s" (buffer-string))))
            (require 'zmq)
            (zmq-load)
            (require 'jupyter-base)
            (setq jupyter-use-zmq t)
            (org-babel-do-load-languages
             'org-babel-load-languages
             (cons '(jupyter . t) (assq-delete-all 'jupyter org-babel-load-languages)))
            (when (derived-mode-p 'org-mode)
              (org-babel-jupyter-make-local-aliases))
            (when (called-interactively-p 'interactive)
              (message "Jupyter is ready (%s)" cli))
            t)
        (error (br/jupyter-setup-warning (error-message-string err)))))))

(provide 'br-jupyter)
;;; br-jupyter.el ends here
