;; Elpaca manages external packages; do not activate the old ELPA tree.
(setq package-enable-at-startup nil)

(when (eq system-type 'darwin)
  (let ((gcc-lib
         (if (file-directory-p "/opt/homebrew")
             "/opt/homebrew/opt/gcc/lib/gcc/current"
           "/usr/local/opt/gcc/lib/gcc/current")))
    (setenv "LIBRARY_PATH"
            (concat gcc-lib path-separator
                    (or (getenv "LIBRARY_PATH") "")))))
