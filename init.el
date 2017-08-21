;; TODO
;;
;; - Figure out how to make the iESS R buffer show the last command run
;;   (even if it was via ess-eval-region, or similar)
;;
;; - Find a good way to do auto-completion (with visible suggestions)
;;
;; - See if you can find a reasonable way to work with SQL interactively
;;
;; - Get a shortcut to pop multi-term
;;
;; - Figure out a good shortcut to do reverse search in a terminal
;;
;; Comint mode all shells:
;;
;; - If you enter the buffer while it's hung and hit enter, you feed the prompt
;;   character(s) back into the shell, which is very annoying
;;



;; Allow packages to be installed ----------------------------------------------

(require 'package)
(require 'cl)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 25)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Add the emacs package dirs to the loadpath
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Make a list of the packages you want
(setq my-package-list '(
			magit
			ess
			projectile
			eyebrowse
			yasnippet
			eval-in-repl
			expand-region
			fill-column-indicator
			stan-mode
			spaceline
			mouse3
			neotree
			multi-term
      ;; Note: If Stan is not installed, this seems to break everything
			stan-snippets
			virtualenvwrapper
      flycheck
      flyspell
      rainbow-delimiters
      smartparens
      web-mode
      magithub
      company
      olivetti
      exec-path-from-shell
      multiple-cursors
      markdown-mode
      auto-complete
      color-theme-sanityinc-tomorrow
      polymode
      let-alist ;; Terminal version seems ask for this periodically
      persp-mode
      which-key
			))

;; Activate package autoloads
(package-initialize)
(setq package-initialize nil)

;; make sure stale packages don't get loaded
(dolist (package my-package-list)
  (if (featurep package)
      (unload-feature package t)))

;; Install packages in package-list if they are not already installed
(unless (cl-every #'package-installed-p my-package-list)
  (package-refresh-contents) (dolist (package my-package-list)
    (when (not (package-installed-p package))
      (package-install package))))

;; add custom lisp directory to path
(let ((default-directory (concat user-emacs-directory "lisp/")))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; on OSX Emacs needs help setting up the system paths
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))



;; Key bindings ----------------------------------------------------------------

;; Turn on easy mode (you have OS level keybindings!)
(cua-mode t)

;; Don't tabify after rect' commands
(setq cua-auto-tabify-rectangles nil)

;; No region when it is not highlighted
(transient-mark-mode 1)

;; Standard Windows behaviour
(setq cua-keep-region-after-copy t)

;; C-a -> select all
(define-key cua-global-keymap (kbd "C-a") 'mark-whole-buffer)

;; C-s -> save
(define-key cua-global-keymap (kbd "C-s") 'save-buffer)

;; C-f -> find
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)

;; Kill the buffer and close the window
(define-key cua-global-keymap (kbd "C-S-w") 'kill-buffer-and-window)

;; Close the current window
(define-key cua-global-keymap (kbd "C-w") 'delete-window)

;; Kill the current buffer
(define-key cua-global-keymap (kbd "C-k") 'kill-this-buffer)

;; Multiple cursors
(require 'multiple-cursors)

;; Where you have a selection, put a cursor on each line
(define-key cua-global-keymap (kbd "C-S-l") 'mc/edit-lines)

;; Use the current point as an anchor to create multiple cursors
(define-key cua-global-keymap (kbd "C-S-k") 'set-rectangular-region-anchor)

(define-key cua-global-keymap (kbd "C-/") 'comment-line)

;; (setq-default mc/max-cursors 20)

;; Fill/re-flow comments etc.
(define-key cua-global-keymap (kbd "C-?") 'fill-individual-paragraphs)

;; Mouse bindings
;; Make right-click do something close to what people expect
(global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)


;; Free up C-RET for REPLs (by default used by cua-rectangle-mark-mode, move the
;; latter to C-S-SPC)
(define-key cua-global-keymap (kbd "<C-return>") nil)
(define-key cua-global-keymap (kbd "<C-S-SPC>") nil)
(setq cua-rectangle-mark-key (kbd "<C-S-SPC>"))
(define-key cua-global-keymap (kbd "<C-S-SPC>") 'cua-rectangle-mark-mode)


;; Shortcuts for changing the size of the windows
(global-set-key (kbd "C-M-{") 'shrink-window)             ;; Vertically
(global-set-key (kbd "C-M-}") 'enlarge-window)            ;; Vertically
(global-set-key (kbd "C-{") 'shrink-window-horizontally)  ;; Horizontally
(global-set-key (kbd "C-}") 'enlarge-window-horizontally) ;; Horizontally


;; C-o for neotree
(global-set-key (kbd "C-o") 'neotree-toggle)


;; Alt-arrow for window navigation
(global-set-key (kbd "<M-up>")    'windmove-up)
(global-set-key (kbd "<M-down>")  'windmove-down)
(global-set-key (kbd "<M-left>")  'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)


;; Control-Tab for buffer navigation within a window (Control-Shift-Tab to go in
;; the other direction)
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)



;; General behavior ------------------------------------------------------------

;; Do spell checking in your comments!
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Do spell checking in normal text!
(add-hook 'text-mode-hook #'flyspell-mode)

;; Spell checking in ESSmode (doesn't seem to work)
(add-hook 'ess-mode-hook #'flyspell-prog-mode)

;; Control-arrow over a whole word, e.g. from |long_word to long_word|, not
;; long|_word
(setq superword-mode 1)

;; Use smartparens
(add-hook 'prog-mode-hook #'smartparens-mode)
;; (add-hook 'ess-mode-hook #'rainbow-delimiters-mode)

;; Use rainbow delimiters when programming
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Load the new version of a file when it changes
(global-auto-revert-mode t)


;; Yasnippets ------------------------------------------------------------------

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode 1)



;; REPL / comint settings ------------------------------------------------------

(require 'comint)

;; Use up and down to scroll through command history
(define-key comint-mode-map (kbd "<up>")
  'comint-previous-matching-input-from-input)

(define-key comint-mode-map (kbd "<down>")
  'comint-next-matching-input-from-input)

;; Possibly make comint buffers more well-behaved when throwing text around
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)


;; Automatically adjust output width in commint buffers
;; from http://stackoverflow.com/a/11255996/7237812
(defun comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (eq nil process)
        (set-process-window-size process (window-height) (window-width))))))

(defun my-shell-mode-hook ()
  ;; add this hook as buffer local, so it runs once per window.
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))

(add-hook 'shell-mode-hook
          (lambda()
             ;; add this hook as buffer local, so it runs once per window.
             (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t)))


;; GUI stuff -------------------------------------------------------------------

;; Remove the scroll bars
(scroll-bar-mode -1)

;; You don't tend to use the toolbar, or the menubar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; No need for the cursor to blink
(blink-cursor-mode -1)

;; No need for bells to ring
(setq ring-bell-function 'ignore)

;; Use a bar cursor instead of the block thing
(setq-default cursor-type 'bar)



;; General text display --------------------------------------------------------

;; Make wrapping at the end of a line use whole words Note, you want to exclude
;; neo tree from this, it makes it very ugly Probably html files too (they tend
;; to drift over 80 chars)
(global-visual-line-mode t)



;; Fonts -----------------------------------------------------------------------
;; 10pt (in 1/10pt units)
(set-face-attribute 'default nil :height 105)



;; Menu / minbuffer navigation -------------------------------------------------

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)



;; Text wrapping ---------------------------------------------------------------

;; Emacs' default is to wrap at 70 columns. This works really well for git
;; commit messages (and potentially emails), but not so well when programming,
;; where you tend to use 80 cols

;; When programming, wrap at 80
(add-hook 'prog-mode-hook
          (lambda ()
            (set-fill-column 80)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-fill-column 80)))

;; When writing prose -- wrap
;; (add-hook 'text-mode-hook '(lambda ()
;;     (setq truncate-lines nil
;;           word-wrap t
;;           global-visual-line-mode 1)))

;; When writing code -- truncate
;; (add-hook 'prog-mode-hook '(lambda ()
;;     (setq truncate-lines t
;;           word-wrap nil
;;           global-visual-line-mode 1)))

;; Note: You can  use M-q and C-u M-q to flow text to 80 columns



;; Line numbers ----------------------------------------------------------------

(require 'linum)
(setq linum-format "%3d ")
(global-linum-mode 1)



;; Encoding --------------------------------------------------------------------

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)



;; ansi-term stuff -------------------------------------------------------------

;; Remove line numbers in term-mode
(add-hook 'term-mode-hook 'my-inhibit-global-linum-mode)

(defun my-inhibit-global-linum-mode ()
  "Counter-act `global-linum-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda () (linum-mode 0))
            :append :local))

;; always use bash
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; enable cua and transient mark modes in term-line-mode
;; Taken from https://www.emacswiki.org/emacs/AnsiTermHints
(defadvice term-line-mode (after term-line-mode-fixes ())
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'transient-mark-mode) t))
(ad-activate 'term-line-mode)


;; disable cua and transient mark modes in term-char-mode
;; Turning this off to see if you can copy and paste
;;(defadvice term-char-mode (after term-char-mode-fixes ())
;;  (set (make-local-variable 'cua-mode) nil)
;;  (set (make-local-variable 'transient-mark-mode) nil))
;;(ad-activate 'term-char-mode)

;; (defadvice term-char-mode (after term-char-mode-fixes ())
;;  (set (make-local-variable 'cua-mode) t)
;;  (set (make-local-variable 'transient-mark-mode) t))
;;(ad-activate 'term-char-mode)

;; From https://www.reddit.com/r/emacs/comments/f0ypy/tmux_in_emacs_shell/?st=iym0z24b&sh=43afbdd8


;; Close dead term buffers
;; http://oremacs.com/2015/01/01/three-ansi-term-tips/
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

;; Always use UTF8
;; https://echosa.github.io/blog/2012/06/06/improving-ansi-term/
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)



;; Scrolling -------------------------------------------------------------------

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;; keyboard scroll one line at a time
(setq scroll-step 1)


;; 80 col rule -----------------------------------------------------------------

(require 'fill-column-indicator)
(setq fci-rule-column 80)


;; Windows ---------------------------------------------------------------------

;; If you have two windows, swap between a horizontal and vertical split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)



;; Markdown --------------------------------------------------------------------

;; Use markdown mode for the following file types
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))

;; Allow pandoc yaml metadata
(setq markdown-use-pandoc-style-yaml-metadata t)

;; Unbind tab so it's possible to use yasnippets from
;; http://wiki.dreamrunner.org/public_html/Emacs/markdown.html
(add-hook 'markdown-mode-hook
          '(lambda ()
             (auto-complete-mode t)
             (local-unset-key [tab])
             (setq-local yas-fallback-behavior '(apply auto-complete))))

;; Unbind Alt-arrow so that you can use it to navigate windows
(add-hook 'markdown-mode-hook
      (lambda ()
        (local-unset-key (kbd "<M-up>"))
        (local-unset-key (kbd "<M-left>"))
        (local-unset-key (kbd "<M-right>"))
        (local-unset-key (kbd "<M-down>"))
        ))

;; Allow math mode for stuff in-between $..$ or $$..$$
(setq markdown-enable-math t)

;; Use Olivetti mode whenever the mode loads (and disable linum-mode)
(require 'olivetti)
(add-hook 'markdown-mode-hook (lambda() (linum-mode -1)))
(add-hook 'markdown-mode-hook (lambda() (olivetti-mode t)))



;; Start-up --------------------------------------------------------------------

;; Remove the splash-screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Taken from http://unix.stackexchange.com/a/152151
;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Change the scratch buffer to markdown-mode
(setq initial-major-mode 'markdown-mode)


;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it
;; on startup.
;;
;; https://stackoverflow.com/a/23365580
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;; Attempt to remove the ESS buffer
;; (kill-buffer "*ESS*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)



;; Multi-term ------------------------------------------------------------------


;; General code editing --------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; SQL -------------------------------------------------------------------------

;; If it's there, load some information on database connections (keep this out
;; of version control for security reasons)
(load "databases.el" 'missing-ok nil)

;; Try to make sql just use regular old spaces for indentation
;;
;; From: http://emacs.stackexchange.com/a/16513/15016
;; Notes: Very weird, everything is constantly indented a lot
;;
;; (defun my-indent ()
;;   (let ((last-indent (if (> (line-number-at-pos) 1)
;;                          (save-excursion
;;                            (previous-line)
;;                            (back-to-indentation)
;;                            (current-column))
;;                        0)))
;;     (save-excursion
;;       (back-to-indentation)
;;       (if (and (eq last-command this-command)
;;                (> (point) (line-beginning-position)))
;;           (delete-region (max (line-beginning-position) (- (point) 4)) (point))
;;         (while (< (current-column) (+ 4 last-indent))
;;           (insert " "))))
;;     (if (< (point) (save-excursion (back-to-indentation) (point)))
;;         (back-to-indentation))))


;; (defun my-sql-mode-hook ()
;;   (setq indent-line-function 'my-indent))

;; (add-hook 'sql-mode-hook 'my-sql-mode-hook)


;; From http://emacs.stackexchange.com/a/13921/15016
;; Use the regular major mode hook to add a buffer-local hack-local-variables-hook
;; (add-hook 'sql-mode-hook 'my-sql-mode-hook)
;; (defun my-sql-mode-hook ()
;;   (add-hook 'hack-local-variables-hook
;;             (lambda () (setq indent-tabs-mode nil) (setq tab-width 4) )))
;;             nil t))


;; Web stuff -------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass?\\'" . web-mode))

;; Bash scripts / terminal repl ------------------------------------------------

(require 'essh) ; if not done elsewhere; essh is in the local lisp folder
(require 'eval-in-repl)
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          '(lambda()
             (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))



;; Python ----------------------------------------------------------------------

(with-eval-after-load "python"
  ;; try to get indent/completion working nicely
  (setq python-indent-trigger-commands '(my-company-indent-or-complete-common
					 indent-for-tab-command
					 yas-expand
					 yas/expand
					 ))
  ;; readline support is wonky at the moment
  (setq python-shell-completion-native-enable nil)
  ;; simple evaluation with C-ret
  (require 'eval-in-repl-python)
  (define-key python-mode-map "\C-c\C-c" 'eir-eval-in-python)
  (define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python))

(add-hook 'inferior-python-mode-hook (lambda () (linum-mode -1)))

;; Use Python 3 via Anaconda -- this doesn't work
;; (setq python-python-command "/home/br/anaconda3/bin/python")

;; This should make the virtual env stuff work in a repl
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)



;; Elisp -----------------------------------------------------------------------
;; Note: This is straight-up stolen from izahn's dotfiles at
;; https://github.com/izahn/dotemacs/blob/496c502aad691deaed1076318e92035d229cfa40/init.el#L486

(with-eval-after-load "elisp-mode"
  (require 'company-elisp)
  ;; ielm
  (require 'eval-in-repl-ielm)
  ;; For .el files
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
  (define-key emacs-lisp-mode-map (kbd "<C-S-return>") 'eval-buffer)
  ;; For *scratch*
  (define-key lisp-interaction-mode-map "\C-c\C-c" 'eir-eval-in-ielm)
  (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  (define-key lisp-interaction-mode-map (kbd "C-c C-b") 'eval-buffer)
  (define-key lisp-interaction-mode-map (kbd "<C-S-return>") 'eval-buffer)
  ;; For M-x info
  (define-key Info-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
  ;; Set up completions
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              ;; make sure completion calls company-elisp first
              (require 'company-elisp)
              (setq-local company-backends
                          (delete-dups (cons 'company-elisp (cons 'company-files company-backends)))))))



;; R and ESS -------------------------------------------------------------------

(require 'ess-site)

;; Don't write everything out to a history file
(setq ess-history-file nil)

;; Start R in the working directory by default
(setq ess-ask-for-ess-directory nil)

;; Stop ess converting an underscore to <-
(ess-toggle-underscore nil)

;; Possibly make it do Rstudio style things...?
(setq ess-default-style (quote RStudio))

;; Allow autocompletion in script files (not just the shell)
(setq ess-tab-complete-in-script t)

;; Try to make the default 'send to REPL' command C-RET
(define-key ess-mode-map (kbd "C-RET")
  'ess-eval-region-or-function-or-paragraph-and-step)

;; Remove line-numbers in the R REPL
(add-hook 'inferior-ess-mode-hook (lambda () (linum-mode -1)))

;; This should make things not freeze, I think
(setq ess-eval-visibly-p 'nowait)

;; Shortcut for the %>% operator
(defun then_R_operator ()
  "R - %>% operator pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))

(define-key ess-mode-map (kbd "C-<") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "C-<") 'then_R_operator)

;; Make C-l clear the iESS comit buffer
(define-key ess-mode-map (kbd "C-l") 'comint-clear-buffer)
(define-key inferior-ess-mode-map (kbd "C-l") 'comint-clear-buffer)


;; For some reason flyspell needs to be enabled for ESS specifically
(add-hook 'ess-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (run-hooks 'prog-mode-hook)
            ))

;; The iESS R buffer only seems to be able to cycle through things directly
;; typed into it (e.g. not sent from and R buffer), which makes it pretty
;; useless. The commands below are an attempt to get the keybindings working for
;; commands *either* typed or sent from and R buffer
;;
;; From: https://stackoverflow.com/a/27440051

;; (defun ess-readline ()
;;   "Move to previous command entered from script *or* R-process and copy
;;    to prompt for execution or editing"
;;   (interactive)
;;   ;; See how many times function was called
;;   (if (eq last-command 'ess-readline)
;;       (setq ess-readline-count (1+ ess-readline-count))
;;     (setq ess-readline-count 1))
;;   ;; Move to prompt and delete current input
;;   (comint-goto-process-mark)
;;   (end-of-buffer nil) ;; tweak here
;;   (comint-kill-input)
;;   ;; Copy n'th command in history where n = ess-readline-count
;;   (comint-previous-prompt ess-readline-count)
;;   (comint-copy-old-input)
;;   ;; Below is needed to update counter for sequential calls
;;   (setq this-command 'ess-readline)
;; )


;; (defun ess-readnextline ()
;;   "Move to next command after the one currently copied to prompt and copy
;;    to prompt for execution or editing"
;;   (interactive)
;;   ;; Move to prompt and delete current input
;;   (comint-goto-process-mark)
;;   (end-of-buffer nil)
;;   (comint-kill-input)
;;   ;; Copy (n - 1)'th command in history where n = ess-readline-count
;;   (setq ess-readline-count (max 0 (1- ess-readline-count)))
;;   (when (> ess-readline-count 0)
;;       (comint-previous-prompt ess-readline-count)
;;   (comint-copy-old-input))
;;   ;; Update counter for sequential calls
;;   (setq this-command 'ess-readline)
;;   )

;; ;; Define them in the ESS REPL only
;; (define-key inferior-ess-mode-map (kbd "<up>") 'ess-readline)
;; (define-key inferior-ess-mode-map (kbd "<down>") 'ess-readnextline)



;; Knitr / Rmarkdown -----------------------------------------------------------

;; Check this out for knitr chunk evaluation:
;; http://stackoverflow.com/a/40966640/7237812
(defun rmd-send-chunk ()
  "Send current R chunk to ess process."
  (interactive)
  (and (eq (oref pm/chunkmode :mode) 'r-mode)
       (pm-with-narrowed-to-span nil
         (goto-char (point-min))
         (forward-line)
         (ess-eval-region (point) (point-max) nil nil 'R))))

;; E.g. to send a prefix, use C-u M-x rmd-send-buffer
(defun rmd-send-buffer (arg)
  "Send all R code blocks in buffer to ess process. With prefix
send regions above point."
  ;; Interactive, with a prefix argument
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      (pm-map-over-spans
       'rmd-send-chunk (point-min)
       ;; adjust this point to send prior regions
       (if arg (point) (point-max))))))

;; When the R/ESS shell resizes, let have access to that information for printing
;; http://stackoverflow.com/a/20015336/7237812
(defun comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (eq nil process)
        (set-process-window-size process (window-height) (window-width))))))


;; Tried to make R code foldable in polymode, no such luck so far
;; The below works for
(add-to-list 'hs-special-modes-alist
           '(markdown-mode "```" "```" nil nil nil))

;; Stan ------------------------------------------------------------------------

(require 'stan-mode)
;; Note: If Stan isn't installed, this seems to break verything
(require 'stan-snippets)



;; Polymode allow you to see formatted code snippets in documents like markdown
;; and Rmarkdown

;;; Markdown
(add-to-list 'auto-mode-alist '("\\.md"   . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))


;; Big Rmarkdown buffers get slow af. See if this helps.
(add-hook 'poly-markdown+r-mode-hook (lambda() (linum-mode -1)))
(add-hook 'poly-markdown+r-mode-hook (lambda() (olivetti-mode t)))


;; Git -------------------------------------------------------------------------

;; magit is great, and let's try magithub
(require 'magit)
(require 'magithub)
(magithub-feature-autoinject t)

;; Set key for magit-status
(global-set-key (kbd "C-x C-g") 'magit-status)



;; Appearance ------------------------------------------------------------------

;; Make emacs transparent
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
;;(set-frame-parameter (selected-frame) 'alpha '(99 . 98))
;; (add-to-list 'default-frame-alist '(alpha . (99 . 98)))


;; Hipster modeline
(require 'spaceline-config)
(spaceline-spacemacs-theme)


;; Your theme
(require 'sanityinc-tomorrow-eighties-theme)
;;(require 'sanityinc-tomorrow-bright-theme)


;; Use Emacs terminfo, not system terminfo
;; http://stackoverflow.com/a/8920373
(setq system-uses-terminfo nil)



;; Misc ------------------------------------------------------------------------

;; Run emacs as a server with clients
(server-start)

;; Strip trailing whitespace from files on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)



;; Mail ------------------------------------------------------------------------

;; Note: This is not in the list of required packages above, as it's
;; recommended that you use the emacs-client bundled with the primary notmuch
;; installation itself (as there are API changes)
(autoload 'notmuch "notmuch" "notmuch mail" t)


;; Put mail stuff in another config, load it in if you actually use it!
(load "mail.el" 'missing-ok nil)

;; Project management ----------------------------------------------------------

;; (require 'projectile)
;; (require 'perspective)
;; (require 'persp-projectile)
;; (persp-mode)

;; ;; Get projectile to user .Rproj files to indicate projects
;; (add-to-list 'projectile-project-root-files-bottom-up ".Rproj")

;; (define-key projectile-mode-map (kbd "C-x x s")
;;   'projectile-persp-switch-project)

;; Trying out workgroups... I have suspicion I'll end up using eyebrowse

;; (require 'eyebrowse)
;; (global-set-key (kbd "C-c C-w") 'eyebrowse-keymap-prefix)
;; (eyebrowse-mode 1)

;; (require 'persp-mode)
;; (persp-mode 1)



;; Customization ---------------------------------------------------------------

;; When making changes via M-x customize-group, save the settings to a separate
;; file
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region ";; Put user configuration here" nil custom-file))
(load custom-file 'noerror)



;; Extra files -----------------------------------------------------------------

;; You don't have a particular use for the file-lock files (your current
;; understanding is that they only work if another Unix user on the same system
;; modifies the same file at the same time, also using Emacs, which does not
;; seem like a problem that you'll run into often)
(setq create-lockfiles nil)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/auto-save-list/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/auto-save-list/" t)))
