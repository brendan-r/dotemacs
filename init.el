;; TODO
;; Show matching parens (underlining is ideal)
;; Get a decent markdown mode
;; A repl, for R and Python, ideally JS and Ruby, too
;; Map 'select all' to C-a
;; This also looks good: https://github.com/edwinhu/emacs-starter-kit

(setq gutter-buffers-tab-enabled nil)



;; Allow packages to be installed ----------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Add the emacs package dirs to the loadpath
(let ((default-directory  "~/.emacs.d/"))
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
			eval-in-repl
			fill-column-indicator
			stan-mode
			stan-snippets
			virtualenvwrapper
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
  (package-refresh-contents)
  (dolist (package my-package-list)
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
(define-key cua-global-keymap (kbd "C-f") 'isearch-forward)


;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-L") 'mc/edit-lines)
(global-set-key (kbd "C-/") 'comment-line)


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
(global-set-key (kbd "<C-o>") 'neotree-toggle)


;; Alt-arrow for window navigation
(global-set-key (kbd "<M-up>")    'windmove-up)
(global-set-key (kbd "<M-down>")  'windmove-down)
(global-set-key (kbd "<M-left>")  'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)


;; Control-Tab for buffer navigation within a window (Control-Shift-Tab to go in
;; the other direction)
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)



;; Yasnippets ------------------------------------------------------------------

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode 1)



;; REPL / comint settings ------------------------------------------------------

(require 'comint)

(define-key comint-mode-map (kbd "<up>")
  'comint-previous-input)

(define-key comint-mode-map (kbd "<down>")
  'comint-next-input)


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

;; Make wrapping at the end of a line use whole words
;; Note, you want to exclude neo tree from this, it makes it very ugly
;; Probably html files too (they tend to drift over 80 chars)
(global-visual-line-mode t)



;; Fonts -----------------------------------------------------------------------
;; 10pt (in 1/10pt units)
(set-face-attribute 'default nil :height 105)



;; Menu / minbuffer navigation -------------------------------------------------

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)



;; Text wrapping ---------------------------------------------------------------

(setq fill-column 80)

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


;; Tell multi-term to unbind C-v as a special terminal thing
;;(require 'multi-term)
;;(add-to-list 'term-unbind-key-list "C-v")

(add-hook 'term-mode-hook (lambda ()
  (define-key term-raw-map (kbd "C-v") 'term-paste)))


;; Map it to term-paste
;; (defun my-term-mode-hook ()
;;   (define-key term-raw-map (kbd "C-v") 'term-paste))
;; (add-hook 'term-mode-hook 'my-term-mode-hook)




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

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))

(setq markdown-use-pandoc-style-yaml-metadata t)


;; Unbind tab so it's possible to use yasnippets
;; from http://wiki.dreamrunner.org/public_html/Emacs/markdown.html
(add-hook 'markdown-mode-hook
          '(lambda ()
             (auto-complete-mode t)
             (local-unset-key [tab])
             (setq-local yas-fallback-behavior '(apply auto-complete))))

;; Unbind Alt-arrow so that you can use it to navigate windows
(add-hook 'markdown-mode-hook
          '(lambda ()
             (auto-complete-mode t)
             (local-unset-key [tab])))


;; Start-up --------------------------------------------------------------------

;; Remove the splash-screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Taken from http://unix.stackexchange.com/a/152151
;; Makes *scratch* empty.
(setq initial-scratch-message "")

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


;; R and ESS -------------------------------------------------------------------

(require 'ess-site)

;; Start R in the working directory by default
(setq ess-ask-for-ess-directory nil)

;; Stop ess converting an underscore to <-
(ess-toggle-underscore nil)

;; Possibly make it do Rstudio style things...?
(setq ess-default-style (quote RStudio))

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

;; Stan ------------------------------------------------------------------------

(require 'stan-mode)
(require 'stan-snippets)

;; Polymode --------------------------------------------------------------------

;; Polymode allow you to see formatted code snippets in documents like markdown
;; and Rmarkdown

;;; Markdown
(add-to-list 'auto-mode-alist '("\\.md"   . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))



;; Appearance ------------------------------------------------------------------

;; Make emacs transparent
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(99 . 98))
(add-to-list 'default-frame-alist '(alpha . (99 . 98)))


;; Hipster powerline
;; (require 'telephone-line)
;; (telephone-line-mode 1)

;; (require 'powerline)
;; (powerline-default-theme)


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
;; (require 'notmuch)


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

(require 'eyebrowse)
(global-set-key (kbd "C-c C-w") 'eyebrowse-keymap-prefix)
(eyebrowse-mode 1)


;; You do not understand what this does and it's pissing you off ---------------
