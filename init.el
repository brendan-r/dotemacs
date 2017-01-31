;; TODO
;; Show matching parens (underlining is ideal)
;; Get a decent markdown mode
;; A repl, for R and Python, ideally JS and Ruby, too
;; Map 'select all' to C-a
;; This has the desired repl behaiour https://github.com/izahn/dotemacs
;; This also looks good: https://github.com/edwinhu/emacs-starter-kit


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


;; Key bindings ----------------------------------------------------------------

;; Turn on easy mode (you have OS level keybindings!)
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rect' commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; Fancy text editing
(require 'multiple-cursors)


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

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; General text display --------------------------------------------------------

;; Make wrapping at the end of a line use whole words
;; Note, you want to exclude neo tree from this, it makes it very ugly
;; Probably html files too (they tend to drift over 80 chars)
(global-visual-line-mode t)


;; Cursor ----------------------------------------------------------------------
(setq-default cursor-type 'bar)

;; Fonts -----------------------------------------------------------------------
;; 11pt (in 1/10pt units)
(set-face-attribute 'default nil :height 110)

;; Menu / minbuffer navigation -------------------------------------------------

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)



;; Text wrapping ---------------------------------------------------------------

;; (setq fill-column 80)

;; Note, it doesn't look

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


;; Navigating buffers ----------------------------------------------------------

;; tabbar
;; (require 'tabbar)
;; (setq tabbar-use-images nil)

;; tabbar-ruler
;; (require 'tabbar-ruler)

;; (setq tabbar-ruler-global-tabbar t)    ; get tabbar
;; ;; (setq tabbar-ruler-global-ruler t)     ; get global ruler
;; ;; (setq tabbar-ruler-popup-menu t)       ; get popup menu.
;; ;; (setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;; (setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move


;; Key mapping -----------------------------------------------------------------
(global-set-key (kbd "C-o") 'neotree-toggle)



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

;; Make pasting meaningful (doesn't actually work)
(defun my-term-mode-hook ()
  (define-key term-raw-map (kbd "\C-v") 'term-paste))
(add-hook 'term-mode-hook 'my-term-mode-hook)


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


;; Appearance ------------------------------------------------------------------

;; Make emacs transparent
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(99 . 98))
(add-to-list 'default-frame-alist '(alpha . (99 . 98)))


;; Hipster powerline
(require 'powerline)
(powerline-default-theme)

;; Your theme
(require 'sanityinc-tomorrow-eighties-theme)
;;(require 'sanityinc-tomorrow-bright-theme)


;; Use Emacs terminfo, not system terminfo
;; http://stackoverflow.com/a/8920373
(setq system-uses-terminfo nil)


;; Smooth mouse scrolling ------------------------------------------------------
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time


;; Markdown --------------------------------------------------------------------


(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))

(setq markdown-use-pandoc-style-yaml-metadata t)


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


;; R and ESS -----------------------------------------------------
(require 'ess-site)

;; Stop ess converting an underscore to <-
(ess-toggle-underscore nil)

;; Possibly make it do Rstudio style things...?
(custom-set-variables
  '(ess-default-style (quote RStudio)))


;; Polymode --------------------------------------------------------------------
;; Polymode allow you to see formatted code snippets in documents like markdown
;; and Rmarkdown

;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md"   . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

