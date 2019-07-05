;; TODO
;;
;; - Try out straight.el
;;
;; - Add a .csv mode (toggle-truncate-lines and csv-align-fields on a mode hook
;;   are probably sufficient)
;;
;; - Figure out how to make the iESS R buffer show the last command run
;;   (even if it was via ess-eval-region, or similar)
;;
;; - Find a good way to do auto-completion in R (with visible suggestions)
;;
;; - See if you can find a reasonable way to work with SQL interactively
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
                        yasnippet
                        eval-in-repl
                        expand-region
                        fill-column-indicator
                        stan-mode
                        spaceline
                        multi-term
                        ;; Note: If Stan is not installed, this seems to break everything
                        stan-snippets
                        virtualenvwrapper
                        flycheck
                        flyspell
                        flyspell-lazy
                        flyspell-correct-ivy
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
                        poly-R
                        poly-markdown
                        let-alist ;; Terminal version seems ask for this periodically
                        persp-mode
                        which-key
                        ivy
                        mwim
                        fold-this
                        nodejs-repl
                        use-package
                        undo-tree
                        sonic-pi
                        smex
                        elpy
                        ranger
                        skewer-mode
                        simple-httpd
                        counsel
                        elfeed
                        cl-lib
                        yaml-mode
                        ;; For reviewing papers
                        pdf-tools
                        org-ref
                        ivy-bibtex
                        interleave
                        frames-only-mode
                        ;; Optional, for work stuff
                        org-gcal
                        anki-editor
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



;; which-key -------------------------------------------------------------------
;; Which key provides a little menu for if you get a brain-freeze in the middle
;; of typing out command
(which-key-mode t)



;; Flycheck --------------------------------------------------------------------

;; Provide some settings to chill flycheck out a little
(setq flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
(setq flycheck-idle-change-delay 3)


;; Flyspell --------------------------------------------------------------------

;; When using aspell, use this environment variable so that your personal
;; dictionary is synced across machines
(setenv "ASPELL_CONF" (concat "home-dir " (expand-file-name
                                           "~/Nextcloud/dictionary/")))

;; Try to calm Flyspell down a little, to reduce typing latency on slower
;; machines
(require 'flyspell-lazy)
(flyspell-lazy-mode 1)

;; Do spell checking in normal text!
(add-hook 'text-mode-hook #'flyspell-mode)

;; Do spell checking in your comments!
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Spell checking in ESSmode (doesn't seem to work)
(add-hook 'ess-mode-hook #'flyspell-prog-mode)

;; For some reason flyspell needs to be enabled for ESS specifically
(add-hook 'ess-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (run-hooks 'prog-mode-hook)
            ))

;; Make use Ivy for flyspell completions.
;;
;; Note: You should try and make this run 'flyspell-buffer' prior to the correct
;; command, now that flyspell is slow
;; (require 'flyspell-correct-ivy)
;; (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)


;; Key bindings ----------------------------------------------------------------

;; Turn on easy mode (you have OS level keybindings!)
(cua-mode t)

;; Use move-where-i-mean to arrive at the beginning/end of indented lines
(setq mwim-beginning-of-line-function 'beginning-of-visual-line
      mwim-end-of-line-function 'end-of-visual-line)

(define-key cua-global-keymap (kbd "<home>") 'mwim-beginning)
(define-key cua-global-keymap (kbd "<end>") 'mwim-end)

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

;; Kill the buffer and close the window
(define-key cua-global-keymap (kbd "C-S-w") 'kill-buffer-and-window)

;; Close the current window
(define-key cua-global-keymap (kbd "C-w") 'delete-window)

;; Switch buffer
(define-key cua-global-keymap (kbd "C-b") 'ivy-switch-buffer)

;; Kill the current buffer
(define-key cua-global-keymap (kbd "C-k")
  '(lambda () (interactive) (kill-buffer (current-buffer))))

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

;; Markdown mode now has it's own block movement commands, but they break CUA
;; mode. Will this overwrite them?
;;(global-set-key (kbd "C-down") 'forward-paragraph)
;;(global-set-key (kbd "C-up") 'backward-paragraph)

;; Alt-arrow for window navigation
(global-set-key (kbd "<M-up>")    'windmove-up)
(global-set-key (kbd "<M-down>")  'windmove-down)
(global-set-key (kbd "<M-left>")  'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)


;; Control-Tab for buffer navigation within a window (Control-Shift-Tab to go in
;; the other direction)
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)

;; Stuff if you're using a Mac
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'meta)


;; Undo / redo
(global-undo-tree-mode 1)            ;; Turn on everywhere
(global-set-key (kbd "C-z") 'undo)   ;; Make ctrl-z undo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo) ;; Make ctrl-Z redo



;; Editing text

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "<C-delete>") 'delete-word)



;; Mouse bindings --------------------------------------------------------------

;; Mouse bindings
;; Make right-click do something close to what people expect
(global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)

;; When you click on a buffer, but you happen to hit the margin, you still want
;; to select it
(global-set-key (kbd "<right-margin> <down-mouse-1>") 'mouse-select-window)
(global-set-key (kbd "<left-margin> <down-mouse-1>") 'mouse-select-window)




;; Ivy -------------------------------------------------------------------------
;; Use it
(require 'ivy)
(require 'counsel)
(require 'swiper)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)

;; (global-set-key (kbd "C-f") 'counsel-grep-or-swiper)
;; Prefer isearch for now, again

(define-key isearch-mode-map (kbd "C-F") 'isearch-forward-symbol-at-point)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-forward-symbol-at-point)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)
        (swiper . ivy--regex-plus)))


(setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never -e '%s' %s")


(require 'smex)
;; (require 'flx)
;; (require 'company-flx)

;; (with-eval-after-load 'company
;;  (company-flx-mode +1))

;; (setq ivy-initial-inputs-alist "^")

;; (setq ivy-initial-inputs-alist
;;       '((org-refile . "^")
;;         (org-agenda-refile . "^")
;;         (org-capture-refile . "^")
;;         (counsel-M-x . "^")
;;         (counsel-describe-function . "^")
;;         (counsel-describe-variable . "^")
;;         (counsel-org-capture . "^")
;;         (counsel-find-file . "^")
;;         (Man-completion-table . "^")
;;         (woman . "^")))

;; (defun ivy--custom-basic (str)
;;   "Match things like in base-Emacs, bash, language-shells, etc. etc."
;;   (ivy--regex-plus (concat "^" str))
;;   )

;; (setq ivy-re-builders-alist '((t . ivy--custom-basic)))

;; General behavior ------------------------------------------------------------

;; Control-arrow over a whole word, e.g. from |long_word to long_word|, not
;; long|_word
(setq superword-mode 1)

;; Use smartparens
(add-hook 'prog-mode-hook #'smartparens-mode)
(require 'smartparens)
;; (add-hook 'ess-mode-hook #'rainbow-delimiters-mode)

;; Use rainbow delimiters when programming
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Load the new version of a file when it changes
(global-auto-revert-mode t)



;; Code editing ----------------------------------------------------------------

(require 'newcomment)

;; Activate auto-fill-mode globally for programming modes
 (add-hook 'prog-mode-hook 'turn-on-auto-fill)
;; But, make it so that it only runs when you're in a comment (e.g. not for
;; prose like markdown, and not for code)
(setq comment-auto-fill-only-comments 1)



;; Yasnippets ------------------------------------------------------------------

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode 1)

;; It seems like it's faster/easier to select snippets using good names, and to
;; keep tab free for auto-complete/company
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(global-set-key (kbd "C-'") 'yas-insert-snippet)

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
;; (setq-default cursor-type 'bar)



;; General text display --------------------------------------------------------

;; Make wrapping at the end of a line use whole words
(global-visual-line-mode t)



;; Fonts -----------------------------------------------------------------------
;; 10pt (in 1/10pt units)
(set-face-attribute 'default nil :height 110)



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

;; It's mad how much of a pain they are! For the time being, going without them
;;
;; (require 'linum)
;; (setq linum-format "%3d ")
;;(global-linum-mode 1)



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
;; (defadvice term-line-mode (after term-line-mode-fixes ())
;;   (set (make-local-variable 'cua-mode) t)
;;   (set (make-local-variable 'transient-mark-mode) t))
;; (ad-activate 'term-line-mode)


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
(setq scroll-margin 0)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; one line at a time
(setq scroll-preserve-screen-position 1)

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
;; ;; (add-hook 'markdown-mode-hook
;;           '(lambda ()
;;              (auto-complete-mode t)
;;              (local-unset-key [tab])
;;              (setq-local yas-fallback-behavior '(apply auto-complete))))

;; Allow math mode for stuff in-between $..$ or $$..$$
(setq markdown-enable-math t)

;; Use Olivetti mode whenever the mode loads (and disable linum-mode)
(require 'olivetti)
(add-hook 'markdown-mode-hook (lambda() (linum-mode -1)))
(add-hook 'markdown-mode-hook (lambda() (olivetti-mode t)))



;; Org-mode --------------------------------------------------------------------

(require 'org-habit)

;; TODO
;; Add some decent capture templates, especially for research / things to review
;;

;; Allow Control-Shift-Up to select wrapped paragraphs
(setq org-disputed-keys t)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "<M-prior>") 'org-todo)
            ;; (define-key org-mode-map (kbd "<C-S-up>") 'org-backward-paragraph)
            ;; (define-key org-mode-map (kbd "<C-S-down>") 'org-foreward-paragraph)
            ))


(add-hook 'org-mode-hook (lambda() (linum-mode -1)))
(add-hook 'org-mode-hook (lambda() (olivetti-mode t)))

;; In ediff, expand all the headings
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

(setq org-default-notes-file (concat "~/Nextcloud/org/inbox.org"))

;; Use the indented-mode / clean-view by default
(setq org-startup-indented t)

(setq org-agenda-files '("~/Nextcloud/org"))

;; Put state-changes into the :LOGBOOK: drawer
(setq org-log-into-drawer "LOGBOOK")

;; Don't force images to be full size in org buffers
(setq org-image-actual-width nil)

;; Hide slashes for italics etc.
(setq org-hide-emphasis-markers t)

;; When diffing org-mode files with ediff, expand everything
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

;; A little function for merging conflicts that arise via NextCloud

;; Notes/tips on ediff:
;; - There's an implicit cursor which moves between each diff, as well as a
;;   neutral 'start' and 'end'
;; - You move between them with space/delete
;; - Press B to copy changes from one to the other
;; - You can always directly edit a buffer

(setq ediff-split-window-function (quote split-window-horizontally))


;; Note: You can try and use something like this to customize the agenda. Tried
;; and aborted here quickly, but could be useul.
;; (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
;;          (timeline . "  % s")
;;          (todo .
;;                " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
;;          (tags .
;;                " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
;;          (search . " %i %-12:c"))
;;       )

;; Agenda-views  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; A custom function for marking things as done in the past (useful for tracking
;; habits). Taken from: https://emacs.stackexchange.com/a/9451
(defun org-todo-did-on-date (&optional arg)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (org-read-date t t nil "when:" nil nil nil))
            ((symbol-function #'org-current-effective-time)
             #'(lambda () my-current-time)))
    (org-todo arg)
    ))


;; Include archive files in the agenda
(setq org-agenda-archives-mode t)

;; Capture templates  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/Nextcloud/org/inbox.org" "Tasks")
        "* TODO %?\n  %i\n  %a")
   ("j" "Journal" entry (file+olp+datetree "~/Nextcloud/org/todo.org")
        "* %?\nEntered on %U\n  %i\n  %a")))


;; Time stamping  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Taken from https://github.com/sk8ingdom/.emacs.d/blob/9d2f40c261864533df59be452117941a7f9b3e3f/org-mode-config/org-todo-states.el

;; Record time and note when a task is completed
(setq org-log-done 'note)

;; Record time and note when the scheduled date of a task is modified
(setq org-log-reschedule 'note)

;; Record time and note when the deadline of a task is modified
(setq org-log-redeadline 'note)

;; Record time and note when clocking out of a task
(setq org-log-clock-out 'note)

;; Record time and note when a task is refiled
(setq org-log-refile 'note)

;; Log inserting a heading
(setq org-trest-insert-todo-heading-as-state-change t)


;; Agenda views  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; An agenda view without personal stuff in it

(setq org-agenda-custom-commands
      '(("w" "Work Stuff"
         ((agenda ""))
         ((org-agenda-tag-filter-preset '("-personal")))
         )
        ("p" "Personal Stuff"
         ((agenda ""))
         ((org-agenda-tag-filter-preset '("+personal")))
         ))
      )

;; Google Calendar integration - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Shouldn't cause a problem if the file is missing. Possibly best to make it
;; machine specific.
;;
;; Note: You're also running this every 30 mins to stay up-to-date, with a cron
;; job via /usr/bin/emacs --script ~/.emacs.d/init.el --eval "(org-gcal-fetch)"
;; (previously via /usr/bin/emacsclient --eval "(org-gcal-fetch)")
(load "~/.emacs.d/org-gcal.el" 'missing-ok nil)


;; Anki integration - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(setq anki-editor-create-decks t)

;; Little commands  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Pinched from https://github.com/alphapapa/unpackaged.el#ensure-blank-lines-between-headings-and-before-contents


(defun ap/org-fix-blank-lines (prefix)
  "Fix blank lines (or lack thereof) between entries and between planning-lines/drawers and entry contents in current subtree.
    With prefix, operate on whole buffer."
  (interactive "P")
  (save-excursion
    (when prefix
      (goto-char (point-min)))
    (when (org-before-first-heading-p)
      (outline-next-heading))
    (ap/org-fix-blank-lines-between-subtree-nodes)
    (ap/org-fix-blank-lines-after-headings)))

(defun ap/org-fix-blank-lines-between-subtree-nodes ()
  "Make sure each heading in subtree is separated from the previous heading's content by a blank line."
  (interactive)
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading))
    (ignore-errors
      ;; Try to work on the parent-level heading to fix all siblings
      ;; of current heading, but if we're at a top-level heading,
      ;; ignore the error.
      (outline-up-heading 1))
    (let ((m (point-marker)))
      (cl-flet ((fix nil (while (not (looking-back "\n\n"))
                           (insert-before-markers "\n"))))
        (org-map-entries #'fix t 'tree))
      ;; Inserting blank lines may move the point, depending on whether
      ;; it was at the beginning of a heading line or somewhere else.
      ;; Use the marker to make sure we are at the same position.
      (goto-char m)
      (org-with-wide-buffer
       ;; `org-map-entries' narrows the buffer, so `looking-back'
       ;; can't see newlines before the top heading, which may cause
       ;; extra newlines to be inserted.  Now we clean them up.
       (outline-back-to-heading)
       (while (looking-back (rx (>= 3 "\n")))
         (delete-char -1 nil)))
      (set-marker m nil))))

(defun ap/org-fix-blank-lines-after-headings ()
  "Make sure a blank line exists after a heading's drawers and planning lines, before the entry content."
  (interactive)
  (when (org-before-first-heading-p)
    (user-error "Before first heading."))
  (cl-flet ((fix nil (let ((end (org-entry-end-position)))
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers.  You might think that
                         ;; `org-at-drawer-p' would suffice, but for
                         ;; some reason it doesn't work correctly
                         ;; when operating on hidden text.  This
                         ;; works, taken from
                         ;; `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n")))))
    (org-map-entries #'fix t 'tree)))

;; Ediff -----------------------------------------------------------------------

;; Windows  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Make Ediff take you back to your old window configuration when you quit
;; Taken from: https://emacs.stackexchange.com/a/7486

(defvar ediff-last-windows nil
  "Last ediff window configuration.")

(defun ediff-restore-windows ()
  "Restore window configuration to `ediff-last-windows'."
  (set-window-configuration ediff-last-windows)
  (remove-hook 'ediff-after-quit-hook-internal
               'ediff-restore-windows))

(defadvice ediff-buffers (around ediff-restore-windows activate)
  (setq ediff-last-windows (current-window-configuration))
  (add-hook 'ediff-after-quit-hook-internal 'ediff-restore-windows)
  ad-do-it)


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
;;
;; Turns out you find these quite useful afterall
;;
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

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



;; General code editing --------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; (setq indent-line-function 'insert-tab)



;; SQL -------------------------------------------------------------------------

;; Indentation  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; (require 'sql-indent)

;; ;; As per https://github.com/alex-hhh/emacs-sql-indent/issues/43
;; ;;
;; ;; Note: You'd like to get a level of indentation after 'on' within a join (for
;; ;; long lists of conditions). Worth investigating in the future!
;; (defvar p43-sql-indentation-offsets-alist
;;   `((select-clause 0)
;;     (insert-clause 0)
;;     (delete-clause 0)
;;     (update-clause 0)
;;     (select-join-condition 0)
;;     (in-select-clause + sqlind-lineup-close-paren-to-open-indentation)
;;     (select-table-continuation + sqlind-lineup-close-paren-to-open-indentation)
;;     ,@sqlind-default-indentation-offsets-alist))

;; (add-hook 'sqlind-minor-mode-hook
;;           (lambda ()
;;             (setq sqlind-indentation-offsets-alist
;;                   p43-sql-indentation-offsets-alist)))

;; ;; Automatically use the sqlind minor mode when editing SQL files
;; (setq sql-mode-hook (quote (sqlind-minor-mode)))

(require 'sql)

;; Use sqlite3 (the standard on Ubuntu, it seems)
(setq sql-sqlite-program "sqlite3")

;; Make C-RET send code to REPL
(define-key sql-mode-map (kbd "<C-return>") 'sql-send-region)
;; Don't focus the REPL after sending SQL to it
(setq sql-pop-to-buffer-after-send-region nil)




;; Web stuff -------------------------------------------------------------------

;; Note: For HTML and CSS, you could use web-mode (spacemacs does, for example),
;; but using independent modes for now, mainly for testing out skewer
(require 'js2-mode)
(require 'skewer-mode)
(require 'simple-httpd)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;; set root folder for httpd server
(setq httpd-root "~/projects/forecast_site")


;; Bash scripts / terminal repl ------------------------------------------------

(require 'essh) ; if not done elsewhere; essh is in the local lisp folder
(require 'eval-in-repl)
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          '(lambda()
             (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))



;; Python ----------------------------------------------------------------------

(require 'elpy)

(package-initialize)
(elpy-enable)

;; Use a standard REPL
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

(setq elpy-rpc-python-command "python3")

;; ;; Use Jupyter
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt")

;; ;; Use iPython
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")


;; Use 79 chars for width (PEP-8)
(add-hook 'elpy-mode-hook
          (lambda ()
            (set-fill-column 79)))



;; (with-eval-after-load "python"
;;   ;; try to get indent/completion working nicely
;;   ;; (setq python-indent-trigger-commands '(my-company-indent-or-complete-common
;;   ;;                                        indent-for-tab-command
;;   ;;                                        ;;yas-expand
;;   ;;                                        ;;yas/expand
;;   ;;                                        ))

;;   ;; readline support is wonky at the moment
;;   (setq python-shell-completion-native-enable nil)
;;   ;; simple evaluation with C-ret
;;   (require 'eval-in-repl-python)
;;   (define-key python-mode-map "\C-c\C-c" 'eir-eval-in-python)
;;   (define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python))

;; (add-hook 'inferior-python-mode-hook (lambda () (linum-mode -1)))

;; ;; Use Python 3 via Anaconda -- this doesn't work
;; (setq python-python-command "$HOME/anaconda3/bin/python")

;; This should make the virtual env stuff work in a repl
;; (require 'virtualenvwrapper)
;; (venv-initialize-interactive-shells)



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
  ;; (define-key emacs-lisp-mode-map (kbd "<C-S-return>") 'eval-buffer)
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



;; Node / JS -------------------------------------------------------------------

(require 'nodejs-repl)

(add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "<C-return>") 'nodejs-repl-send-region)))



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

(defun rmd-fold-block ()
  "Fold the contents of the current R block, in an Rmarkdown file (can be undone
   with fold-this-unfold-at-point)"
  (interactive)
  (and (eq (oref pm/chunkmode :mode) 'r-mode)
       (pm-with-narrowed-to-span nil
                                 (goto-char (point-min))
                                 (forward-line)
                                 (fold-this (point) (point-max)))))

(defun rmd-unfold-all-blocks ()
  fold-this-unfold-all)


;; E.g. to send a prefix, use C-u M-x rmd-send-buffer
(defun rmd-fold-all-blocks (arg)
  "Fold all R blocks in an Rmarkdown file (can be undone with
   fold-this-unfold-all)"
  ;; Interactive, with a prefix argument
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      (pm-map-over-spans
       'rmd-fold-block (point-min)
       ;; adjust this point to fold prior regions
       (if arg (point) (point-max))))))


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



(defun rmd-plot-chunk (header)
  "Insert an r-chunk in markdown mode. Necessary due to interactions between
polymode and yas snippet"
  (interactive "sHeader: ")
  (insert (concat "```{r " header ", fig.cap='(ref:" header
                  ")'}\n\n\n\n```\n\n(ref:" header ") Plot caption here\n\n"))
  (forward-line -6))



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
;; Note: If Stan isn't installed, this seems to break verything
(require 'stan-snippets)



;; Polymode --------------------------------------------------------------------

;; (require 'polymode)
;; (require 'poly-markdown)
(require 'poly-R)

;;; Markdown
(add-to-list 'auto-mode-alist '("\\.md"   . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+R-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+R-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+R-mode))


;; Big Rmarkdown buffers get slow af. See if this helps.
(add-hook 'poly-markdown+r-mode-hook (lambda() (linum-mode -1)))
(add-hook 'poly-markdown+r-mode-hook (lambda() (olivetti-mode t)))



;; Git -------------------------------------------------------------------------

(require 'magit)

;; Note, this taken from https://emacs.stackexchange.com/a/32610
;; Owing to a problem you were experiencing with wrapping commit messages,
;; caused by comment-auto-fill-only-comments, set to t, above
(use-package git-commit
             :ensure nil
             :preface
             (defun me/git-commit-set-fill-column ()
               (setq-local comment-auto-fill-only-comments nil)
               (setq fill-column 72))
             :config
             (advice-add 'git-commit-turn-on-auto-fill :before #'me/git-commit-set-fill-column))


;; Set key for magit-status
(global-set-key (kbd "C-x C-g") 'magit-status)


;; Try out magithub
;;
;; This may cause errors without an update of magit. It *shouldn't* do anything
;; unless it can auth, so adding this line *should* be harmless on machines where
;; you don't have an interest in GH stuff.

;; (require 'magithub)
;; (magithub-feature-autoinject t)

;; Misc ------------------------------------------------------------------------

;; Run emacs as a server with clients
(setq server-use-tcp t)
(server-start)

;; Strip trailing whitespace from files on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)



;; Mail ------------------------------------------------------------------------

(if (load "~/.emacs.d/mail.el" 'missing-ok nil)
    ;; If there is a file called mail.el (where you should keep machine
    ;; specific, non committed email configs), then assume that mu4e is
    ;; installed, and run the following
    (progn
      ;; Note: Tested with mu v0.9.18 @1f232b6 and Emacs 25.2
      (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

      ;; make sure mu4e is in your load-path
      (require 'mu4e)

      ;; If you get your mail without an explicit command,
      ;; use "true" for the command (this is the default)
      (setq mu4e-get-mail-command "offlineimap")

      ;; Use format=flowed
      (setq mu4e-compose-format-flowed t)

      ;; Use olivetti mode to make things look nice and wrap lines
      (add-hook 'mu4e-compose-mode-hook
                (lambda() (olivetti-mode t)))

      (add-hook 'mu4e-view-mode-hook
                (lambda() (olivetti-mode t)))

      (setq message-send-mail-function 'message-send-mail-with-sendmail)
      (setq mu4e-view-html-plaintext-ratio-heuristic 1000)

      ;; This could make HTML mail less horrible
      (setq mu4e-html2text-command "html2text -utf8 -style pretty -width 72")

      ;; Only needed if your maildir is _not_ ~/Maildir
      ;; Must be a real dir, not a symlink
      ;; (setq mu4e-maildir "/home/user/Maildir")

      ;; these must start with a "/", and must exist
      ;; (i.e.. /home/user/Maildir/sent must exist)
      ;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
      ;; already exist

      ;; below are the defaults; if they do not exist yet, mu4e offers to
      ;; create them. they can also functions; see their docstrings.
      (setq mu4e-sent-folder   "/Sent")
      (setq mu4e-drafts-folder "/Drafts")
      (setq mu4e-trash-folder  "/Trash")

      ;; Load the mail file again to overwrite any settings for local use
      (load "~/.emacs.d/mail.el")

      )
  (ding)
  )



;; Databases -------------------------------------------------------------------

;; Put mail stuff in another config, load it in if you actually use it!
(load "~/.emacs.d/databases.el" 'missing-ok nil)



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



;; i3 Integration --------------------------------------------------------------

;; This is pretty simple; if it looks like you're using i3, prefer popping
;; frames as opposed to splitting windows. Doesn't work for everything
;; (e.g. iELM), but works well enough most of the time. Assumes that wmctrl is
;; installed, which it isn't by default, but it's only apt get away
(when (string-match "i3" (shell-command-to-string "wmctrl -m"))
  (progn
    (require 'frames-only-mode)
    (frames-only-mode t)
    ))



;; Sonic pi stuff --------------------------------------------------------------
(require 'sonic-pi)
(setq sonic-pi-path "/usr/lib/sonic-pi/")

;; Optionally define a hook
(add-hook 'sonic-pi-mode-hook
          (lambda ()
            ;; This setq can go here instead if you wish
            (setq sonic-pi-path "/usr/lib/sonic-pi/")
            (define-key ruby-mode-map (kbd "<C-return>") 'sonic-pi-send-region)
            (define-key ruby-mode-map "C-c C-b" 'sonic-pi-stop-all))
          )


;; ranger-mode -----------------------------------------------------------------

(setq ranger-cleanup-eagerly t)



;; Reviewing papers ------------------------------------------------------------
;;
;; Experimenting with the system described at:
;; https://codearsonist.com/reading-for-programmers

(require 'pdf-tools)
(require 'org-ref)
(require 'ivy-bibtex)
(require 'interleave)


(setq reading-list-file (file-truename "~/Nextcloud/org/reading-list.org"))

;; pdf-tools - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; apparently nothing required. This is a replacement for docview
;; (the built-in emacs pdf viewer)


;; org-ref - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; For some reason you get an error about this, which goes away if you set it,
;; as per https://github.com/jkitchin/org-ref/blob/master/org-ref.org
(setq org-latex-prefer-user-labels t)

;; Use this to make references to bibtex entries in documents that you're
;; writing. It can also do loads of other cool shit, like extract references
;; from pdfs which you click and drag into the .bib file. Sometimes it can even
;; download the pdf! A demo here: https://www.youtube.com/watch?v=2t925KRBbFc
(require 'org-ref)

(setq org-ref-notes-directory "~/Nextcloud/org/"
      org-ref-bibliography-notes reading-list-file
      org-ref-default-bibliography '("~/Nextcloud/org/index.bib")
      org-ref-pdf-directory "~/Nextcloud/papers/")

;; ivy-bibtex - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Use this to track down local bibtex references, and e.g. pull up the paper

;; If you're looking to find information on a paper that you don't have locally,
;; then this can be used to find one via biblio.el, but it's faster to just call
;; that directly and search

;; This is cool for searching local stuff (assuming it exists). Searching
;; remotely is a bit clunky.  You can do this in a semi manual fashion with M-x
;; ivy-bibtex, M-o f, Crossref
(setq
 ivy-bibtex-bibliography "~/Nextcloud/org/index.bib" ;; where your references are stored
 ivy-bibtex-library-path "~/Nextcloud/papers/" ;; where your pdfs etc are stored
 ivy-bibtex-notes-path "~/Nextcloud/org/index.org" ;; where your notes are stored
 bibtex-completion-bibliography "~/Nextcloud/org/index.bib" ;; writing completion
 bibtex-completion-notes-path reading-list-file
 ivy-re-builders-alist
 '((ivy-bibtex . ivy--regex-ignore-order)
   (t . ivy--regex-plus))
 )


;; interleave  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; It would be cool if you could figure out how to make this use separate frames


;; biblio.el - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


;; Get a list of the exit code and the output from the function

;; Things you want to happen:
;;
;; 1. Download the paper
;; 2. Add a bibtex entry (you could extract this from biblio, or, you could
;;    possibly obtain it from org-ref)
;;    You should use this (write-region <STRING> nil <FILENAME> 'append)
;; 3. Add an entry in the org-file

;; 4. Open it in interleave mode

(defun add-to-reading-list (title file)
    "Based on the title of a paper, and a path to it's PDF, create a reading list
  entry"
    ;; Come up with the string that you want to add to your reading-list
    (setq org-reading-list-entry
          (format
           "\n\n* %s\n  :PROPERTIES:\n  :INTERLEAVE_PDF: %s\n  :END:\n\n"
           title file))

    ;; Append that string to the end of your reading-list
    (write-region org-reading-list-entry nil reading-list-file 'append)
    ;; Note: it might be a better idea to open the file in a buffer, and then
    ;; append the text to the end, as you want to move
    )


(defun get-paper-async (title doi)
  "
Shell out the to command get-from-doi. If it works, return the shell output as a
string, otherwise, error out

This works, but you can't have it return the filename without waiting on
it. This means that you need to write to a temporary buffer somewhere, and then
do something when that worked. Or, just do everything inside this async
function.

"
  (async-start
   ;; The thing to be done asynchronously
   (lambda ()
      (with-temp-buffer
        (list (call-process
               (file-truename "~/projects/dotfiles/bin/get-from-doi")
               nil
               (current-buffer)
               nil
               "10.2307/3149462"
               (file-truename "~/Nextcloud/papers/"))
              (buffer-string)
              "hello"
              )
        )
      )

   ;; The thing to do when that thing is done
   (lambda (result)

     (setq exit-code (pop result))
     (setq file-name (pop result))
     (setq other-thing (pop result))

     (if (eq exit-code 0)
         (progn
           (message "Downloaded paper '%s'  DOI: %s" file-name other-thing)
           (message "Downloaded paper '%s'  DOI: %s" title doi)
           (add-to-reading-list title file-name)
           Here, you'd insert something to open the file
           (message "Finnished! %s" file-name)
           )
       ;;(error "Couldn't find a PDF for that DOI")
       (error "Failed to download paper '%s'  DOI: %s" title doi)
       )
     )
   )
  )


(defun biblio-get-paper--lookup-record (record)
  "Retrieve a RECORD from Dissemin, and display it.
RECORD is a formatted record as expected by `biblio-insert-result'."
  (let-alist record
    (if .doi
        (progn
          (message "Attempting to download paper '%s'  DOI: %s" .title .doi)
          (get-paper-async .title .doi)
          )
      (user-error "Need both a DOI and a title, which don't appear to be present"))))

(defun biblio-get-paper--register-action ()
  "Add Dissemin to list of `biblio-selection-mode' actions."
  (add-to-list 'biblio-selection-mode-actions-alist
               '("Download and add to reading list" . biblio-get-paper--lookup-record)))


(add-hook 'biblio-selection-mode-hook #'biblio-get-paper--register-action)



;; Elfeed ----------------------------------------------------------------------

(require 'cl-lib)
(require 'elfeed)
(require 'youtube-dl)

(load "~/.emacs.d/lisp/youtube-dl-emacs/youtube-dl.el" 'missing-ok nil)

(global-set-key (kbd "C-x w") 'elfeed)


;; ;; Stop lines wrapping in search mode
;; (add-hook 'elfeed-search-mode-hook
;;                (lambda() (visual-line-mode t)))


;; Read entries in olivetti mode
(add-hook 'elfeed-show-mode-hook
                (lambda() (olivetti-mode t)))

;; You can use
;; https://www.youtube.com/feeds/videos.xml?playlist_id=xxx
;; or
;; https://www.youtube.com/feeds/videos.xml?channel_id=xxx
(setq elfeed-feeds
      '(
        ;; 'Magazine' stuff ----------------------------------------------------
        ;;
        ;; Mike Zaminsky / Using Emacs
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxkMDXQ5qzYOgXPRnOBrp1w" yt emacs hacking entertainment)
        ;; Noisey
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0iwHRFpv2_fpojZgQhElEQ" yt music entertainment)
        ;; iD Magazine
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9ISPZsMaBi5mutsgX6LC1g" yt music entertainment)
        ;; Motherboard
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCB6PV0cvJpzlcXRG7nz6PpQ" yt hacking entertainment)
        ;; Thump
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCS6R2iiAJ1FvEYl4B3zmljw" yt music entertainment)
        ;; The Fader
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRCOCvfOkoqneyQCbNOUPwg" yt music entertainment)
        ;; KODX Seattle
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC4YGLMPVaCdMFiIJL9_Pq2A" yt interviews entertainment)
        ;;
        ;;
        ;; Data Science Stuff --------------------------------------------------
        ;;
        ;; Brian Caffo!
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdjFpvS8lvT2MJVthOUvlyg" yt work)
        ;; A man called Derek Kane, never heard-of-him/watched-these before
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC33qFpcu7eHFtpZ6dp3FFXw" yt work)
        ;; PyData
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCOjD18EJYcsBog4IozkF_7w" yt work)
        ;; Harvard's Stat 110 class. Mainly the guts of parametric
        ;; distributions. YT RSS limited to first 15, but there's 15 more
        ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL2SOU6wwxB0uwwH80KTQ6ht66KWxbzTIo" yt work)
        ;; O'Reilly Strata blah blah 2018
        ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL055Epbe6d5a_38V9wp-aru0XQ34dgqCA" yt work)
        ;; Spark Summit
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRzsq7k4-kT-h3TDUBQ82-w" yt work)
        ;; InfoQ conferences: "AI", ML, data engineering
        ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLndbWGuLoHeYsZk6VpCEj_SSd9IFgjJ-2" yt work)
        ;; Uber AI labs
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCOb_oiEfSedawuvRA0oaVoQ" yt work)
        ;;
        ;;
        ;; Data Science Stuff, but more 'YouTubey' -----------------------------
        ;;
        ;; Mainly copied from sites with lists when googling 'data science
        ;; youtube'. Possibly not good.
        ;;
        ;; Dan Van Boxel
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC6tnRFKGiq1DlybcqP5rZ7A" yt work)
        ;; Hvass Labs
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbba38V6vcglqVL--8kVVmg" yt work)
        ;; sentdex
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCfzlCWGWYyIQ0aLC5w48gBQ" yt work)
        ;; The SemiColon (more tutorial crap)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCwB7HrnRlOfasrbCJoiZ9Lg" yt work)
        ;;
        ;; News ----------------------------------------------------------------
        ;;
        ;; UK Parliament, PMQs only
        ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL40441042C458B62B" yt news)
        ;; Democracy now
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCzuqE7-t13O4NIDYJfakrhw" yt news)
        ;; Channel 4 news
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCTrQ7HXWRRxr7OsOtodr2_w" yt news)
        ;;
        ;;
        ;; Games & Gaming ------------------------------------------------------
        ;;
        ;; Waypoint
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCWu9QuHF-dcakBmhullIH6w" yt gaming)
        ;; G4 TV!
        ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLb9M3FeiZ1K74ni3oo-JmCAAnChPwQCdo" yt gaming)
        ;;
        ;; Music ---------------------------------------------------------------
        ;;
        ;; Ninja Tune
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCEXRv_qihRwjsV91ftx23-A" yt music)
        ;; Warp Records
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCvbd4lVoe8ur0zJJRuuhC_Q" yt music)
        ;; Boilerroom
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCGBpxWJr9FNOcFYA5GkKrMg" yt music live)
        ;; Alias Bass
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3SZ8dE0Mg_7GdsarM_CTOg" yt music)
        ))



;; youtube-dl config (yoinked straight from
;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el)

(setq youtube-dl-directory "~/Videos")

(defface elfeed-youtube
  '((t :foreground "#f9f"))
  "Marks YouTube videos in Elfeed."
  :group 'elfeed)

(push '(youtube elfeed-youtube)
      elfeed-search-face-alist)

(defun elfeed-show-youtube-dl ()
  "Download the current entry with youtube-dl."
  (interactive)
  (pop-to-buffer (youtube-dl (elfeed-entry-link elfeed-show-entry))))

(cl-defun elfeed-search-youtube-dl (&key slow)
  "Download the current entry with youtube-dl."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (dolist (entry entries)

      (setq vid-file (elfeed-youtube-dl-filename-sanizer
                      (elfeed-entry-title entry)))

      (setq vid-dir (concat
                     (file-name-as-directory youtube-dl-directory)
                     (elfeed-feed-title (elfeed-entry-feed entry))))

      (if (null (youtube-dl (elfeed-entry-link entry)
                            :title (elfeed-entry-title entry)
                            :slow slow
                            :destination vid-file
                            :directory vid-dir
                            :autoplay t
                            )
                ;; Note: You're not really how to do the queuing
                ;; thing... Possibly a hook in ytdl, or just watch the process,
                ;; or just watch for the file to appear..?
                ;;
                ;; Check out youtube-dl--sentinel
                ;;
                ;; Could you store whether it should be played or not
                ;;
                ;; (vlc/enqueue (concat (filename-as-directory vid-dir) vid-name))
                )
          (message "Entry is not a YouTube link!")
        (message "Downloading %s" (elfeed-entry-title entry)))
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (unless (use-region-p) (forward-line)))))


(defun expose (function &rest args)
  "Return an interactive version of FUNCTION, 'exposing' it to the user."
  (lambda ()
    (interactive)
    (apply function args)))

(defalias 'elfeed-search-youtube-dl-slow
  (expose #'elfeed-search-youtube-dl :slow t))

(define-key elfeed-show-mode-map "d" 'elfeed-show-youtube-dl)
(define-key elfeed-search-mode-map "d" 'elfeed-search-youtube-dl)
(define-key elfeed-search-mode-map "D" 'elfeed-search-youtube-dl-slow)
(define-key elfeed-search-mode-map "L" 'youtube-dl-list)


(defun elfeed-youtube-dl-filename-sanizer(filename)
  "Produce a sanitized filename for downloaded youtube-dl videos, using a simple
  regex convention determined in elisp (as opposed to a sophisticated one
  determined in Python).

  The reasoning is that you'd like a simple string that you can pass to VLC once
  the thing is donwloaded"
  (concat
   (replace-regexp-in-string
    "_+" "_"
    (replace-regexp-in-string "[[:space:][:punct:]]" "_" filename)
    )
   ".mp4"
   )
  )



;; VLC ------------------------------------------------------------------------

;; So, now you've downloaded all this crap to watch, you have to watch it

(require 'vlc)


;; Appearance ------------------------------------------------------------------

;; Do this last so you get immediate visual feedback if something breaks
;; Make emacs transparent
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
;;(set-frame-parameter (selected-frame) 'alpha '(99 . 98))
;; (add-to-list 'default-frame-alist '(alpha . (99 . 98)))


;; Your theme
(require 'sanityinc-tomorrow-eighties-theme)
(set-face-attribute 'fringe nil :background "#2d2d2d" :foreground "#2d2d2d")

;;(require 'sanityinc-tomorrow-bright-theme)

;; Hipster modeline
;; Note: This uses up a notable amount of CPU on your little x200t!
;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)


;; Use Emacs terminfo, not system terminfo
;; http://stackoverflow.com/a/8920373
(setq system-uses-terminfo nil)
(put 'downcase-region 'disabled nil)
