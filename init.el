(require 'org)
(require 'ob-tangle)

(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))
