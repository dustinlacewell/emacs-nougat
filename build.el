(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(use-package git)

(defun fix-org-git-version ()
  "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))


(defun fix-org-release ()
  "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(defun to-elisp (file-name &optional keep-export)

  (interactive)

  (use-package org
    :mode ("\\.org\\'" . org-mode)
    :config
    ;; This forces straight to load the package immediately in an attempt to avoid the
    ;; Org that ships with Emacs.
    (require 'org)
    (require 'ox-org)
    (defalias #'org-git-version #'fix-org-git-version)
    (defalias #'org-release #'fix-org-release)

    ;; Auto-fill paragraphs
    (add-hook 'org-mode-hook 'turn-on-auto-fill))

  (let* ((outline-file-name (expand-file-name file-name))
         (base-file-name (file-name-sans-extension outline-file-name))
         (output-file-name (format "%s.export.org" base-file-name))
         (elisp-file-name (format "%s.el" base-file-name)))
    (find-file outline-file-name)
    (org-export-to-file 'org output-file-name)
    (org-babel-tangle-file output-file-name elisp-file-name)
    (unless keep-export (delete-file output-file-name))))

;; TODO refactor these ^ v

(defun to-html (file-name &optional keep-export)

  (interactive)
  (use-package htmlize)
  (use-package org
    :mode ("\\.org\\'" . org-mode)
    :config
    ;; This forces straight to load the package immediately in an attempt to avoid the
    ;; Org that ships with Emacs.
    (require 'org)
    (require 'ox-org)
    (defalias #'org-git-version #'fix-org-git-version)
    (defalias #'org-release #'fix-org-release)

    ;; Auto-fill paragraphs
    (add-hook 'org-mode-hook 'turn-on-auto-fill))

  (let* ((outline-file-name (expand-file-name file-name))
         (base-file-name (file-name-sans-extension outline-file-name))
         (output-file-name (format "%s.export.org" base-file-name))
         (html-file-name (format "%s.html" base-file-name)))
    (find-file outline-file-name)
    (org-export-to-file 'org output-file-name)
    (find-file output-file-name)
    (org-export-to-file 'html html-file-name)
    (unless keep-export (delete-file output-file-name))))
