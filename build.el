;; bootstrap straight.el
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

;; straight.el org fix boilerplate
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

;; export boilerplate
(defun destination-file-name-from-source (source ext)
  (let* ((expanded (expand-file-name source))
         (sans (file-name-sans-extension expanded))
         (file-name (format "%s.%s" sans ext)))
    file-name))

(defun render-file-name-from-source (source)
  (let* ((expanded (expand-file-name source))
         (sans (file-name-sans-extension expanded))
         (extension (file-name-extension source))
         (file-name (format "%s.rendered.%s" sans extension)))
    file-name))

(defun install-org ()
  (use-package org
    :mode ("\\.org\\'" . org-mode)
    :config
    ;; This forces straight to load the package immediately in an attempt to avoid
    ;; the Org that ships with Emacs.
    (require 'org)
    (require 'ox-org)
    (use-package htmlize
      :config (require 'htmlize))
    (defalias #'org-git-version #'fix-org-git-version)
    (defalias #'org-release #'fix-org-release)))

(defun export (source ext cont &optional destination keep-export)
  (install-org)
  (let* ((source (expand-file-name source))
         (render (render-file-name-from-source source))
         (destination (or destination
                          (destination-file-name-from-source source ext))))
    (find-file source)
    (funcall cont source render destination)
    (when (and (file-exists-p render) (not keep-export))
      (delete-file render))))

(defun do-elisp (source render destination)
  (message "source: %s" source)
  (message "-> %s" (org-export-to-file 'org render))
  (message "-> %s" destination)
  (rename-file (car (org-babel-tangle-file render nil 'emacs-lisp)) destination))

(defun do-html (source render destination)
  (org-export-to-file 'org render)
  (find-file render)
  (org-export-to-file 'html destination))

(defun to-elisp (source &optional destination keep-export)
  (interactive)
  (export source "el" 'do-elisp destination keep-export))

(defun to-html (source &optional destination keep-export)
  (interactive)
  (export source "html" 'do-html destination keep-export))
