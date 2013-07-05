;;; packed.el --- package manager agnostic Emacs Lisp package utilities

;; Copyright (C) 2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120624
;; Version: 0.3.1
;; Status: beta
;; Homepage: http://tarsius.github.com/packed
;; Keywords: compile, convenience, lisp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Packed provides some package manager agnostic utilities to work
;; with Emacs Lisp packages.  As far as Packed is concerned packages
;; are collections of Emacs Lisp libraries that are stored in a
;; dedicated directory such as a vcs repository.  And libraries are
;; Emacs Lisp files that provide the correct feature (matching the
;; filename).

;; Where a package manager might depend on metadata, Packed instead
;; uses some heuristics to get the same information -- that is slower
;; and might also fail at times but avoids having to create the
;; metadata in the first place.

;;; Code:

(require 'bytecomp)

(eval-when-compile
  (require 'cl)) ; case, push

(declare-function autoload-rubric "autoload")
(declare-function autoload-find-destination "autoload")
(declare-function autoload-file-load-name "autoload")


;;; Options.

(defgroup packed nil
  "Emacs package utilities."
  :group 'convenience
  :prefix 'packed)

(defcustom packed-loaddefs-filename "loaddefs.el"
  "Name of the files used to store extracted autoload definitions."
  :group 'packed
  :type 'file)


;;; Libraries.

(defun packed-el-suffixes (&optional nosuffix must-suffix)
  "Return a list of the valid suffixes of Emacs Lisp source libraries.
Unlike `get-load-suffixes' don't return the suffixes for
byte-compile destinations just those of source files.

If NOSUFFIX is non-nil the `.el' part is omitted.  IF MUST-SUFFIX
is non-nil all returned suffixes contain `.el'.  This uses the
variables `load-suffixes' (from which it removes \".elc\") and
`load-file-rep-suffixes'."
  (append (unless nosuffix
            (let ((load-suffixes (remove ".elc" load-suffixes)))
              (get-load-suffixes)))
          (unless must-suffix
            load-file-rep-suffixes)))

(defun packed-el-regexp ()
  "Return the valid suffixes of Emacs libraries as a regular expression."
  (concat (regexp-opt (packed-el-suffixes nil t)) "\\'"))

(defun packed-source-file (elc)
  "Return the Emacs source file for byte-compile destination ELC."
  (let ((standard (concat (file-name-sans-extension
                           (file-name-sans-extension elc)) ".el"))
        (suffixes (remove ".el" (packed-el-suffixes)))
        file)
    (while (and (not file) suffixes)
      (unless (file-exists-p (setq file (concat standard (pop suffixes))))
        (setq file nil)))
    (or file standard)))

(defun packed-locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
Unlike `locate-library' don't return the byte-compile destination
if it exists but always the Emacs source file.

LIBRARY should be a relative file name of the library, a string.
It can omit the suffix (a.k.a. file-name extension) if NOSUFFIX is
nil (which is the default, see below).
This command searches the directories in `load-path' like `\\[load-library]'
to find the file that `\\[load-library] RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `load-suffixes'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'.

When called from a program, the file name is normally returned as a
string.  When run interactively, the argument INTERACTIVE-CALL is t,
and the file name is displayed in the echo area."
  (interactive (list (completing-read "Locate library: "
                                      (apply-partially
                                       'locate-file-completion-table
                                       load-path (get-load-suffixes)))
                     nil nil
                     t))
  (let ((file (locate-file (substitute-in-file-name library)
                           (or path load-path)
                           (packed-el-suffixes nosuffix))))
    (if interactive-call
        (if file
            (message "Library is file %s" (abbreviate-file-name file))
          (message "No library %s in search path" library)))
    file))

(defvar packed-ignore-library-regexp
  (regexp-opt (list "^t$" "test")))

(defvar packed-ignore-directory-regexp
  (regexp-opt (list "^t$" "test")))

(defun packed-ignore-directory-p (directory &optional package)
  "Whether DIRECTORY should be ignored based on it's filename.
Return t if DIRECTORY's filename matches `packed-ignore-directory-regexp'.
If optional PACKAGE also matches that regular expression also then don't
ignore the directory."
  (let ((filename (packed-filename directory)))
    (and (not (member filename '("RCS" "CVS")))
         packed-ignore-directory-regexp
         (string-match packed-ignore-directory-regexp filename)
         (or (not package)
             (not (string-match packed-ignore-directory-regexp package))))))

(defmacro packed-with-file (file &rest body)
  "Execute BODY in a buffer containing the contents of FILE.
If FILE is nil or equal to `buffer-file-name' execute BODY in the
current buffer.  Move to beginning of buffer before executing BODY.
FILE should be an Emacs lisp source file."
  (declare (indent 1) (debug t))
  (let ((filesym (make-symbol "--file--")))
    `(let ((,filesym ,file))
       (save-match-data
         (save-excursion
           (if (and ,filesym (not (equal ,filesym buffer-file-name)))
               (with-temp-buffer
                 (insert-file-contents ,filesym)
                 (with-syntax-table emacs-lisp-mode-syntax-table
                   ,@body))
             (goto-char (point-min))
             (with-syntax-table emacs-lisp-mode-syntax-table
               ,@body)))))))

(defun packed-library-p (file &optional package)
  "Return non-nil if FILE is an Emacs source library.
Actually provide the feature provided by FILE (which has to match
it's filename)."
  (let ((name (file-name-nondirectory file)))
    (save-match-data
      (and (string-match (packed-el-regexp) name)
           (not (or (file-symlink-p file)
                    (string-match "^\\." name)
                    (string-equal name dir-locals-file)
                    (auto-save-file-name-p name)
                    (if package
                        (string-equal name (concat package "-pkg.el"))
                      (string-match "-pkg\\.el$" name))
                    (and packed-ignore-library-regexp
                         (string-match packed-ignore-library-regexp
                                       (file-name-nondirectory file))
                         (or (not package)
                             (not (string-match
                                   packed-ignore-library-regexp
                                   package))))))
           (packed-library-feature file)))))

(defun packed-libraries (directory
                         &optional package full relaxed nonrecursive)
  "Return a list of libraries in the package directory DIRECTORY.
DIRECTORY is assumed to contain the libraries belonging to a
single package.  Some assumptions are made about what directories
and what files should be considered.

If optional FULL is non-nil return absolute paths otherwise paths
relative to DIRECTORY.  If optional RELAXED is non-nil include
Emacs lisp files that don't provide a feature or only features
that don't match the filename."
  ;; avoid cl blasphemy
  (let (libraries)
    (dolist (elt (packed-libraries-1
                  directory (or package (packed-filename directory))
                  nonrecursive))
      (when (or relaxed (cdr elt))
        (setq libraries (cons (if full
                                  (car elt)
                                (file-relative-name (car elt) directory))
                              libraries))))
    libraries))

(defun packed-libraries-1 (directory &optional package nonrecursive)
  "Return a list of Emacs lisp files in the package directory DIRECTORY.
DIRECTORY is assumed to contain the libraries belonging to a
single package.  Some assumptions are made about what directories
and what files should be considered.

The return value has the form ((LIBRARY . FEATURE)...).  FEATURE
is nil if LIBRARY does not provide a feature or only features
that don't match the filename."
  (let (libraries)
    (dolist (f (directory-files directory t "^[^.]"))
      (cond ((file-directory-p f)
             (or nonrecursive
                 (file-exists-p (expand-file-name ".nosearch" f))
                 (packed-ignore-directory-p f package)
                 (setq libraries (nconc (packed-libraries-1 f package)
                                        libraries))))
            ((string-match (packed-el-regexp)
                           (file-name-nondirectory f))
             (push (cons f (packed-library-p f package)) libraries))))
    (nreverse libraries)))

(defun packed-main-library (directory &optional package noerror nosingle)
  "Return the main library from the package directory DIRECTORY.
Optional PACKAGE is the name of the package; if it is nil the
basename of DIRECTORY is used as the package name.

Return the library whose basename matches the package name.  If
that fails append \"-mode\" to the package name, respectively
remove that substring, and try again.  That library must also
provide the correct feature.

Unless optional NOSINGLE is non-nil and if there is only a single
Emacs lisp file return that even if it doesn't match the package
name.

If the main library cannot be found raise an error or if optional
NOERROR is non-nil return nil."
  (packed-main-library-1 (or package (packed-filename directory))
                         (packed-libraries-1 directory package)
                         noerror nosingle))

(defun packed-main-library-1 (package libraries &optional noerror nosingle)
  "Return the main library among LIBRARIES of the package PACKAGE.
PACKAGE is a package name, a string.  LIBRARIES is a list of full
library filenames or an alist as returned by `packed-libraries-1'.
In the latter case also ensure that the main library provides the
correct feature.

Return the library whose basename matches the package name.  If
that fails append \"-mode\" to the package name, respectively
remove that substring, and try again.

Unless optional NOSINGLE is non-nil and if there is only a single
library return that even if it doesn't match the package name.

If no library matches raise an error or if optional NOERROR is
non-nil return nil."
  (let ((match
         (cond ((and (not nosingle)
                     (not (cdr libraries)))
                (car libraries))
               ((packed-main-library-2 package libraries))
               ((packed-main-library-2
                 (if (string-match "-mode$" package)
                     (substring package 0 -5)
                   (concat package "-mode"))
                 libraries)))))
    (cond ((and (not match)
                (not noerror))
           (error "Cannot determine main library of %s" package))
          ((atom match)
           match)
          ((cdr match)
           (car match))
          ((not noerror)
           (error "Main library %s provides no or wrong feature"
                  (car match))))))

(defun packed-main-library-2 (package libraries)
  ;; avoid cl blasphemy
  (let ((regexp (concat "^" (regexp-quote package) (packed-el-regexp) "$")))
    (catch 'found
      (dolist (lib libraries)
        (when (string-match regexp (file-name-nondirectory
                                    (if (consp lib) (car lib) lib)))
          (throw 'found lib))))))

(defun packed-filename (file)
  "Return the filename (aka basename) of FILE."
  (file-name-nondirectory (directory-file-name file)))


;;; Load Path.

(defun packed-add-to-load-path (directory &optional package)
  (mapc (apply-partially 'add-to-list 'load-path)
        (packed-load-path directory package)))

(defun packed-remove-from-load-path (directory &optional package recursive)
  (cond (recursive
         (dolist (path load-path)
           (when (string-match (concat (regexp-quote directory) "^") path)
             (setq load-path (delete path load-path)))))
        (t
         (dolist (path (packed-load-path directory package))
           (setq load-path (delete path load-path))))))

(defun packed-load-path (directory &optional package)
  (let (lp in-lp)
    (dolist (f (directory-files directory t "^[^.]"))
      (cond ((file-regular-p f)
             (and (not in-lp)
                  (packed-library-p
                   f (or package (packed-filename directory)))
                  (add-to-list 'lp (directory-file-name directory))
                  (setq in-lp t)))
            ((file-directory-p f)
             (and (not (packed-ignore-directory-p directory package))
                  (not (file-exists-p (expand-file-name ".nosearch" f)))
                  (setq lp (nconc (packed-load-path f package) lp))))))
    lp))


;;; Byte Compile.

(defun packed-compile-package (directory &optional package force)
  (unless noninteractive
    (save-some-buffers)
    (force-mode-line-update))
  (with-current-buffer (get-buffer-create byte-compile-log-buffer)
    (setq default-directory (expand-file-name directory))
    (unless (eq major-mode 'compilation-mode)
      (compilation-mode))
    (let ((default-directory default-directory)
          (skip-count 0)
          (fail-count 0)
          (lib-count 0)
          (dir-count 0)
          file dir last-dir)
      (displaying-byte-compile-warnings
       (dolist (elt (packed-libraries-1 directory package))
         (setq file (car elt)
               dir (file-name-nondirectory file))
         (if (cdr elt)
             (case (byte-recompile-file file force 0)
               (no-byte-compile (setq skip-count (1+ skip-count)))
               ((t)             (setq  lib-count (1+  lib-count)))
               ((nil)           (setq fail-count (1+ fail-count))))
           (setq skip-count (1+ skip-count)))
         (unless (eq last-dir dir)
           (setq last-dir dir dir-count (1+ dir-count)))))
      (message "Done (Total of %d file%s compiled%s%s%s)"
               lib-count (if (= lib-count 1) "" "s")
               (if (> fail-count 0) (format ", %d failed"  fail-count) "")
               (if (> skip-count 0) (format ", %d skipped" skip-count) "")
               (if (> dir-count 1)
                   (format " in %d directories" dir-count) "")))))


;;; Autoloads.

(defun packed-loaddefs-file (&optional directory)
  (locate-dominating-file (or directory default-directory)
                          packed-loaddefs-filename))

(defun packed-load-autoloads (&optional directory)
  (let ((file (packed-loaddefs-file directory)))
    (if file
        (load file)
      (message "Cannot locate loaddefs file for %s" directory))))

(defmacro packed-with-loaddefs (dest &rest body)
  (declare (indent 1))
  `(let ((generated-autoload-file dest)
         ;; Generating autoloads runs theses hooks; disable them.
         fundamental-mode-hook
         prog-mode-hook
         emacs-lisp-mode-hook)
     (require 'autoload)
     (prog2
         (unless (file-exists-p generated-autoload-file)
           (write-region
            (replace-regexp-in-string
             ";; no-byte-compile: t\n" ""
             (autoload-rubric generated-autoload-file))
            nil generated-autoload-file))
         (progn ,@body)
       (let (buf)
         (while (setq buf (find-buffer-visiting generated-autoload-file))
           (with-current-buffer buf
             (save-buffer)
             (kill-buffer)))))))

(defun packed-update-autoloads (dest path)
  (when (or dest (setq dest (packed-loaddefs-file)))
    (packed-with-loaddefs dest
      (update-directory-autoloads path)
      (byte-compile-file dest t))))

(defun packed-remove-autoloads (dest path)
  (when (or dest (setq dest (packed-loaddefs-file)))
    (packed-with-loaddefs dest
      ;; `autoload-find-destination' clears out autoloads associated
      ;; with a file if they are not found in the current buffer
      ;; anymore (which is the case here because it is empty).
      (with-temp-buffer
        (let ((autoload-modified-buffers (list (current-buffer))))
          (dolist (d path)
            (when (and (file-directory-p d)
                       (file-exists-p d))
              (dolist (f (directory-files d t (packed-el-regexp)))
                (autoload-find-destination f (autoload-file-load-name f)))))))
      (byte-compile-file dest t))))


;;; Features.

(defconst packed-provided-regexp "\
\(\\(?:cc-\\|silentcomp-\\)?provide[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\\(?:[\s\t\n]+'\
\(\\([^(),]+\\))\\)?)")

(defun packed-provided ()
  (let (features)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward packed-provided-regexp nil t)
        (unless (save-match-data
                  (or (nth 3 (syntax-ppss))   ; in string
                      (nth 4 (syntax-ppss)))) ; in comment
          (dolist (feature (cons (match-string 1)
                                 (when (match-string 2)
                                   (split-string (match-string 2) " " t))))
            (add-to-list 'features (intern feature))))))
    (or features
        (and (goto-char (point-min))
             (re-search-forward
              "^(provide-theme[\s\t\n]+'\\([^)]+\\))" nil t)
             (list (intern (concat (match-string 1)
                                   "-theme"))))
        (and (goto-char (point-min))
             (re-search-forward
              "^(provide-me\\(?:[\s\t\n]+\"\\(.+\\)\"\\)?)" nil t)
             (list (intern (concat (match-string 1)
                                   (file-name-sans-extension
                                    (file-name-nondirectory
                                     buffer-file-name)))))))))

;; It is wrong for a library foo.el to provide foo-mode but there are many
;; packages that do just that and I don't want to deal with it right now.
(defvar packed-library-feature--accept-mode-suffix t)

(defun packed-library-feature (file)
  "Return the first valid feature actually provided by FILE.

Here valid means that requiring that feature would actually load FILE.
Normally that is the case when the feature matches the filename, e.g.
when \"foo.el\" provides `foo'.  But if \"foo.el\"s parent directory's
filename is \"bar\" then `bar/foo' would also be valid.  Of course this
depends on the actual value of `load-path', here we just assume that it
allows for file to be found.

This can be used to determine if an Emacs lisp file should be considered
a library.  Not every Emacs lisp file has to provide a feature / be a
library.  If a file lacks an expected feature then loading it using
`require' still succeeds but causes an error."
  (let ((features (packed-with-file file (packed-provided)))
        feature)
    (setq file (file-name-sans-extension
                (file-name-sans-extension file)))
    (while features
      (setq feature (pop features))
      (if (or (eq feature (intern (file-name-nondirectory file)))
              (string-match (concat (convert-standard-filename
                                     (symbol-name feature)) "$")
                            file)
              ;; $$$ kludge
              (and packed-library-feature--accept-mode-suffix
                   (or (equal (symbol-name feature)
                              (concat (file-name-nondirectory file) "-mode"))
                       (string-match
                        (concat (convert-standard-filename
                                 (concat (symbol-name feature) "-mode")) "$")
                        file))))
          (setq features nil)
        (setq feature nil)))
    feature))

(defconst packed-required-regexp "\
\(\\(?:cc-\\)?require[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\
\\(?:\\(?:[\s\t\n]+\\(?:nil\\|\".*\"\\)\\)\
\\(?:[\s\t\n]+\\(?:nil\\|\\(t\\)\\)\\)?\\)?)")

(defun packed-required ()
  (let (hard soft)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward packed-required-regexp nil t)
        (let ((feature (intern (match-string 1))))
          (cond ((save-match-data
                   (or (nth 3 (syntax-ppss))    ; in string
                       (nth 4 (syntax-ppss))))) ; in comment
                ((match-string 2)
                 (add-to-list 'soft feature))
                (t
                 (add-to-list 'hard feature))))))
    (list hard soft)))


;;; Info Pages.

(defvar packed-ginstall-info
  (or (executable-find "ginstall-info")
      (executable-find "install-info")))

(defconst packed-texinfo-regexp "\\.\\(txi\\|texi\\(nfo\\)?\\)\\'")

(defun packed-enable-info-dir-file (dir-file)
  "Add the directory containing DIR-FILE to `Info-directory-list'.
Before doing so initialize the default value of the latter if
that hasn't happened yet.  If DIR-FILE doesn't exist do nothing."
  (when (file-exists-p dir-file)
    (require 'info)
    (info-initialize)
    (add-to-list 'Info-directory-list (file-name-directory dir-file))))

(defun packed-install-info (directory dir-file)
  "Install info files from DIRECTORY in DIR-FILE.

In the directory containing DIR-FILE create links to info and
texinfo files in DIRECTORY and recursively all non-hidden
subdirectories; and add the info files to DIR-FILE.  Files are
linked to instead of copied to make it easier to later remove
files from a particular DIRECTORY.

If a texinfo file exists create a link to it and create the info
file in the directory containing DIR-FILE.  The corresponding
info file if it also exists in DIRECTORY is ignored."
  (let ((default-directory (file-name-directory dir-file)))
    (dolist (f (packed-info-files directory))
      (let ((l (file-name-nondirectory f)))
        (make-symbolic-link f l t)
        (when (string-match packed-texinfo-regexp f)
          (call-process "makeinfo" nil nil nil l)
          (setq l (concat (file-name-sans-extension l) ".info")))
        (call-process packed-ginstall-info nil nil nil l
                      (file-name-nondirectory dir-file))))))

(defun packed-uninstall-info (directory dir-file)
  "Uninstall info files located in DIRECTORY from DIR-FILE.

In the directory containing DIR-FILE remove links to info and
texinfo files in DIRECTORY and recursively all non-hidden
subdirectories; and remove the info files from DIR-FILE.

When removing a symlink to a texinfo file also remove the info
file created from it.  Also remove the corresponding entries from
DIR-FILE."
  (let ((default-directory (file-name-directory dir-file))
        (r (concat "^" (regexp-quote directory))))
    (dolist (f (directory-files default-directory "^[^.]"))
      (when (and (file-symlink-p f)
                 (string-match r (file-truename f)))
        (when (string-match packed-texinfo-regexp f)
          (delete-file f)
          (setq f (concat (file-name-sans-extension f) ".info")))
        (call-process packed-ginstall-info nil nil nil "--delete" f "dir")
        (delete-file f)))))

(defun packed-info-files (directory)
  "Return a list of info and texinfo files in DIRECTORY.

Return a list of absolute filenames of info and texinfo files in
DIRECTORY and recursively all non-hidden subdirectories.  If both
an info file and the corresponding texinfo file exist only
include the latter in the returned list."
  (let (files name)
    (dolist (f (directory-files directory t "^[^.]"))
      (cond ((file-directory-p f)
             (setq files (nconc (packed-info-files f) files)))
            ((file-regular-p f)
             (cond ((and (string-match "\\.info\\'" f)
                         (setq name (file-name-sans-extension f))
                         (not (file-exists-p (concat name ".texinfo")))
                         (not (file-exists-p (concat name ".texi")))
                         (not (file-exists-p (concat name ".txi"))))
                    (setq files (cons f files)))
                   ((string-match "\\.\\(txi\\|texi\\(nfo\\)?\\)\\'" f)
                    (setq files (cons f files)))))))
    (sort files 'string<)))

(provide 'packed)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; packed.el ends here
