
;;;; Code:

(require 'flycheck)
(require 'dash)

(defun unity-find-project-sln-from-dir (dir-path)
  "walks up the tree from the current dir, trying to find an sln.

If it finds one, returns it, else nil.
"
  (if (and (file-exists-p dir-path) (not (string= dir-path "/")))
      (let* (
            (dir-name (file-name-base (directory-file-name dir-path)))
            (sln-path (concat (file-name-as-directory dir-path) 
                              (concat dir-name ".sln"))))
        (if (file-exists-p sln-path)
            sln-path
          (unity-find-project-sln-from-dir (expand-file-name (concat dir-path "/.."))) ;; keep going down stack
          ))
    nil))

(defun unity-find-project-dir-from-file (project-file)
  (file-name-directory (unity-find-project-sln-from-dir 
                        (file-name-directory buffer-file-name))))

(defun buffer-has-unity-sln-parent ()
  (unity-find-project-sln-from-dir (file-name-directory buffer-file-name)))

(defun restore-root-if-necessary (project-root err) 
  (let* (
         (error-filename (flycheck-error-filename err))
         (is-rooted (string-match (concat "^" project-root ".*") error-filename)))
    (if (not is-rooted) 
        (flycheck-error-new
         :filename (concat project-root error-filename)
         :line (flycheck-error-line err)
         :column (flycheck-error-column err)
         :message (flycheck-error-message err)
         :level (flycheck-error-level err))
      err)))

(defun parse-patterns-and-restore-path (output checker _buffer) 
  "the file names listed in the error output from unity doesn't include 
the project root. flycheck can't find the files if it's not an absolute path."
  (let (
        (raw-parse-results (flycheck-parse-with-patterns output checker _buffer))
        (project-root (unity-find-project-dir-from-file buffer-file-name)))
    (-map 
     (lambda (err) (restore-root-if-necessary project-root err))
     raw-parse-results)))

(defvar unity-path "/Applications/Unity/Unity.app/Contents/MacOS")
(add-to-list 'exec-path unity-path)

(defvar mdtool-error-patterns 
      '(
        ("^\\(?1:.*\\.cs\\)(\\(?2:[0-9]+\\),\\(?3:[0-9]+\\))\\W*: error \\(?4:.*$\\)" 
         error)
        ("^\\(?1:.*\\.cs\\)(\\(?2:[0-9]+\\),\\(?3:[0-9]+\\))\\W*: warning \\(?4:.*$\\)"
         warning)))

(flycheck-declare-checker unity-csharp-flychecker
  "given a c-sharp file, looks for the unity file and then tries to build it using unity itsel. slower than mdtool."

  :command '("Unity" "-batchmode" "-logFile" "-quit" 
             "-projectPath" 
             (eval (unity-find-project-dir-from-file buffer-file-name)))

  :error-patterns mdtool-error-patterns

  :modes 'csharp-mode

  :error-parser 'parse-patterns-and-restore-path

  ;; checker only valid if we can find an sln
  :predicate #'buffer-has-unity-sln-parent)

(flycheck-declare-checker unity-csharp-testrunner-flychecker
  "given a c-sharp file, looks for the unity file and then tries to build it using unity itsel. slower than mdtool."

  :command '("Unity" "-batchmode" "-logFile" "-quit" 
             "-executeMethod" "UnTest.TestRunner.RunTestsFromConsole"
             "-projectPath" 
             (eval (unity-find-project-dir-from-file buffer-file-name)))

  :error-patterns mdtool-error-patterns

  :modes 'csharp-mode

  :error-parser 'parse-patterns-and-restore-path

  ;; checker only valid if we can find an sln
  :predicate #'buffer-has-unity-sln-parent)

(defun compile-unity-tests ()
  (interactive)
  (let ((project-root (unity-find-project-dir-from-file buffer-file-name)))
    (if project-root
        (compile (concat unity-path "/Unity "
                         "-batchmode -logFile -quit " 
                         "-executeMethod UnTest.TestRunner.RunTestsFromConsole "
                         "-projectPath " project-root)))))

(add-to-list 'compilation-error-regexp-alist
             '("^\\(?1:.*\\.cs\\)(\\(?2:[0-9]+\\),\\(?3:[0-9]+\\))\\W*: error \\(?4:.*$\\)"
               1 2 3 nil 4))

(add-to-list 'exec-path "/Applications/Xamarin Studio.app/Contents/MacOS")
(flycheck-declare-checker unity-csharp-mdtool-flychecker
  "given a c-sharp file, looks for the unity file and then tries to build it using mdtool."

  :command '("mdtool" "build" 
             (eval (unity-find-project-sln-from-dir (file-name-directory buffer-file-name))))

  :error-patterns mdtool-error-patterns

  :modes 'csharp-mode

  ;; checker only valid if we can find an sln
  :predicate #'buffer-has-unity-sln-parent)



;;; unity-csharp ends here
