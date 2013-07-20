
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

(defun buffer-has-unity-sln-parent ()
  (unity-find-project-sln-from-dir (file-name-directory buffer-file-name)))

(defun parse-patterns-and-restore-path (output checker _buffer) 
  "the file names listed in the error output from unity doesn't include 
the project root. flycheck can't find the files if it's not an absolute path."
  (let (
        (raw-parse-results (flycheck-parse-with-patterns output checker _buffer))
        (project-root (file-name-directory 
                        (unity-find-project-sln-from-dir 
                         (file-name-directory buffer-file-name)))))
    (-map 
     (lambda (err) (flycheck-error-new
                    :filename (concat project-root (flycheck-error-filename err))
                    :line (flycheck-error-line err)
                    :column (flycheck-error-column err)
                    :message (flycheck-error-message err)
                    :level (flycheck-error-level err)))
     raw-parse-results)))

(add-to-list 'exec-path "/Applications/Unity/Unity.app/Contents/MacOS")
(flycheck-declare-checker unity-csharp-flychecker
  "given a c-sharp file, looks for the unity file and then tries to build it using unity itsel. slower than mdtool."

  :command '("Unity" "-batchmode" "-logFile" "-quit" 
             "-projectPath" 
             (eval (file-name-directory (unity-find-project-sln-from-dir (file-name-directory buffer-file-name)))))

  :error-patterns '(
                    ("^\\(?1:.*\\.cs\\)(\\(?2:[0-9]+\\),\\(?3:[0-9]+\\))\\W*: error \\(?4:.*$\\)" 
                     error)
                    ("^\\(?1:.*\\.cs\\)(\\(?2:[0-9]+\\),\\(?3:[0-9]+\\))\\W*: warning \\(?4:.*$\\)"
                     warning))

  :modes 'csharp-mode

  :error-parser 'parse-patterns-and-restore-path

  ;; checker only valid if we can find an sln
  :predicate #'buffer-has-unity-sln-parent)

(add-to-list 'exec-path "/Applications/Xamarin Studio.app/Contents/MacOS")
(flycheck-declare-checker unity-csharp-mdtool-flychecker
  "given a c-sharp file, looks for the unity file and then tries to build it using mdtool."

  :command '("mdtool" "build" 
             (eval (unity-find-project-sln-from-dir (file-name-directory buffer-file-name))))

  :error-patterns '(
                    ("^\\(?1:.*\\.cs\\)(\\(?2:[0-9]+\\),\\(?3:[0-9]+\\))\\W*: error \\(?4:.*$\\)" 
                     error)
                    ("^\\(?1:.*\\.cs\\)(\\(?2:[0-9]+\\),\\(?3:[0-9]+\\))\\W*: warning \\(?4:.*$\\)"
                     warning))

  :modes 'csharp-mode

  ;; checker only valid if we can find an sln
  :predicate #'buffer-has-unity-sln-parent)



;;; unity-csharp ends here
