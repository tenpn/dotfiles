
;;;; Code:

(defun csharp-find-sln-file-from-dir (dir-path)
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
          (csharp-find-sln-file-from-dir (expand-file-name (concat dir-path "/.."))) ;; keep going down stack
          ))
    nil))

(defun buffer-has-unity-sln-parent ()
  (csharp-find-sln-file-from-dir (file-name-directory buffer-file-name)))

(add-to-list 'exec-path "/Applications/Xamarin Studio.app/Contents/MacOS")
(flycheck-declare-checker unity-csharp-flychecker
  "given a c-sharp file, looks for the unity file and then tries to build it using mdtool."

  :command '("mdtool" "build" 
             (eval (csharp-find-sln-file-from-dir (file-name-directory buffer-file-name))))

  :error-patterns '(
                    ("^\\(?1:.*\\.cs\\)(\\(?2:[0-9]+\\),\\(?3:[0-9]+\\))\\W*: error \\(?4:.*$\\)" 
                     error)
                    ("^\\(?1:.*\\.cs\\)(\\(?2:[0-9]+\\),\\(?3:[0-9]+\\))\\W*: warning \\(?4:.*$\\)"
                     warning))

  :modes 'csharp-mode

  ;; checker only valid if we can find an sln
  :predicate #'buffer-has-unity-sln-parent)


;;; unity-csharp ends here
