(defvar required-packages
  '(ace-jump-mode expand-region magit no-easy-keys projectile 
		  color-theme-solarized json-mode flycheck ag icicles csharp-mode)
  "A list of packages to ensure are installed at launch.")

(defun required-packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (required-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'required-packages)
;;; required-packages.el ends here
