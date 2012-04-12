(add-to-list `load-path "~/.emacs.d/")
(add-to-list `load-path "~/.emacs.d/icicles")
(require 'p4)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style "linux")
 '(indent-tabs-mode nil)
 '(tab-always-indent t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(setq c-basic-indent 4)
(setq tab-width 4)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(autoload 'powershell "powershell" "run powershell as a shellw ithin emacs" t)
;;(setq explicit-shell-file-name "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
;;(setq explicit-powershell.exe-args '("-Command" "-" )) ; interactive, but no command prompt 

;; https://gist.github.com/2367513
(defun execute-shell-command-on-buffer (shell-command-text)
  (interactive "MShell command:")
  (shell-command (format shell-command-text (shell-quote-argument buffer-file-name)))
  )

(global-set-key (kbd "M-\"") 'execute-shell-command-on-buffer)

;; ideally I want to manipulate the current buffer filename to extract the path to the compiler, 
;; but that's beyond me at the moment.
(defun compile-script-with-debug ()
  (interactive)
  (shell-command "P:/BombGameBranch_iOS/Binaries/Win32/bombgame-win32-debug.com make")
  )

(global-set-key (kbd "C-c C-c") 'compile-script-with-debug)

;; again ideally this should find the right exe to execute
(defun run-bombgame-editor (level-name)
  (interactive "Mlevel:")
  (shell-command (format "P:/BombGameBranch_iOS/Binaries/Win32/bombgame-win32-debug.com editor %s -NoGADWarning" level-name))
  )

(global-set-key (kbd "C-c C-e") 'run-bombgame-editor)

(require 'icicles)
(icy-mode 1)
