(add-to-list `load-path "~/.emacs.d/")
(add-to-list `load-path "~/.emacs.d/icicles")
(add-to-list `load-path "~/.emacs.d/elpa")
(add-to-list `load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list `load-path "~/.emacs.d/color-theme-solarized")
(require 'p4)
(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

(require 'color-theme-solarized)
(color-theme-solarized-dark)

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
  (interactive "sShell command:")
  (shell-command (format shell-command-text (shell-quote-argument buffer-file-name)))
  )

(global-set-key (kbd "M-\"") 'execute-shell-command-on-buffer)
(global-set-key (kbd "<C-tab>") 'icicle-buffer)

(require 'compile)
(mapcar
 (lambda (x)
   (add-to-list 'compilation-error-regexp-alist-alist x))
 
 (list
  ;; Microsoft C/C++:
  ;;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
  ;;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
  ;;  .\cppcli1.cpp(36): error C2059: syntax error : 'public'
  ;;  e:\projects\myce40\tok.h(85) : error C2236: unexpected 'class' '$S1'
  ;;  myc.cpp(14) : error C3149: 'class System::String' : illegal use of managed type 'String'; did you forget a '*'?
  ;;   ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) ?\: \\(error\\|warning\\) C[0-9]+:" 1 3)
  '(msvc "^[ \t]*\\([A-Za-z0-9\\.][^(]*\\.\\(cpp\\|c\\|h\\)\\)(\\([0-9]+\\)) *: +\\(error\\|fatal error\\|warning\\) C[0-9]+:" 1 3)
  
  ))

(setq compilation-error-regexp-alist
      (mapcar 'car compilation-error-regexp-alist-alist))

;; ideally I want to manipulate the current buffer filename to extract the path to the compiler, 
;; but that's beyond me at the moment.
(defun compile-script-with-debug ()
  (interactive)
  (save-some-buffers "all no questions")
  (compile "P:/BombGameBranch_iOS/Binaries/Win32/bombgame-win32-debug.com make")
  )

;; again ideally this should find the right exe to execute
(defvar bombgame-levels-loaded (list "bomb_prototype"))
(defun run-bombgame-editor (level-name)
  (interactive (list (read-from-minibuffer "Level: " (car bombgame-levels-loaded) nil nil 'bombgame-levels-loaded)))
  (save-some-buffers "all no questions")
  (shell-command (format "P:/BombGameBranch_iOS/Binaries/Win32/bombgame-win32-debug.com editor %s -NoGADWarning" level-name))
  )

(global-set-key (kbd "C-c e") 'run-bombgame-editor)
(global-set-key (kbd "C-c c") 'compile-script-with-debug)

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(defun my-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (turn-on-auto-revert-mode)
  (setq indent-tabs-mode nil)
  (require 'flymake)
  (flymake-mode 1)
  )

(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

(setq auto-mode-alist (cons '("\\.uc$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cs$" . csharp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mxml$" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pde$" . java-mode) auto-mode-alist))

(require 'actionscript-mode)
(setq auto-mode-alist (cons '("\\.as$" . actionscript-mode) auto-mode-alist))

(require 'linum)
(global-linum-mode)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(require 'icicles)
(icy-mode 1)
