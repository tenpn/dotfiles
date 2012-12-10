;; -*- no-byte-compile: t -*-

(add-to-list `load-path "~/.emacs.d/")
(add-to-list `load-path "~/.emacs.d/icicles")
(add-to-list `load-path "~/.emacs.d/elpa")
(add-to-list `load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list `load-path "~/.emacs.d/color-theme-solarized")
(add-to-list `load-path "~/.emacs.d/packed")

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-global-mode 1)

;; (require 'p4)
(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

(require 'color-theme-solarized)
(color-theme-solarized-dark)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(require 'required-packages)

(if (eq system-type 'windows-nt)
    (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git")
  )
(global-set-key (kbd "C-#") 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style "linux")
 '(indent-tabs-mode nil)
 '(read-buffer-completion-ignore-case t)
 '(tab-always-indent t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(c-set-offset 'substatement-open 0)
(c-set-offset 'inline-open 0)

(setq c-basic-indent 4)
(setq tab-width 4)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(autoload 'powershell "powershell" "run powershell as a shellw ithin emacs" t)
;;(setq explicit-shell-file-name "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
;;(setq explicit-powershell.exe-args '("-Command" "-" )) ; interactive, but no command prompt 

;; editing major mode
(require 'powershell-mode)
(setq auto-mode-alist (cons '("\\.ps1$" . powershell-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.psm1$" . powershell-mode) auto-mode-alist))

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

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(defun my-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (turn-on-auto-revert-mode)
;;  (setq indent-tabs-mode nil)
  (require 'flymake)
  (if (string= "windows-nt" system-type) (flymake-mode 1))
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

;; typing, pasting or inserting with selected text causes selected text to be replaced
(delete-selection-mode 1)

(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)

(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

(require 'flymake-cursor)

(require 'wc)

(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-c C-f") 'icicle-recent-file)

(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path 
            (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" 
                             (getenv "PATH")))
      )
  )

(require 'icicles)
(icy-mode 1)
