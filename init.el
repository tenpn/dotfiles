;; -*- no-byte-compile: t -*-

;;; code:

(add-to-list `load-path "~/.emacs.d/")
(add-to-list `load-path "~/.emacs.d/elpa")
(add-to-list `load-path "~/.emacs.d/packed")

(require 'flymake)

;; http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror)) 

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-global-mode 1)

;; (require 'p4)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/")
             'APPEND)
(package-initialize)
(require 'required-packages)

(load-theme 'solarized-dark t)

(defun goto-solarized-dark () (interactive) (load-theme 'solarized-dark t))
(defun goto-solarized-light () (interactive) (load-theme 'solarized-light t))

(if (eq system-type 'windows-nt)
    (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git")
  )
(global-set-key (kbd "C-#") 'magit-status)
(global-set-key (kbd "C-\\") 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style "linux")
 '(indent-tabs-mode nil)
 '(mouse-wheel-mode nil)
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
(setq default-tab-width 4)
(setq tab-width 4)

(require 'projectile)
(projectile-global-mode)

;; turn on native indexing, so we get ignoring using .projectile files
(setq projectile-use-native-indexing t) 
(setq projectile-enable-caching t)

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

;; on osx just-one-space conflicts with quicklaunch
(global-set-key (kbd "C-c g") 'just-one-space)

(when (not window-system)
  ; map odd terminal chords to keys
  (define-key key-translation-map (kbd "<backtab>") (kbd "<S-tab>"))
  (define-key key-translation-map (kbd "C-@") (kbd "C-SPC"))
  (define-key key-translation-map (kbd "M-,") (kbd "M-<"))
  (define-key key-translation-map (kbd "M-.") (kbd "M->"))
  (define-key key-translation-map (kbd "M-I") (kbd "<C-tab>"))
  (define-key key-translation-map (kbd "M-*") (kbd "M-DEL"))
  (define-key key-translation-map (kbd "M-m") (kbd "C-_")))


(defun scroll-down-chunk () 
  (interactive)
  (scroll-down 7))
(defun scroll-up-chunk () 
  (interactive)
  (scroll-up 7))

(global-set-key (kbd "C-v") 'scroll-up-chunk)
(global-set-key (kbd "M-v") 'scroll-down-chunk)

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
;;  (require 'flymake)
;;  (if (string= "windows-nt" system-type) (flymake-mode 1))
  )

(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

(require 'haml-mode)

(setq auto-mode-alist (cons '("\\.uc$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cs$" . csharp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mxml$" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pde$" . java-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)) 

;; one prototype makes heavy use of json files, but names them bytes to avoid confusing unity.
(add-to-list 'auto-mode-alist '("\\.bytes\\'" . json-mode)) 

;; UPM package files are yaml
(add-to-list 'auto-mode-alist '("\\.upm\\'" . yaml-mode)) 

;; tells emacs that .h files can sometimes be for objective c, 
;; if they have an interface attribute
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>" 
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))

(require 'actionscript-mode)
(setq auto-mode-alist (cons '("\\.as$" . actionscript-mode) auto-mode-alist))

(require 'linum)
(global-linum-mode)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-src-fontify-natively 't)
(setq org-log-done t)

;; typing, pasting or inserting with selected text causes selected text to be replaced
(delete-selection-mode 1)

(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;;(require 'flymake-cursor)

(require 'wc)

;; replace buffer list with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defadvice ibuffer
  (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name."
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-c C-f") 'icicle-recent-file)

(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

(fset 'move-line-to-last-line
   [?\M-m ?\C-  ?\C-p ?\C-e ?  ])
(global-set-key (kbd "C-c m") 'move-line-to-last-line)

(fset 'copy-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217837 67108896 5 134217847] 0 "%d")) arg)))
(global-set-key (kbd "C-c w") 'copy-line)

(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path 
            (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" 
                             (getenv "PATH")))
      )
  )

(defun open-in-explorer ()
  "Run explorer on the directory of the current buffer."
  (interactive)
  (shell-command (concat "explorer "
                         (replace-regexp-in-string "/" "\\\\"
                                                   (expand-file-name default-directory)))))

;; set up quick jump to I'm Feeling Lucky some term and open it in a browser
(global-set-key "\C-cj" 'webjump) ;; regular webjump
(setq webjump-sites
      (append '(
                ("lucky google" . 
                 [simple-query 
                  "http://google.com"
                  "http://google.com/search?btnI=1&q="
                  ""]))
              ))

(require-maybe 'ag)

(require 'flycheck)

(load "unity-csharp")
(add-to-list `flycheck-checkers 'unity-csharp-mdtool-flychecker)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

(global-set-key (kbd "C-c t") 'compile-unity-tests)

(require 'multiple-cursors)

(global-set-key (kbd "C-c v") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)


(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

(global-hl-line-mode +1)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; when compiling, re-use compile buffers in other frames
(setq-default display-buffer-reuse-frames t)

;; window/frame navigation
;; S-[arrow keys] moves around open frames, windows
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

(require 'icicles)

(icy-mode 1)


