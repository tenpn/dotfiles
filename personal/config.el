
(prelude-require-packages
 '(iedit
   solarized-theme
   omnisharp))

;; helm rewire

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "<C-tab>") 'helm-mini)

;; line breaks for long helm lines
(add-hook 'helm-after-initialize-hook (lambda () (with-helm-buffer (visual-line-mode))))

(setq helm-move-to-line-cycle-in-source nil)

;; c-mode stuff

(setq c-basic-offset 4)
(setq c-basic-indent 4)
(setq default-tab-width 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq c-default-style "linux")
(setq indent-tabs-mode nil)
(setq safe-local-variable-values (quote ((c-default-style . "k&r") (c-default-style "k&r"))))
(setq tab-always-indent t)
(c-set-offset 'substatement-open 0)
(c-set-offset 'inline-open 0)

;; omnisharp

(require 'csharp-mode)
(setq csharp-want-imenu nil)

(require 'omnisharp)

(defun my-csharp-mode ()
  (add-to-list 'company-backends 'company-omnisharp)
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  )
(add-hook 'csharp-mode-hook 'my-csharp-mode)

(setq eldoc-idle-delay 0.1 flycheck-display-errors-delay 0.2)

(setq omnisharp-server-executable-path
      "~/Documents/dev/personal/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")
(setq omnisharp--curl-executable-path "/usr/bin/curl")

(define-key omnisharp-mode-map (kbd "C-c c y") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "C-c c u") 'omnisharp-helm-find-usages)
(define-key omnisharp-mode-map (kbd "C-c c s") 'omnisharp-helm-find-symbols)

;; I hate the full-screen default scroll. let's step instead of leap

(defun scroll-down-chunk ()
  (interactive)
  (scroll-down 7))
(defun scroll-up-chunk ()
  (interactive)
  (scroll-up 7))

(global-set-key (kbd "C-v") 'scroll-up-chunk)
(global-set-key (kbd "M-v") 'scroll-down-chunk)

;; other

(setq-default dired-listing-switches "-alhv")

(require 'linum)
(global-linum-mode)

(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

; turn off prelude's bullshit whitespace
(setq prelude-whitespace nil)

(global-set-key (kbd "C-#") 'magit-status)
(global-set-key (kbd "C-\\") 'magit-status)

(defun goto-solarized-dark () (interactive) (load-theme 'solarized-dark t))
(defun goto-solarized-light () (interactive) (load-theme 'solarized-light t))

;; we make heavy use of yaml files, but name them bytes to avoid confusing unity.
(add-to-list 'auto-mode-alist '("\\.bytes\\'" . yaml-mode)) 

;; hide that annoying magit warning
(setq magit-last-seen-setup-instructions "1.4.0")
