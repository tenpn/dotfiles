(prelude-require-packages '(solarized-theme omnisharp))

;; helm rewire

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "<C-tab>") 'helm-mini)

;; c-mode stuff

(setq c-basic-offset 4)
(setq c-basic-indent 4)
(setq default-tab-width 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)

;; omnisharp

(require 'omnisharp)
(require 'csharp-mode)

(setq omnisharp-server-executable-path
      "~/Documents/dev/personal/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")

(defun my-csharp-mode ()
  (add-to-list 'company-backends 'company-omnisharp)
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  (whole-line-or-region-mode)
  (electric-pair-mode))
(add-hook 'csharp-mode-hook 'my-csharp-mode)

(setq eldoc-idle-delay 0.1
      flycheck-display-errors-delay 0.2)

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
