
(prelude-require-packages
 '(iedit
   solarized-theme
   omnisharp
   color-theme-solarized
   magit
   elogcat
   monky
   tomatinho
   yasnippet))

(require 'yasnippet)
(yas-global-mode 1)

;; line breaks for long helm lines
(add-hook 'helm-after-initialize-hook (lambda () (with-helm-buffer (visual-line-mode))))

(setq helm-move-to-line-cycle-in-source nil)

(set-face-attribute 'default nil :height 100)

;; c-mode stuff

(setq c-basic-offset 4)
(setq c-basic-indent 4)
(setq default-tab-width 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq c-default-style "linux")
(setq indent-tabs-mode nil)
(setq safe-local-variable-values (quote ((c-default-style . "k&r") (c-default-style "k&r") (c-default-style . "bsd") (c-default-style "bsd"))))
(setq tab-always-indent t)
;;(c-set-offset 'substatement-open 0)
;;(c-set-offset 'inline-open 0)

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

(setq eldoc-idle-delay 1 flycheck-display-errors-delay 0.5)

(if (eq system-type 'darwin)
    (setq omnisharp-server-executable-path
          "~/Documents/dev/personal/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")
  (setq omnisharp-server-executable-path
        "c:/Users/Andrew/AppData/Roaming/.emacs.d/.cache/omnisharp/server/v1.19.0/OmniSharp.exe"))
(if (eq system-type 'darwin)
    (setq omnisharp--curl-executable-path "/usr/bin/curl")
  (setq omnisharp--curl-executable-path "C:/ProgramData/chocolatey/bin/curl.exe"))

(define-key omnisharp-mode-map (kbd "C-c c y") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "C-c c u") 'omnisharp-helm-find-usages)
(define-key omnisharp-mode-map (kbd "C-c c s") 'omnisharp-helm-find-symbols)

;; eldoc support might be slow?
(setq omnisharp-eldoc-support nil)

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

(setq magit-push-always-verify "PP")

(defun goto-solarized-dark () (interactive) (load-theme 'solarized-dark t))
(defun goto-solarized-light () (interactive) (load-theme 'solarized-light t))

;; we make heavy use of yaml files, but name them bytes to avoid confusing unity.
(add-to-list 'auto-mode-alist '("\\.bytes\\'" . yaml-mode))

;; whitespace cleanup can conflict with other people's IDEs, so don't do it
(add-hook 'yaml-mode-hook
          (lambda ()
            (make-variable-buffer-local 'prelude-clean-whitespace-on-save)
            (setq prelude-clean-whitespace-on-save nil)))

;; hide that annoying magit warning
(setq magit-last-seen-setup-instructions "1.4.0")

;; re-add missing first-parent
(require 'magit-core)
(require 'magit-log)
(magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent")
(add-hook 'magit-mode-hook
          (lambda()
            (local-unset-key (kbd "<C-tab>"))))

;; helm rewire

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "<C-tab>") 'helm-mini)

(defun calc-eval-region (arg beg end)
  "Calculate the region and display the result in the echo area.
With prefix ARG non-nil, insert the result at the end of region."
  (interactive "P\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (if (not (null arg))
        (message "%s = %s" expr result)
      (goto-char end)
      (save-excursion (insert result)))))

;; sql don't wrap

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(require 'elogcat)
(setq elogcat-enable-klog nil)

;; undo-tree conflicts with iedit, as does flyspell-auto-correct-previous-word

(global-undo-tree-mode 0)
(define-key flyspell-mode-map (kbd "C-;") nil)

(require 'monky)

;; pico-8 config

(add-to-list 'auto-mode-alist '("\\.p8\\'" . lua-mode))

(defun pico8-launch-buffer ()
  "launch pico-8 with the current buffer as a command"
  (interactive)
  (save-buffer)
  (start-process "pico8" nil "/Applications/PICO-8/PICO-8.app/Contents/MacOS/pico8"
                 "-windowed" "1"
                 "-run" buffer-file-name))

;; tomatino

(global-set-key (kbd "s-t") 'tomatinho)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; windows-specific bindings
(if (or (eq system-type 'windows-nt) (eq system-type 'ms-dos))
    (progn
      ;; projectile needs windows-specific grep
      (define-key projectile-mode-map (kbd "C-c p s d") 'projectile-pt)
      ;; behave more like windows 
      (global-set-key (kbd "C-<backspace>") 'backward-kill-word)))


(global-set-key (kbd "C-c c") 'helm-calcul-expression)
;; restart the server with a consistent name
(setq server-name "emacs")
(server-start)

(smart-tabs-insinuate 'c)

