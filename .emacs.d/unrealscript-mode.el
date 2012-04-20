(defvar unrealscript-mode-hook nil)

(defvar unrealscript-mode-map
  (let ((unrealscript-mode-map (make-sparse-keymap)))
 (define-key unrealscript-mode-map "\C-j" 'newline-and-indent)
 unrealscript-mode-map)
  "Keymap for UNREALSCRIPT major mode")


(defconst unrealscript-font-lock-keywords-1
  (list
   '("\\<\\(?:break\\|c\\(?:\\(?:as\\|ontinu\\)e\\)\\|do\\|e\\(?:lse\\|xtends\\)\\|for\\(?:each\\)?\\|i\\(?:f\\|nterface\\)\\|new\\|return\\|switch\\|var\\|while\\|class\\)\\>" . font-lock-keyword-face))
  "Minimal highlighting expressions for UNREALSCRIPT mode.")

(defconst unrealscript-font-lock-keywords-2
  (append unrealscript-font-lock-keywords-1
    (list
     '("\\<\\(?:array\\|b\\(?:ool\\|yte\\)\\|c\\(?:lass\\|o\\(?:erce\\|lor\\|ords\\)\\)\\|de\\(?:faultproperties\\|legate\\)\\|e\\(?:num\\|vent\\)\\|f\\(?:alse\\|loat\\|unction\\)\\|int\\|local\\|name\\|o\\(?:ptional\\|ut\\)\\|plane\\|r\\(?:egion\\|otator\\)\\|st\\(?:ate\\|r\\(?:ing\\|uct\\)\\)\\|true\\|v\\(?:\\(?:a\\|ecto\\)r\\)\\)\\>" . font-lock-keyword-face)))
  "Additional Keywords to highlight in UNREALSCRIPT mode.")


(defconst unrealscript-font-lock-keywords-3
  (append unrealscript-font-lock-keywords-2
    (list
     '("\\<\\(?:A\\(?:bstract\\|llowAbstract\\|uto\\(?:Comment\\|ExpandCategories\\)\\)\\|Co\\(?:llapseCategories\\|nfig\\)\\|D\\(?:ep\\(?:endsOn\\|recated\\)\\|isplayName\\|ontCollapseCategories\\)\\|Edit\\(?:Condition\\|InlineNew\\)\\|FriendlyName\\|Hide\\(?:Categories\\|DropDown\\)\\|I\\(?:\\(?:mplemen\\|nheri\\)ts\\)\\|N\\(?:ative\\(?:Replication\\)?\\|o\\(?:Export\\|nTransient\\|t\\(?:EditInlineNew\\|Placeable\\)\\)\\)\\|P\\(?:erObject\\(?:Config\\|Localized\\)\\|laceable\\)\\|ShowCategories\\|T\\(?:oolTip\\|ransient\\)\\|Within\\)\\>" . font-lock-type-face)
     '("\\<\\(?:auto\\|c\\(?:lient\\|on\\(?:fig\\|st\\)\\)\\|d\\(?:atabinding\\|eprecated\\|uplicatetransient\\)\\|e\\(?:dit\\(?:const\\|fixedsize\\|inline\\(?:use\\)?\\|oronly\\)\\|x\\(?:ec\\|port\\)\\)\\|globalconfig\\|i\\(?:gnores\\|n\\(?:it\\|put\\|stanced\\|terp\\)\\|terator\\)\\|l\\(?:atent\\|ocalized\\)\\|n\\(?:ative\\(?:replication\\)?\\|o\\(?:clear\\|export\\|import\\|ntransactional\\|tforconsole\\)\\)\\|operator\\|p\\(?:o\\(?:\\(?:inte\\|stoperato\\)r\\)\\|r\\(?:eoperator\\|ivate\\|otected\\)\\|ublic\\)\\|re\\(?:liable\\|pnotify\\)\\|s\\(?:erver\\|i\\(?:mulated\\|ngular\\)\\|kip\\|tatic\\)\\|tra\\(?:nsient\\|vel\\)\\|unreliable\\)\\>" . font-lock-keyword-face)
     '("\\<\\(?:A\\(?:bs\\|cos\\|dd\\(?:Item\\)?\\|llActors\\|s\\(?:c\\|in\\)\\|tan\\)\\|B\\(?:asedActors\\|egin\\(?:Play\\|State\\)\\)\\|C\\(?:aps\\|eil\\|h\\(?:ildActors\\|r\\)\\|l\\(?:amp\\|earTimer\\)\\|o\\(?:\\(?:llidingActor\\)?s\\)\\)\\|D\\(?:estroyed\\|i\\(?:\\(?:sabl\\|vid\\)e\\)\\|ynamicActors\\)\\|E\\(?:mpty\\|n\\(?:\\(?:abl\\|dStat\\)e\\)\\|val\\|xp\\)\\|F\\(?:Clamp\\|M\\(?:ax\\|in\\)\\|Rand\\|astTrace\\|in\\(?:d\\|ish\\(?:Anim\\|Interpolation\\)\\)\\)\\|G\\(?:etTimer\\(?:Count\\|Rate\\)\\|oTo\\(?:State\\)?\\)\\|I\\(?:n\\(?:Str\\|itGame\\|sert\\(?:Item\\)?\\|vert\\)\\|s\\(?:\\(?:InStat\\|TimerActiv\\)e\\)\\)\\|L\\(?:e\\(?:ft\\|n\\|rp\\)\\|o\\(?:cs\\|ge\\)\\)\\|M\\(?:ax\\|i\\(?:rrorVectorByNormal\\|[dn]\\)\\)\\|Normal\\|OverlappingActors\\|P\\(?:o\\(?:pState\\|stBeginPlay\\)\\|reBeginPlay\\|ushState\\)\\|R\\(?:and\\|e\\(?:move\\(?:I\\(?:ndex\\|tem\\)\\)?\\|pl\\(?:ace\\)?\\)\\|ight\\|ound\\)\\|S\\(?:et\\(?:State\\|Timer\\)\\|in\\|leep\\|merp\\|p\\(?:awn\\|lit\\)\\|q\\(?:rt\\|uare\\)\\)\\|T\\(?:an\\|ick\\|ouchingActors\\|race\\(?:Actors\\)?\\)\\|V\\(?:Rand\\|Size\\|isible\\(?:\\(?:Colliding\\)?Actors\\)\\)\\|\\(?:ro\\|vec\\)t\\)\\>" . font-lock-function-name-face)))
  "Balls-out highlighting in UNREALSCRIPT mode.")

(defvar unrealscript-font-lock-keywords unrealscript-font-lock-keywords-3
  "Default highlighting expressions for UNREALSCRIPT mode.")


(defvar unrealscript-indent-width 4)

(defun looking-at-unrealscript-indent-keyword ()
  (or (looking-at "^[\t ]*while") (looking-at "^[\t ]*if") (looking-at "^[\t ]*else") (looking-at "^[\t ]*for")))

(defun looking-at-unrealscript-block-end ()  
  (or (looking-at "^[\t ]*end") (and (looking-at "^.*}[ \t]*$") (not (looking-at "^.*{")))))

(defun looking-at-unrealscript-block-start ()
  (or (looking-at "^[\t ]*begin") (and (looking-at "^.*{") (not (looking-at "^.*}[ \t]*$")))))

;; Function to control indenting.
(defun unrealscript-indent-line ()
  "Indent current line as Unrealscript code"
  (interactive)
  ;; Set the point to beginning of line.
  (beginning-of-line)
  (if (bobp)
   (indent-line-to 0)
 (let ((not-indented t) (lines-back 0) cur-indent)
   (if  (looking-at-unrealscript-block-end) ; Check for closing brace
    ;; if we are at the end of a block
    (progn
   (save-excursion
     (forward-line -1)
     (setq lines-back (+ lines-back 1))
     (setq cur-indent (- (current-indentation) unrealscript-indent-width)))
   ;; Safety check to make sure we don't indent negative.
   (if (< cur-indent 0)
    (setq cur-indent 0)))
  ;; else scan backward 
  (save-excursion
    (if (looking-at-unrealscript-block-start) ; Opening block     
     (progn
    (forward-line -1)
    (setq lines-back (+ lines-back 1))
    (setq cur-indent (current-indentation))
    (setq not-indented nil))
   (while not-indented
     (forward-line -1)
     (setq lines-back (+ lines-back 1))
     (if (looking-at-unrealscript-block-end) ;; Closing Block
      (progn
     (setq cur-indent (current-indentation))
     (setq not-indented nil))
    (if (looking-at-unrealscript-block-start)
     (progn
       (setq cur-indent (+ (current-indentation) unrealscript-indent-width))
       (setq not-indented nil))
      (if (looking-at-unrealscript-indent-keyword)
       (progn
      (setq cur-indent (current-indentation))
      (forward-line 1)
      (setq lines-back (- lines-back 1))
      (if (looking-at-unrealscript-block-start)
       (setq not-indented nil) ;; has block
        (if (zerop lines-back) ;; no block
         (progn
        (setq cur-indent (+ cur-indent unrealscript-indent-width))       
        (setq not-indented nil))
       (setq not-indented nil))))
     (if (bobp)
      (setq not-indented nil)))))))))
   (if cur-indent
    (indent-line-to cur-indent)
  (indent-line-to 0))))) 

(defun unrealscript-populate-syntax-table (table)
  "Populate the given syntax table as necessary for a C-like language.
This includes setting ' and \" as string delimiters, and setting up
the comment syntax to handle both line style \"//\" and block style
\"/*\" \"*/\" comments."

  (modify-syntax-entry ?_  "_"    table)
  (modify-syntax-entry ?\\ "\\"    table)
  (modify-syntax-entry ?+  "."    table)
  (modify-syntax-entry ?-  "."    table)
  (modify-syntax-entry ?=  "."    table)
  (modify-syntax-entry ?%  "."    table)
  (modify-syntax-entry ?<  "."    table)
  (modify-syntax-entry ?>  "."    table)
  (modify-syntax-entry ?&  "."    table)
  (modify-syntax-entry ?|  "."    table)
  (modify-syntax-entry ?\' "\""    table)
  (modify-syntax-entry ?\240 "."  table)

  ;; Set up block and line oriented comments.  The new C
  ;; standard mandates both comment styles even in C, so since
  ;; all languages now require dual comments, we make this the
  ;; default.
  (modify-syntax-entry ?/  ". 124b" table)
  (modify-syntax-entry ?*  ". 23" table)

  (modify-syntax-entry ?\n "> b" table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?\^m "> b" table)
  table)

(defvar unrealscript-mode-syntax-table
  (let ((unrealscript-mode-syntax-table (unrealscript-populate-syntax-table (make-syntax-table))))
 unrealscript-mode-syntax-table)
  "Syntax table for unrealscript-mode")

(defun unrealscript-mode ()
  "Major mode for editing Unrealscript files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table unrealscript-mode-syntax-table)
  (use-local-map unrealscript-mode-map)
  (setq indent-line-function 'unrealscript-indent-line)
  (setq font-lock-defaults '(unrealscript-font-lock-keywords nil t))
  (setq major-mode 'unrealscript-mode)
  (setq mode-name "UNREALSCRIPT")
  (run-hooks 'unrealscript-mode-hook)
  (setq case-fold-search t)
  (setq font-lock-keywords-case-fold-search t))


(provide 'unrealscript-mode)
