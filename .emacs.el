;;; package --- Alex's emacs config

;;; Commentary:
;; 


;; no start up message
;;; Code:

(defvar shmeemacs/default-font-size 110)

;; default is 800 KB
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     (gcs-done))))

;; Silence compiler warnings
(setq comp-async-report-warnings-errors nil)



(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;disable visual scrollbar
(tool-bar-mode -1)   ;disable the toolbar
(tooltip-mode -1)    ;disable tooltips
(set-fringe-mode 10) ;give some breathing room

(menu-bar-mode -1)   ;disable the menu bar

;; set up the visible bell
(setq visible-bell t)

;; scrolling settings
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; scroll one line at a time
(setq mouse-wheel-progressive-speed nil)           ;; dont accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                 ;; scroll window under mouse
(setq scroll-step 1)                               ;; scroll one line at a time

;; frame transparency and maximize window by default
(set-frame-parameter (selected-frame) 'alpha '(97 . 97))
(add-to-list 'default-frame-alist 'alpha '(97 . 97))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable line numbers and customize their format
(column-number-mode)
(global-display-line-numbers-mode t)

;; enable for the following modes:
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda () display-line-numbers-mode 1)))

;; disable for the following modes:
(dolist (mode '(shell-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () display-line-numbers-mode 1)))

;; dont warn for large files
(setq large-file-warning-threshold nil)

;; dont warn for following symlinked files
(setq vc-follow-symlinks t)



;; set font and size
(set-face-attribute 'default nil :font "Fira Code Retina" :height shmeemacs/default-font-size)

;; enable proper unicode glyph support
(defun conf/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
		     (lambda (i) (string-equal (car i) block-name))
		     unicode-fonts-block-font-mapping))
	 (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
	 (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
	  `(,updated-block))))

(use-package unicode-fonts
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  (mapcar
   (lambda (block-name)
     (conf/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
   '("Dingbats"
     "Emoticons"
     "Miscellaneous Symbols and Pictographs"
     "Transport and Map Symbols"))
  (unicode-fonts-setup))

;; emojis in buffers
(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;; Auto saving
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-autosave-when-idle t))

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package source
(require 'package)

;; set variable for remote archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/"))) 

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platformsq
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(use-package command-log-mode)
(use-package diminish)
(use-package swiper)
(use-package counsel)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
        :map ivy-minibuffer-map
        ("TAB" . ivy-alt-done)
	("C-l" . ivy-alt-done)
	("C-j" . ivy-next-line)
	("C-k" . ivy-previous-line)
	:map ivy-switch-buffer-map
	("C-k" . ivy-previous-line)
	("C-l" . ivy-done)
	("C-d" . ivy-switch-buffer-kill)
	:map ivy-reverse-i-search-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; don't start searches with 

;; icons used by doom-modeline
(use-package all-the-icons)
;; use cute doom modeline
(use-package doom-modeline
  :ensure t
  :init ( doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 50)))

;; doom themes
(use-package doom-themes)
(load-theme 'doom-challenger-deep t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; key-bindings package
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer shmeemacs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (shmeemacs/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose-theme")))

(use-package use-package-chords
  :disabled
  :config (key-chord-mode 1))

(defun conf/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(defun conf/dont-arrow-me-bro ()
   (interactive)
   (message "Array keys are bad, you know?"))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'conf/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; use visual line motion even outside of the visual line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil) ;; bug)????
  :custom
  (setq evil-collection-mode-list
	(remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

;; GUIX PACKAGES

;; Hydra for transient keybindings
;; here we have it allowing use to scale text
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(shmeemacs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

;; projectile package for "Project Interaction"
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev/")
    (setq projectile-project-search-path '("~/dev/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; magit for git integration
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(defun shmeemacs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; set up org mode
(defun shmeemacs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :hook (org-mode . shmeemacs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers nil
	org-src-fontify-natively t
	org-fontify-quote-and-verse-blocks t
	org-src-tab-acts-natively t
	org-edit-src-content-indentation 2
	org-hide-block-startup nil
	org-src-preserve-indentation nil
	org-startup-folded 'content
	org-cycle-separator-lines 2)

  ;; org-agenda settings
  (setq org-agenda-files
	'("~/life/tasks.org"
	  "~/life/birthdays.org"
	  "~/life/habits.org")
	org-agenda-start-with-log-mode t
        org-agenda-log-done 'time
	org-log-into-drawer t)

  ;; set up todo workflows
  ;; entry 1 is the default workflow
  ;; entry 2 is a template custom workflow - see if it makes sense for you (project planning)
  ;; TODO add more custom workflows that work for me
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(t)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; configure agenda views for workslows
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/life/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/life/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/life/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/life/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/life/metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))
  
  (setq org-modules
	'(org-crypt
	  org-habit
	  org-bookmark
	  org-eshell
	  org-irc))

  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)
      ("tasks.org" :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ledger . t)
     (python . t)
     (C . t)
     (haskell . t)
     (latex . t)
     (makefile . t)
     (lisp . t)
     (R . t)
     (lua . t)
     (sql . t)))

  (shmeemacs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))
  

(defun shmeemacs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; visual column package for ORG mode so lines wrap
(use-package visual-fill-column
  :hook (org-mode . shmeemacs/org-mode-visual-fill))


;; LANGUAGE SERVERS
(use-package lsp-mode
  :commands lsp
  :hook ((typescript-mode) . lsp)
  :bind (:map lsp-mode-map
	      ("TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable nil))

(shmeemacs/leader-keys
 "l" '(:ignore t :which-key "lsp")
 "ld" 'xref-find-definitions
 "lr" 'xref-find-references
 "ln" 'lsp-ui-find-next-reference
 "lp" 'lsp-ui-find-prev-reference
 "ls" 'counsel-imenu
 "le" 'lsp-ui-flycheck-list
 "lS" 'lsp-ui-sideline-mode
 "lX" 'lsp-execute-code-action)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

;; debug adapter support
(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

;; lisp and scheme configs
(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
	 (scheme-mode . lispy-mode)))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators c-w additional
					additional-movement slurp/barf-cp
					prettify)))

;; set up nvm to manage node versions
(use-package nvm
  :defer t)

;; typescript/javascript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Clojure
(use-package cider
  :mode "\\.clj[sc]?\\'"
  :config
  (evil-collection-cider-setup))

;; common lisp
(use-package sly
  :disabled
  :mode "\\.lisp\\'")

(use-package slime
  :disabled
  :mode "\\.lisp\\'")

;; C/C++
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
	 (lambda () (require 'ccls) (lsp))))

;; golang
(use-package go-mode
  :hook (go-mode . lsp-deferred))

;; rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :defer t)

;; markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun shmeemacs/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun shmeemacs/markdown-mode-hook ()
    (shmeemacs/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'shmeemacs/markdown-mode-hook))

;; HTML
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode)

(use-package skewer-mode)

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; CODE COMPILATION
(use-package compile
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

;; sytax checking with flycheck
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; smart parentheses
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; http
(use-package know-your-http-well
  :defer t)

(provide 'init)

 
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-magit counsel-projectile magit projectile super-save emojify unicode-fonts hydra evil-collection evil general doom-themes helpful ivy-rich which-key rainbow-delimiters use-package doom-modeline diminish counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
