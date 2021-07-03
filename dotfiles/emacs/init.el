;; ######## SANE DEFAULTS ###############

(setq ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil
      echo-keystrokes 0.5
      confirm-kill-processes nil
      disabled-command-function nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-inside-paren 1)
(column-number-mode t)
(setq inhibit-startup-screen t)

;;(add-hook 'emacs-startup-hook (lambda () (org-agenda nil "n")))

;; scrolling
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      hscroll-margin 1
      hscroll-step 1
      scroll-preserve-screen-position t)


(winner-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; (desktop-save-mode t)
(save-place-mode t)

;; Security hype
(setq gnutls-verify-error t
      tls-checktrust t)

;; Disk space is cheap. Save lots. (c) Sacha Chua
;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t
      auto-save-list-file-prefix "~/.emacs.d/autosave/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; But don't create stupid lockfiles
(setq create-lockfiles nil)

;; History
(setq savehist-file "~/.emacs.d/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring)
      recentf-max-saved-items 50)
(savehist-mode 1)
(recentf-mode 1)

;; Start emacs in fullscreen
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Removes trailing whitespaces when saving the file
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq user-full-name (getenv "NAME")
      user-mail-address (getenv "EMAIL"))

;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (set-face-attribute 'default nil :family "Fira Code Retina" :height 140))

(add-to-list 'exec-path "/Users/sgunisetty/.nix-profile/bin")
(setq auth-sources '("~/.authinfo.gpg"))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Automatically reload files edited outside emacs
(global-auto-revert-mode t)

;; shift arrow to change window
(windmove-default-keybindings)

;; cursor type
(unless (display-graphic-p)
  (setq-default cursor-type 'bar))

;; ################## DISPLAY ################################

(delete-selection-mode t)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq set-fringe-mode '(5 . 0))

(use-package which-key
  :init (which-key-mode))

(use-package all-the-icons
  :ensure t)

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'column))

;; ########### THEME ####################
(use-package ample-theme
  :ensure t
  :init
  (progn
    (load-theme 'ample t t)
    (load-theme 'ample-flat t t)
    (load-theme 'ample-light t t)))

(use-package humanoid-themes
  :ensure t
  :init
  (progn
    (load-theme 'humanoid-light t t)
    (load-theme 'humanoid-dark t t)))

(use-package modus-themes
  :ensure t
  :init
  (progn
    (load-theme 'modus-vivendi t t)
    (load-theme 'modus-operandi t t)))

(use-package spacegray-theme
  :ensure t
  :init
  (progn
    (load-theme 'spacegray t t)))

(if (display-graphic-p)
    (enable-theme 'modus-vivendi)
  (enable-theme 'ample))

;; ############### MAGIT #################
(use-package magit
  :init (setq magit-diff-refine-hunk t)
  :commands (magit-status)
  :bind (("C-c m" . magit-status))
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-gitflow
  :hook (magit-mode-hook . turn-on-magit-gitflow))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode t))

(use-package git-link
  :commands (git-link git-link-commit git-link-open-in-browser)
  :custom (git-link-open-in-browser t))

(use-package forge
  :after magit)

;; ########################################################################

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ;;("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
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
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  ;;(setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package all-the-icons-ivy
  :init (all-the-icons-ivy-setup)
  :ensure t)

(use-package counsel
  :demand t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-b" . counsel-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev/git")
    (setq projectile-project-search-path '("~/dev/git")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode)
  :init
  (setq counsel-ag-base-commad "ag -zS --nocolor --nogroup %s")
  (setq counsel-rg-base-command "rg -S -M 200 --no-heading --line-number --color never %s ."))

(use-package multiple-cursors
  :init
  (progn
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)))

(global-set-key (kbd "C-x ;") 'set-rectangular-region-anchor)
(global-set-key (kbd "M-D") 'kill-whole-line)

(use-package expand-region
  :config
  (global-set-key (kbd "C-x -") 'er/expand-region))

(use-package paredit
  :ensure t
  :bind (("C-c d" . paredit-forward-down))
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(use-package paredit-everywhere
  :ensure t
  :diminish paredit-everywhere-mode
  :config
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              (highlight-parentheses-mode))))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode))))

(global-highlight-parentheses-mode)

(use-package flycheck-clj-kondo
  :ensure t)

(use-package company
  :custom
  (company-require-match nil)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.7)
  (company-tooltip-align-annotation t)
  :hook ((cider-repl-mode-hook  . #'cider-company-enable-fuzzy-completion)
         (cider-mode-hook . #'cider-company-enable-fuzzy-completion)
         (after-init-hook . 'global-company-mode))
  :config
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'flycheck-mode)
  (put-clojure-indent 'defui '(1 nil nil (1)))
  (put-clojure-indent 'facts 1)
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'future-fact 1)
  (put-clojure-indent 'not-join 1)
  (put-clojure-indent 'or-join 1)
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'search 1)
  (put-clojure-indent 'scan 1)
  (require 'flycheck-clj-kondo))

(use-package cider
  :ensure t
  :bind (("C-c c" . cider-connect))
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook
            '(lambda ()
               (define-key cider-repl-mode-map (kbd "C-c C-b") 'cider-repl-clear-buffer))))


(defun cider--gather-session-params (session)
  "Gather all params for a SESSION."
  (let (params)
    (dolist (repl (cdr session))
      (when (buffer-name repl)
        (setq params (cider--gather-connect-params params repl))))
    (when-let* ((server (cider--session-server session)))
      (setq params (cider--gather-connect-params params server)))
    params))

(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  :config
  ;; Configure the Clojure Refactoring prefix:
  (cljr-add-keybindings-with-prefix "C-c C-m")
  :diminish clj-refactor-mode)

(use-package org
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE")
          (sequence "BACKLOG" "PLAN" "ACTIVE" "REVIEW" "HOLD" "INCOMPLETE" "|" "COMPLETED" "CANCLED")))
  (setq org-agenda-files
        '("~/dev/sunil/svg-orgfiles/tasks.org"
          "~/dev/sunil/svg-orgfiles/journal.org"
          "~/dev/sunil/svg-orgfiles/logbook.org"
          "~/dev/sunil/svg-orgfiles/personal-and-home.org"))
  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/dev/sunil/svg-orgfiles/tasks.org" "Tasks")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("tp" "Project" entry (file+olp "~/dev/sunil/svg-orgfiles/tasks.org" "Projects")
           "* PLAN %?\n  %U\n  %a\n  %i" :empty-lines 1)))
  (setq org-modules '(org-habit)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package ag)

(use-package wgrep-ag
  :after ag)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

(use-package terraform-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; Haskell mode
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; OR for flymake support:
  (add-hook 'haskell-mode-hook 'flymake-mode)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (if (file-exists-p (concat tide-project-root "node_modules/typescript/bin/tsserver"))
    (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
    (setq tide-tsserver-executable "/Users/sgunisetty/.nix-profile/bin/tsserver"))
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq tide-format-options
        '(:indentSize 2 :tabSize 2 :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
  (setq typescript-indent-level 2)
  (local-set-key (kbd "C-c d") 'tide-documentation-at-point)
  (company-mode +1))

(defun mac-font ()
  (interactive)
  (set-face-attribute 'default nil :family "Fira Code Retina" :height 120))

(defun lg-font ()
  (interactive)
  (set-face-attribute 'default nil :family "Fira Code Retina" :height 140))

;; typescript setup
(use-package tide
  :ensure t
  :config
  (progn
    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))))

;; use web-mode + tide-mode for javascript instead
(use-package js2-mode
  :ensure t
  :config
  (progn
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    ;; configure javascript-tide checker to run after your default javascript checker
    (setq js2-basic-offset 2)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))

(use-package json-mode
  :ensure t
  :config
  (progn
    (flycheck-add-mode 'json-jsonlint 'json-mode)
    (add-hook 'json-mode-hook 'flycheck-mode)
    (setq js-indent-level 2)
    (add-to-list 'auto-mode-alist '("\\.json" . json-mode))))

(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js" . web-mode))
    ;; this magic incantation fixes highlighting of jsx syntax in .js files
    (setq web-mode-content-types-alist
          '(("jsx" . "\\.js[x]?\\'")))
    (add-hook 'web-mode-hook
              (lambda ()
                (setq web-mode-code-indent-offset 2)
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))
                (when (string-equal "jsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))
                (when (string-equal "js" (file-name-extension buffer-file-name))
                  (progn
                    (setup-tide-mode)
                    (with-eval-after-load 'flycheck
                      (flycheck-add-mode 'typescript-tslint 'web-mode)
                      (flycheck-add-mode 'javascript-tide 'web-mode))))))))
