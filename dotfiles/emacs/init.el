;; ######################################
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

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Automatically reload files edited outside emacs
(global-auto-revert-mode t)

;; shift arrow to change window
(windmove-default-keybindings)

;; cursor type
(setq-default cursor-type 'bar)

;; ############################################################
;; ################## DISPLAY ################################

(global-hl-line-mode t)

(delete-selection-mode t)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq set-fringe-mode '(5 . 0))

(setq vj/font-name "Hack")
(defcustom vj/font-size 11 "My default font size")

(use-package which-key
  :init (which-key-mode))

(use-package all-the-icons)

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'column))

;; ########################################################################
;; ########################################################################

;; ########### THEME ####################
(use-package ample-theme
  :init
  (progn
    (load-theme 'ample t t)
    (load-theme 'ample-flat t t)
    (load-theme 'ample-light t t)
    (enable-theme 'ample))
  :defer t
  :ensure t)

;; ############### MAGIT #################
(use-package magit
  :init (setq magit-diff-refine-hunk t)
  :commands (magit-status)
  :bind (("C-c m" . magit-status)))

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
;; ########################################################################

(use-package projectile
  :defer 5
  :init
  (setq projectile-switch-project-action #'projectile-dired
        projectile-completion-system 'ivy)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-global-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(use-package ivy
  :init
  (setq ivy-initial-inputs-alist nil)
  :bind (("C-x b" . ivy-switch-buffer))
  :custom
  (progn (setq ivy-use-virtual-buffers t)
         (setq ivy-display-style 'fancy)
         (setq ivy-count-format "%d/%d "))
  :config
  (ivy-mode 1)
  (use-package ivy-hydra
    :defer t)
  (use-package flx))

(use-package all-the-icons-ivy
  :init (all-the-icons-ivy-setup))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)))

(use-package counsel
  :after swiper
  :bind
  (("M-y" . counsel-yank-pop) ;; Better kill ring package
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :config
  (counsel-mode))

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

;; (use-package intero
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode))

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


;; Javascript and Typescript

;; json-mode
(use-package json-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))

(use-package prettier-js
  :ensure t
  :after (rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))

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
