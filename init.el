;;; .emacs -- My emacs config file
;;; Commentary:
;;; Code:



;; Package configuration
;;
;; Make sure use-package is installed and loaded
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents t)            ;async package refresh
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


;; Environment variables

;; Make sure environment variables are properly taken from shell (osx)
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH"
					 "MANPATH"
					 "DICTIONARY"))
  (exec-path-from-shell-initialize))

;; Need LANG for spell check (not initialized in launcher env)
(setenv "DICTIONARY" "en_US")
;; less breaks shell; use cat
(setenv "PAGER" "cat")
;; add my own binaries to PATH
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name user-emacs-directory) "bin"))
;; Make ipython prompt work in emacs
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
;; Use emacs client as editor
(setenv "EDITOR" "emacsclient")

;; direnv loads environment variables based on directory
(use-package direnv
  :ensure t
  :config
  (direnv-mode))


;; Code completion

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setf company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-show-numbers t
        company-require-match nil)
  (global-company-mode 1))


(use-package rtags
  :ensure t
  :config
  (setq rtags-path (concat (rtags-package-install-path)
                           "rtags-"
                           rtags-package-version
                           "/bin/"))
  (rtags-start-process-unless-running)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (local-set-key (kbd "M-.") #'rtags-find-symbol-at-point))))

;; Python completion/jump to definition
(use-package anaconda-mode
  :config
  (require 'python)
  (add-hook 'python-mode-hook #'anaconda-mode))

;; Python completion
(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda))



(use-package company-shell
  :ensure t
  :config
  (add-to-list 'company-backends 'company-shell))


;; Visuals
(use-package alect-themes
  :ensure t
  :config
  (alect-create-theme dark)
  (custom-theme-set-faces
   'alect-dark
   '(flyspell-incorrect  ((t (:underline (:color "red" :style wave)))))))
(set-face-attribute 'default nil :height 120)


;; Modeline documentation
(use-package eldoc
  :config
  (add-hook 'c-mode-hook #'eldoc-mode))


;; PDF
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))


;; Helm
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "C-x C-w") #'write-file)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-c i") #'helm-mini)
  (global-set-key (kbd "M-x") #'helm-M-x))
(use-package helm-mode
  :diminish helm-mode
  :config
  (setq helm-completing-read-handlers-alist
        '((describe-function . helm-completing-read-symbols)
          (describe-variable . helm-completing-read-symbols)
          (completion-at-point . nil))
        helm-mode-handle-completion-in-region nil)
  (helm-mode 1))


;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (global-set-key (kbd "\C-cx") #'projectile-compile-project))
(use-package helm-projectile
  :ensure t)




;; e-mail

(setq user-full-name  "Peter Thompson")


;; Spelling
(use-package ispell
  :config
  (let ((hunspell-exec (executable-find "hunspell")))
    (when hunspell-exec

      ;; Work around for Hunspell 1.7.0
      (defun manage-hunspell-1.7 (old-function-ispell &rest arguments)
        "Add a null device when calling \"hunspell -D\"."
        (if  (equal "-D"  (nth 4 arguments))
            (funcall old-function-ispell hunspell-exec null-device t nil "-D" null-device)
          (apply old-function-ispell arguments)))
      (advice-add 'ispell-call-process :around #'manage-hunspell-1.7)

      (setq ispell-program-name hunspell-exec)
      (setq ispell-local-dictionary-alist '(("en_US"
                                             "[[:alpha:]]"
                                             "[^[:alpha:]]"
                                             "[']"
                                             t
                                             ("-d" "en_US")
                                             nil
                                             iso-8859-1)))
      (ispell-change-dictionary "en_US" t))))


;; Comint

(use-package comint
  :config
  (setq
   ;; We get completion from company
   comint-completion-autolist nil
   comint-completion-addsuffix t
   comint-input-ignoredups t
   ;; Do not set to non-nil; breaks clisp
   comint-process-echoes nil
   comint-move-point-for-output nil
   comint-scroll-show-maximum-output nil
   comint-buffer-maximum-size 1024)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
  (defun close-comint-hook ()
    "Automatically close the comint buffer."
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (process event)
                            (when (string= event "finished\n")
                              (kill-buffer (current-buffer))))))
  (add-hook 'comint-exec-hook #'close-comint-hook)
  (defun my-comint-scroll-hook()
    (setq-local scroll-conservatively 101))
  (add-hook 'comint-mode-hook #'my-comint-scroll-hook))


;; Shell

(defun set-comint-process-echoes ()
  (setq comint-process-echoes t))

(use-package shell
  :config
  (let ((shell (executable-find "bash")))
    (when shell
      (setq explicit-shell-file-name shell)))
  (add-hook 'shell-mode-hook #'compilation-shell-minor-mode)
  (add-hook 'shell-mode-hook #'set-comint-process-echoes))


;; Mac-specific key bindings
(setq mac-right-command-modifier 'meta
      mac-command-modifier 'meta
      mac-right-option-modifier 'meta
      mac-option-modifier 'super)

(use-package diminish
  :ensure t)

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "mypackages")))

(require 'cl-lib)

(use-package yaml-mode
  :ensure t)

(setq-local lexical-binding t)

(use-package abbrev
  :config
  (abbrev-mode -1))

(use-package simple
  :config
  (auto-fill-mode -1))


(use-package cmake-mode
  :ensure t)


(require 'cc-mode)
(use-package clang-format
  :ensure t
  :config
  (advice-add #'c-indent-region :override #'clang-format-region)
  (define-key c++-mode-map (kbd "TAB")  #'clang-format-region))

;; Unbind suspend when in a separate window
(when (or (eq window-system 'x)
          (eq window-system 'ns))
  (global-unset-key (kbd "C-z")))

(defun save-all-markers-advice (orig-fun &rest args)
  "Restore marker location after jumping to a register.
Advise around ORIG-FUN called with ARGS."
  (let ((points (save-current-buffer
                  (map 'list (lambda (buffer)
                               (set-buffer buffer)
                               (cons buffer (point)))
                       (buffer-list))))
        (ret (apply orig-fun args)))
    (mapc (lambda (window)
            (let ((buffer-point (cdr (assoc (window-buffer window) points))))
              (when buffer-point (set-window-point window buffer-point))))
          (window-list))
    ret))
(advice-add 'jump-to-register :around #'save-all-markers-advice)

(use-package man
  :config
  (setq Man-notify-method 'pushy))

(cl-flet ((disable (mode) (when (fboundp mode) (funcall mode -1))))
  (disable 'menu-bar-mode)
  (disable 'tool-bar-mode)
  (disable 'scroll-bar-mode))
(setf use-dialog-box nil)

;; Remap ALT to META
(setq x-alt-keysym 'meta)

;; Better mouse scrolling
(setf mouse-wheel-scroll-amount '(3 ((shift) . 3))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

(use-package face-remap
  :config
  (setf text-scale-mode-step 1.1))


(setf ring-bell-function 'ignore)
(setf set-mark-command-repeat-pop t)

(use-package autoinsert
  :config
  (auto-insert-mode 1))

(setq-default require-final-newline t)
(setq-default fill-column 79)
(menu-bar-enable-clipboard)
(column-number-mode t)
(setf kill-whole-line t)
(show-paren-mode 1)
(electric-pair-mode 1)
(add-to-list 'revert-without-query ".+\\.\\(?:pdf\\|PDF\\)$")
(setf sentence-end-double-space nil)
(setf gc-cons-threshold 20000000)       ;Makes GC faster
(setq-default indent-tabs-mode nil)
(setf comment-auto-fill-only-comments t
      auto-fill-function nil)
(setq-default indent-tabs-mode nil
              align-default-spacing 2)

(use-package ediff
  :config
  (setq-default ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package gdb-mi
  :config
  (setf gdb-show-main t)
  (setq gdb-many-windows t)
  (setq gdb-display-buffer-other-frame-action '((display-buffer-reuse-window display-buffer-pop-up-frame)
                                                (reusable-frames . visible)
                                                (inhibit-same-window . t)
                                                (pop-up-frame-parameters
                                                 (height . 14)
                                                 (width . 80)
                                                 (unsplittable . t)
                                                 (tool-bar-lines)
                                                 (menu-bar-lines)
                                                 (minibuffer)))))

(dotimes (i 10)
  (global-set-key (kbd (format "M-%d" i))
                  `(lambda ()
                     (interactive)
                     (select-window (window-at 0 0))
                     (other-window (- ,i 1)))))

(setf backup-directory-alist
      (list (cons ".*"
                  (expand-file-name (concat user-emacs-directory
                                            "backups")))))

;; Swap keys for regex and regular isearch/replace
(global-set-key (kbd "\C-s") #'isearch-forward-regexp)
(global-set-key (kbd "\C-r") #'isearch-backward-regexp)
(global-set-key (kbd "\C-\M-s") #'isearch-forward)
(global-set-key (kbd "\C-\M-r") #'isearch-backward)
(global-set-key (kbd "\M-%") #'query-replace-regexp)
(global-set-key (kbd "C-M-%") #'query-replace)
(global-set-key (kbd "\M-%") #'query-replace-regexp)
(global-set-key (kbd "<C-M-backspace>") #'backward-kill-sexp)
(setq-default isearch-allow-scroll t)

(defun my-exchange-point-and-mark (&optional arg)
  "Change when region is activated in `exchange-point-and-mark'.
The state of the region will not altered without a prefix ARG,
otherwise it is enabled."
  (interactive "p")
  (exchange-point-and-mark (and (not (use-region-p))
                                (= 1 arg))))

(global-set-key (kbd "C-x C-x") 'my-exchange-point-and-mark)

(use-package calc-units
  :config
  (setf math-additional-units
        '((GiB "1024 * MiB" "Giga Byte")
          (MiB "1024 * KiB" "Mega Byte")
          (KiB "1024 * B" "Kilo Byte")
          (B nil "Byte")
          (Gib "1024 * Mib" "Giga Bit")
          (Mib "1024 * Kib" "Mega Bit")
          (Kib "1024 * b" "Kilo Bit")
          (b "B / 8" "Bit"))))

(use-package uniquify
  :config
  (setf uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package recentf
  :config
  (recentf-mode t)
  (setf recentf-max-saved-items 1000))

(use-package compile
  :config
  (setf compile-command "make -k -j4")
  (setf compilation-read-command nil)
  (setf compilation-save-buffers-predicate t))

(use-package my-compile
  :config
  (global-set-key (kbd "\C-cw") #'my-compile-setup-windows))

(use-package elscreen
  :ensure t
  :config
  (setq elscreen-tab-display-control nil)
  (setq elscreen-display-screen-number nil)
  (elscreen-start))

(require 'grep)
(let ((ack-cmd (cond ((executable-find "ack-grep") "ack-grep")
                     ((executable-find "ack") "ack")))
      (ack-args " --nogroup -H "))
  (when ack-cmd
    (grep-apply-setting 'grep-command (concat ack-cmd ack-args))))

(defun grep-override (grep-cmd)
  "Run grep with GREP-CMD."
  (let ((old-cmd grep-command))
    (grep-apply-setting 'grep-command grep-cmd)
    (unwind-protect
        (call-interactively #'grep)
      (grep-apply-setting 'grep-command old-cmd))))

(defun grep-global ()
  "Run grep with GNU Global."
  (interactive)
  (grep-override "global --result grep -xi "))

(use-package magit
  :ensure t
  :config
  (defvar magit-last-seen-setup-instructions "1.4.0"))

(global-set-key (kbd "\C-cs") #'shell)
(global-set-key (kbd "\C-cd") #'gdb)
(global-set-key (kbd "\C-cf") #'recentf-open-files)
(global-set-key (kbd "\C-cg") #'magit-status)
(global-set-key (kbd "\C-cb") #'grep-global)
(global-set-key (kbd "\C-cq") #'grep)

(defadvice pwd (before kill-pwd activate)
  "Place working directory in kill ring when calling `pwd'."
  (kill-new default-directory))

(use-package delsel
  :config
  (pending-delete-mode 1))

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.5))

(use-package multiple-cursors
  :ensure t
  :config
  (define-key mc/keymap (kbd "<return>") nil)
  (global-set-key (kbd "C-c j") #'mc/mark-all-like-this-dwim)
  (global-set-key (kbd "C-c u") #'mc/mark-all-symbols-like-this-in-defun)
  (global-set-key (kbd "C-c l") #'mc/edit-lines))

(use-package asm-mode
  :config
  (setq asm-comment-char ?\@))

(use-package sql
  :config
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))
  (let ((sqlite (cond ((executable-find "sqlite3"))
                      ((executable-find "sqlite")))))
    (when sqlite
      (setq-default sql-sqlite-program sqlite))))

(eval-after-load 'lisp-mode
  '(define-key lisp-mode-map (kbd "C-c C-c") 'eval-buffer))


(use-package web-mode
  :ensure t
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . web-mode)))

(use-package tide
  :ensure t
  :config
  (defun my-tide-setup ()
    (when (string-match "^tsx?$" (file-name-extension buffer-file-name))
      (tide-setup)))
  (add-hook 'web-mode-hook #'my-tide-setup))

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(use-package python
  :config
  (add-hook 'python-mode-hook
   (lambda () (setq-local comment-inline-offset 2)))
  (setq-default python-shell-interpreter
                (cl-flet ((exec-find (program)
                                     (when (executable-find program) program)))
                  (cond ((exec-find "ipython3"))
                        ((exec-find "python3"))
                        ((exec-find "ipython"))
                        ((exec-find "python")))))

  (let ((ipython-p (or (equal python-shell-interpreter "ipython3")
                       (equal python-shell-interpreter "ipython"))))
    (when ipython-p
      (setq python-shell-interpreter-args "--matplotlib"))))

(use-package rjsx-mode
  :ensure t
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js-switch-indent-offset 2)
  (setq js-indent-level 2))

(use-package blacken
  :ensure t
  :config
  (add-hook 'python-mode-hook #'blacken-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (let ((fixed (executable-find "chktex-fixed")))
    (when fixed
      (setq flycheck-tex-chktex-executable fixed)))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(require 'cc-mode)

(add-hook 'c++-mode-hook
          (defun my-c++-flycheck-hook ()
            (setq-local flycheck-clang-language-standard "c++17")
            (setq-local flycheck-gcc-language-standard "c++17")))

(add-hook 'c-mode-hook
          (defun my-c-flycheck-hook()
            ;; (flycheck-select-checker 'c/c++-gcc)
            (setq-local flycheck-clang-language-standard "gnu11")
            (setq-local flycheck-gcc-language-standard "gnu11")))

;; arduino...
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

(require 'cc-mode)

(setq-default tab-width 4)

(defun my-check-header-guards ()
  "Make sure header guard defines match filename."
  (save-excursion
    (goto-char (point-min))
    (when
        (search-forward-regexp
         "#ifndef\\s-+\\(\\(\\sw\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+#define\\s-+\\1"
         nil t)
      (let ((guard (match-string 1))
            (expected (upcase
                       (concat (file-name-nondirectory
                                (file-name-sans-extension buffer-file-name))
                               "_"
                               (file-name-extension buffer-file-name)))))
        (when (not (string= guard expected))
          (message "Warning: header guard not consistent with filename."))))))

(defun my-header-hook ()
  "Header file save hook."
  (when (string= (file-name-extension buffer-file-name) "h")
    (my-check-header-guards)))

(add-hook 'after-save-hook #'my-header-hook)

;; Fix c++ enum

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
(require 'org)
(require 'org-capture)
(require 'org-agenda)

(remove-hook 'org-agenda-after-show-hook #'org-narrow-to-subtree)

(defvar my-org-folder (file-name-as-directory (expand-file-name "~/org/")))
(defvar my-org-doc-file (expand-file-name "~/doc/doc.org"))

(defun my-org-capture ()
  "Store a note in the agenda file."
  (interactive)
  (let* ((agenda-file (concat my-org-folder "agenda.org"))
         (org-capture-entry `("t" "task" entry (file ,agenda-file) "* %^{Task Name}
/Entered on %U/
%?")))
    (ignore-errors (org-capture))))

;; (global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(defvar my-capture-temp nil)

(defun my-parse-bibtex (bibtex-string)
  (string-match "\\@\\(.*\\){\\(.*\\),\\(\\(?:.\\|\n\\)*\\)}" bibtex-string)
  (let* ((doc-type (match-string 1 bibtex-string))
         (doc-ref (match-string 2 bibtex-string))
         (doc-params (match-string 3 bibtex-string))
         (doc-param-alist ())
         (param-remain doc-params)
         (properties))
    (while (string-match
            "\\(?:.\\|\n\\)*?\\([a-zA-Z]+\\)={\\(.*?\\)}\\(\\(?:.\\|\n\\)*\\)"
            param-remain)
      (let ((key (match-string 1 param-remain))
            (val (match-string 2 param-remain)))
        (push (cons key val) doc-param-alist)
        (setq param-remain (match-string 3 param-remain))))
    (setq doc-param-alist (nreverse doc-param-alist))
    (list doc-type doc-ref doc-param-alist)))

(defun my-make-property-string (property-alist)
  (with-temp-buffer
    (insert (apply #'concat
                   (mapcar #'(lambda (key-val)
                               (concat ":" (upcase (car key-val)) ": " (cdr key-val) "\n"))
                           property-alist)))
    (org-indent-region (point-min) (point-max))
    (org-indent-remove-properties-from-string (buffer-string))))

(defvar my-property-string nil)
(defvar my-doc-folder (expand-file-name "~/doc/"))
(defvar my-bibtex-string nil)

(defun my-get-doc-info ()
  (let* ((file-name (expand-file-name (read-file-name "File Name: "
                                                      nil
                                                      nil
                                                      t)))
         (bibtex-string (read-string "BibTeX: "))
         (parse-result (my-parse-bibtex bibtex-string))
         (doc-type (first parse-result))
         (doc-ref (second parse-result))
         (doc-param-alist (third parse-result))
         (title (or (cdr (assoc "title" doc-param-alist))
                    (read-string "Title: "
                                 (file-name-base file-name))))
         ;; (new-file-name (expand-file-name (concat my-doc-folder
         ;;                                          doc-ref
         ;;                                          (file-name-extension file-name t))))
         )
    ;; (copy-file file-name new-file-name)
    ;; (push (cons "file_link" (concat "file:" new-file-name)) doc-param-alist)
    (push (cons "file_link" (concat "file:" file-name)) doc-param-alist)
    (setq my-property-string (my-make-property-string doc-param-alist))
    (setq my-bibtex-string
          (replace-regexp-in-string "\\`\[ \t\n\]*"
                                    ""
                                    (replace-regexp-in-string "\[ \t\n\]*\\'"
                                                              "\n"
                                                              bibtex-string)))
    title))

(setf
 ;; Files
 org-default-notes-file (concat my-org-folder "notes.org")

 ;; Go to link on RET
 org-return-follows-link t

 ;; Indent text based on outline
 org-startup-indented t

 ;; Start agenda on current day
 org-agenda-start-on-weekday nil

 org-log-done 'note

 ;; Show \alpha, \beta, \gamma... as UTF-8
 org-pretty-entities t
 ;; Only enable super/subscripts within {}
 org-use-sub-superscripts '{}

 ;; Show items with dates in the TODO buffer
 org-agenda-todo-ignore-scheduled nil
 org-agenda-todo-ignore-deadlines nil

 org-todo-keywords '((type "TODO(t)" "WAIT(w@/!)""|" "DONE(d@/!)"))

 org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                          ("WAIT" :foreground "dark orange" :weight bold)
                          ("DONE" :foreground "forest green" :weight bold))

 org-agenda-files (list (concat my-org-folder "agenda.org"))



 ;; Capture templates
 org-capture-templates
 `(("t" "task" entry (file ,(concat my-org-folder "agenda.org"))
    "* TODO %^{Task Name} %^G\n/Entered on %U/\n%?")
   ("d" "document" entry (file ,my-org-doc-file)
    "* %(my-get-doc-info) %^G
:PROPERTIES:
:DATE_ADDED: %U
%(eval my-property-string):END:
:BIBTEX:
%(eval my-bibtex-string):END:

%?")))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
(require 'dired-aux)
(setf dired-listing-switches "-lAh"
      dired-dwim-target t
      dired-isearch-filenames 'dwim
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      delete-by-moving-to-trash t)
(require 'ibuffer)
;; Use Dired x
(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))
(nconc completion-ignored-extensions
       '(".fdb_latexmk" ".synctex.gz" ".fls" ".snm" ".nav" ".out"))

(add-hook 'dired-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(use-package diredful
  :ensure t
  :config
  (diredful-mode 1))

(use-package dired
  :config
  (setq dired-omit-files (mapconcat #'identity
                                    '("^\\.DS_Store$"
                                      "^\\.localized$"
                                      "^\\.com.apple.timemachine.supported$")
                                    "\\|")))

;; Macros
(global-set-key "\C-x(" 'kmacro-start-macro-or-insert-counter)

;; Don't use emac's ls program
(require 'ls-lisp)
(setf ls-lisp-use-insert-directory-program t)

(defun open-in-external-app (file-name)
  "Open the file given by FILE-NAME in external app.
The app is chosen from your OS's preference."
  (interactive (list (read-file-name "File: " nil nil t "")))
  (let ((fname (expand-file-name file-name)))
    (cl-flet ((system-p (sys) (string-equal system-type sys)))
      (cond ((system-p "windows-nt")
             (let ((w32-fname (replace-regexp-in-string "/" "\\" fname t t)))
               (w32-shell-execute "open" w32-fname)))
            ((system-p "darwin")
             (shell-command (format "open \"%s\"" fname)))
            ((system-p "gnu/linux")
             (let ((process-connection-type nil))
               (start-process "" nil "xdg-open" fname)))
            (t (error "Unknown system type: %s" system-type)))
      (recentf-add-file fname))))

(global-set-key "\C-co" 'open-in-external-app)


(define-minor-mode override-mode
  "Minor mode for overriding default keybindings."
  :init-value nil
  :lighter ""
  :keymap `((,(kbd "C-.") . dabbrev-expand)
            (,(kbd "M-o") . other-window))
  :global t)
(override-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX
(use-package tex
  :ensure auctex
  :config
  (require 'preview)
  (require 'reftex)
  (require 'latex)
  (require 'font-latex)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (push '("Latexmk" "latexmk %s" TeX-run-TeX nil t :help "Run latexmk") TeX-command-list)


  (setf preview-default-option-list '("displaymath" "floats" "graphics" "textmath")
        preview-auto-cache-preamble t
        preview-auto-reveal nil
        preview-preserve-counters t
        preview-image-type 'tiff
        TeX-PDF-mode t
        TeX-electric-math '("$" . "$")
        TeX-electric-sub-and-superscript t
        reftex-cite-format 'natbib
        reftex-label-alist '(("IEEEeqnarray" ?e "eq:" "~(\\ref{%s})" nil nil)
                             ("IEEEeqnarray*" ?e "eq:" "~(\\ref{%s})" nil nil))
        font-latex-match-reference-keywords '(("citep" "*[{") ("citet" "*[{")))

  (setq LaTeX-font-list
        '((?\C-a ""              ""  "\\mathcal{"    "}")
          (?\C-b "\\textbf{"     "}" "\\bm{"     "}") ;Use bm
          (?\C-c "\\textsc{"     "}")
          (?\C-e "\\emph{"       "}")
          (?\C-f "\\textsf{"     "}" "\\mathsf{"     "}")
          (?\C-i "\\textit{"     "}" "\\mathit{"     "}")
          (?\C-m "\\textmd{"     "}")
          (?\C-n "\\textnormal{" "}" "\\mathnormal{" "}")
          (?\C-r "\\textrm{"     "}" "\\mathrm{"     "}")
          (?\C-s "\\textsl{"     "}" "\\mathbb{"     "}")
          (?\C-t "\\texttt{"     "}" "\\mathtt{"     "}")
          (?\C-u "\\textup{"     "}")
          (?\C-d "" "" t)))

  (defun my-env-frame (environment)
    (let ((LaTeX-default-position nil)
          (LaTeX-default-format "rCl"))
      (LaTeX-env-label environment)))

  (defun my-latex-frame-hook ()
    (LaTeX-add-environments
     '("frame" my-env-frame)))

  (defun my-env-IEEEeqnarray (environment)
    (let ((LaTeX-default-position nil)
          (LaTeX-default-format "rCl"))
      (LaTeX-env-array environment)))

  (defun my-latex-IEEE-hook ()
    (LaTeX-add-environments
     '("IEEEeqnarray" my-env-IEEEeqnarray)
     '("IEEEeqnarray*" my-env-IEEEeqnarray)
     "matrix"
     "bmatrix"
     "Bmatrix"
     "pmatrix"
     "Pmatrix"
     "vmatrix"
     "Vmatrix"
     "tikzpicture"
     "smallmatrix"))

  (add-to-list 'LaTeX-indent-environment-list '("matrix"))
  (add-to-list 'LaTeX-indent-environment-list '("bmatrix"))
  (add-to-list 'LaTeX-indent-environment-list '("Bmatrix"))
  (add-to-list 'LaTeX-indent-environment-list '("pmatrix"))
  (add-to-list 'LaTeX-indent-environment-list '("Pmatrix"))
  (add-to-list 'LaTeX-indent-environment-list '("vmatrix"))
  (add-to-list 'LaTeX-indent-environment-list '("Vmatrix"))
  (add-to-list 'LaTeX-indent-environment-list '("smallmatrix"))

  (add-hook 'LaTeX-mode-hook #'my-latex-IEEE-hook)

  (require 'texmathp)
  (dolist (math-env '("IEEEeqnarray" "IEEEeqnarray*"))
    (add-to-list 'LaTeX-indent-environment-list `(,math-env LaTeX-indent-tabular))
    (add-to-list 'font-latex-math-environments math-env)
    (add-to-list 'LaTeX-label-alist `(,math-env . LaTeX-equation-label))
    (add-to-list 'reftex-label-alist `(,math-env ?e "eq:" "~(\\ref{%s})" t nil))
    (add-to-list 'texmathp-tex-commands `(,math-env env-on)))
  (texmathp-compile)

  (defvar my-latex-class-completion-list
    '("article"
      "proc"
      "minimal"
      "report"
      "book"
      "slides"
      "memoir"
      "letter"
      "beamer"))

  (defvar my-latex-package-list
    '(("amsmath" . ("centertags"
                    "tbtags"
                    "sumlimits"
                    "nosumlimits"
                    "intlimits"
                    "nointlimits"
                    "namelimits"
                    "nonamelimits"
                    "leqno"
                    "reqno"
                    "fleqno"))
      ("amsfonts" . ())
      ("amssymb" . ())
      ("amsthm" . ())
      ("IEEEtrantools" . ())
      ("bm" . ())
      ("natbib" . ("square"
                   "super"
                   "sort"
                   "sort&compress"
                   "compress"
                   "comma"
                   "numbers"))
      ("tikz" . ())
      ("natbib" . ("round"
                   "square"
                   "curly"
                   "angle"
                   "semicolon"
                   "colon"
                   "comma"
                   "authoryear"
                   "numbers"
                   "super"
                   "sort"
                   "sort&compress"
                   "compress"
                   "longnamesfirst"
                   "sectionbib"
                   "nonamebreak"
                   "merge"
                   "elide"
                   "mcite"))
      ("geometry" . ("letterpaper"
                     "a5paper"
                     "b5paper"
                     "executivepaper"
                     "legalpaper"
                     "landscape"
                     "total="
                     "margin="))
      ("graphicx" . ())
      ("hyperref" . ())
      ("tabularx" . ())
      ("booktabs" . ())))

  (defvar my-latex-option-completion-list
    '("10pt"
      "11pt"
      "12pt"
      "a4paper"
      "letterpaper"
      "a5paper"
      "b5paper"
      "executivepaper"
      "legalpaper"
      "draft"
      "twocolumn"
      "oneside"
      "twoside"
      "notitlepage"
      "titlepage"))

  (defvar my-temp)

  (setcdr (assoc 'latex-mode auto-insert-alist)
          '("options, RET: "
            "\\documentclass["
            ((completing-read "options: " my-latex-option-completion-list) str & ", " | -2) & -2 & ?\] | -1
            ?{ (completing-read "class: " my-latex-class-completion-list) "}\n"
            ((completing-read "package: " (mapcar #'car my-latex-package-list))
             '(progn
                (setq my-temp (eval str))
                nil)
             "\\usepackage["
             ((completing-read "options: " (cdr-safe (assoc my-temp my-latex-package-list))) str & ", " | -2) & -2 & ?\] | -1
             ?{ str "}\n")
            "\n"
            "\\title{" (let ((title (read-string "title: ")))
                         (setq v1 (not (string-empty-p title)))
                         title) & "}\n" | -7
            "\\author{" (when v1
                          (read-string "author: " user-full-name)) & "}\n" | -8
            _ "\n\\begin{document}\n\n"
            (if v1
                "\\maketitle\n\n"
              "") _
            -
            "\n\n\\end{document}"))

  (define-skeleton my-insert-latex-package
    "Insert a latex package"
    (completing-read "package: " (mapcar #'car my-latex-package-list))
    '(progn
       (setq my-temp (eval str))
       nil)
    "\\usepackage["
    ((completing-read "options: " (cdr-safe (assoc my-temp my-latex-package-list))) str & ", " | -2) & -2 & ?\] | -1
    ?{ str "}\n")

  (define-key TeX-mode-map (kbd "C-c k")  #'my-insert-latex-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp

(require 'tramp)
(setf password-cache-expiry (* 60 60))

;; Finally start emacs server
(server-start)

(require 'cc-vars)

(c-add-style "my-c-style"
             '("k&r"
               (c-offsets-alist . ((innamespace . 0)
                                   (inline-open . 0)))
               (c-doc-comment-style . ((java-mode . javadoc)
                                       (pike-mode . autodoc)
                                       (c-mode . gtkdoc)
                                       (c++-mode . nil)))
               (c-basic-offset . 4)))

(setf c-default-style "my-c-style")

;; (require 'c++-doxygen-mode)
;; (add-hook 'c++-mode-hook #'c++-doxygen-mode)

(c-toggle-auto-newline 1)
(c-toggle-electric-state 1)

(setq-default c-cleanup-list
              '(brace-else-brace
                brace-elseif-brace
                brace-catch-brace
                empty-defun-braces
                one-liner-defun
                defun-close-semi
                list-close-comma
                scope-operator))

(add-to-list 'auto-mode-alist '("Sconstruct\\'" . python-mode))

(provide '.emacs)
;;; .emacs ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-extra-options "-shell-escape")
 '(custom-safe-themes
   (quote
    ("04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" "3fd0fda6c3842e59f3a307d01f105cce74e1981c6670bb17588557b4cebfe1a7" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "b747fb36e99bc7f497248eafd6e32b45613ee086da74d1d92a8da59d37b9a829" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(ede-project-directories (quote ("/home/prt/workspace/tooling")))
 '(initial-buffer-choice t)
 '(package-selected-packages
   (quote
    (helm-rtags rtags yaml-mode tide web-mode docker eslintd-fix eslint-fix rjsx-mode blacken use-package pdf-tools company-shell direnv helm helm-core company projectile elscreen clang-format modern-cpp-font-lock highlight-symbol multiple-cursors company-clang powerline package-build shut-up git commander f cask flycheck protobuf-mode helm-gtags diminish cmake-mode slime-company openwith monokai-theme magit llvm-mode helm-projectile exec-path-from-shell diredful company-anaconda bash-completion auctex alect-themes)))
 '(safe-local-variable-values
   (quote
    ((gud-gdb-command-name . "gdb-multiarch -i=mi -x gdb build/application")
     (gud-gdb-command-name . "gdb-multiarch -i=mi -x gdb build/m")
     (projectile-project-compilation-cmd . "make -k -j4")
     (projectile-project-compilation-dir . "build"))))
 '(send-mail-function (quote smtpmail-send-it)))
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-page 'disabled nil)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-top-columns ((t (:inherit helm-header :height 119)))))
