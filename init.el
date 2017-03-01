
;;; .emacs -- My emacs config file
;;; Commentary:
;;; Code:

(require 'cl)

(setq-local lexical-binding t)

(require 'package)
(setf package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(let ((package-list (list 'magit
                          'bash-completion
                          'company
                          'slime
                          'slime-company
                          'flycheck
                          'rw-hunspell
                          'auctex
                          'diredful
                          'helm
                          'exec-path-from-shell
                          'company-anaconda)))

  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(set-default-font "Droid Sans Mono 11")
(require 'alect-themes)
(alect-create-theme dark)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (setq exec-path-from-shell-variables '("PATH"
                                         "MANPATH"
                                         "DICTIONARY"))
  ;; Need LANG for spell check (not initialized in launcher env)
  (setenv "LANG" "en_CA.UTF-8")
  (exec-path-from-shell-initialize))

(global-set-key (kbd "s-s") #'dabbrev-expand)

;; Unbind suspend when in a separate window
(when (or (eq window-system 'x)
          (eq window-system 'ns))
  (global-unset-key (kbd "C-z")))

;; Restore marker location after jumping to a register
(defun save-all-markers-advice (orig-fun &rest args)
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

;; Display time on modeline
(require 'time)
(setq display-time-default-load-average nil
      display-time-24hr-format t
      display-time-day-and-date t)
(display-time-mode t)

(require 'man)
(setq Man-notify-method 'pushy)

(require 'pdf-tools)
(require 'pdf-view)
(add-to-list 'auto-mode-alist '("\\.\\(?:pdf\\|PDF\\)\\'" . pdf-view-mode))
(add-hook #'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
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

(require 'face-remap)
(setf text-scale-mode-step 1.1)

(setf user-full-name "Peter Thompson"
      user-mail-address "peter.thompson92@gmail.com")

(set-face-attribute 'default nil :height 105)

(setf ring-bell-function 'ignore)
(setf set-mark-command-repeat-pop t)

(require 'autoinsert)
(auto-insert-mode 1)

(setq-default require-final-newline t)
(setq-default fill-column 79)
(menu-bar-enable-clipboard)
(column-number-mode t)
(setf kill-whole-line t)
(show-paren-mode 1)
(electric-pair-mode 1)
(add-to-list 'revert-without-query ".+\\.pdf$")
(setf sentence-end-double-space t)
(setf gc-cons-threshold 20000000)       ;Makes GC faster
(setq-default indent-tabs-mode nil)
(setf comment-auto-fill-only-comments t
      auto-fill-function nil)
(setq-default indent-tabs-mode nil
              align-default-spacing 2)
(add-hook 'prog-mode-hook #'turn-on-auto-fill)
(require 'eldoc)
(add-hook 'c-mode-hook #'eldoc-mode)

(require 'ediff)
(setq-default ediff-window-setup-function #'ediff-setup-windows-plain)

(require 'gdb-mi)
(setf gdb-show-main t)
(setq gdb-many-windows t)

(dotimes (i 10)
  (global-set-key (kbd (format "M-%d" i))
                  `(lambda ()
                     (interactive)
                     (select-window (window-at 0 0))
                     (other-window (- ,i 1)))))

(setf backup-directory-alist (list (cons ".*"
                                         (expand-file-name (concat user-emacs-directory
                                                                   "backups")))))

(defun my-increment-number-at-point (amount)
  (interactive "p")
  (save-excursion
    (skip-chars-backward "0123456789")
    (unless (looking-at "[0123456789]+")
      (error "No number at point"))
    (replace-match (number-to-string (+ amount (string-to-number (match-string 0)))))))
(global-set-key (kbd "\C-c +") #'my-increment-number-at-point)

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

(require 'calc)
(setf math-additional-units
      '((GiB "1024 * MiB" "Giga Byte")
        (MiB "1024 * KiB" "Mega Byte")
        (KiB "1024 * B" "Kilo Byte")
        (B nil "Byte")
        (Gib "1024 * Mib" "Giga Bit")
        (Mib "1024 * Kib" "Mega Bit")
        (Kib "1024 * b" "Kilo Bit")
        (b "B / 8" "Bit")))

(defun my-fit-text-to-window (max-size)
  ;; After saving current window configuration, split frame horizontally and
  ;; decrease text size until the number of columns in the window is greater
  ;; than 80.
  (let ((frameset (frameset-save (list (selected-frame)))))
    (delete-other-windows)
    (split-window-horizontally)
    (cl-labels
        ((fix-text-size (start-size)
                        (set-face-attribute 'default nil :height start-size)
                        (if (< (window-body-width) 80)
                            (fix-text-size (- start-size 5))
                          start-size)))
      (let ((text-size (fix-text-size max-size)))
        (frameset-restore frameset)
        (set-face-attribute 'default nil :height text-size)
        text-size))))


(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))
    (when (frame-parameter nil 'fullscreen)
      (my-fit-text-to-window 130))))

(global-set-key [f11] 'toggle-fullscreen)

(require 'uniquify)
(setf uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'doc-view)
(require 'autorevert)
(setf auto-revert-interval 1)
(add-hook 'doc-view-mode-hook #'auto-revert-mode)
(setf doc-view-resolution 300)

(require 'recentf)
(recentf-mode t)
(setf recentf-max-saved-items 1000)

(require 'compile)

(defun my-compile ()
  "Call `compile' interactively from a chosen directory."
  (interactive)
  (let ((default-directory
          (read-directory-name
           "Compilation directory: "
           ;; Use current compilation directory if the default directory is a
           ;; subdirectory
           (if (and compilation-directory
                    (locate-dominating-file default-directory
                                            (lambda (dir)
                                              (file-equal-p dir
                                                            compilation-directory))))
               compilation-directory
             default-directory))))
    (call-interactively #'compile)))

(require 'grep)
(let ((ack-cmd (cond ((executable-find "ack-grep") "ack-grep")
                     ((executable-find "ack") "ack")))
      (ack-args " --nogroup -H "))
  (when ack-cmd
    (grep-apply-setting 'grep-command (concat ack-cmd ack-args))))

(defun grep-override (grep-cmd)
  (let ((old-cmd grep-command))
    (grep-apply-setting 'grep-command grep-cmd)
    (unwind-protect
        (call-interactively #'grep)
      (grep-apply-setting 'grep-command old-cmd))))

(defun grep-global ()
  (interactive)
  (grep-override "global --result grep -xi "))

(require 'magit)
(defvar magit-last-seen-setup-instructions "1.4.0")

(require 'files)
(defun my-revert-buffer()
  "Call `revert-buffer' without query."
  (interactive)
  (revert-buffer nil t))

(global-set-key (kbd "\C-cs") #'shell)
(global-set-key (kbd "\C-cr") #'recompile)
(global-set-key (kbd "\C-cx") #'my-compile)
(global-set-key (kbd "\C-cd") #'gdb)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "\C-cf") #'recentf-open-files)
(global-set-key (kbd "\C-cg") #'magit-status)
(global-set-key (kbd "\C-cb") #'grep-global)
(global-set-key (kbd "\C-cq") #'grep)
(global-set-key (kbd "\C-cv") #'my-revert-buffer)


(require 'org-bibtex)
(defun my-org-bibtex-transfer ()
  "Read Bibtex entry when in Bibtex mode, paste when in org mode."
  (interactive)
  (cond ((eq major-mode 'bibtex-mode) (org-bibtex-read))
        ((eq major-mode 'org-mode) (org-bibtex-write))
        (t (message "Need to be in bibtex-mode or org-mode."))))

(defun my-in-org-table-p ()
  (cl-labels ((context-in-table-p (context)
                                  (let ((type (org-element-type context)))
                                    (or (eq type 'table)
                                        (eq type 'table-cell)
                                        (eq type 'table-row)
                                        (let ((parent (org-element-property :parent context)))
                                          (when parent
                                            (context-in-table-p parent)))))))
    (context-in-table-p (org-element-context))))

(defun my-highlight-org-table-line ()
  (when (my-in-org-table-p) (hl-line-highlight)))

(defun my-unhighlight-org-table-line ()
  (when (my-in-org-table-p) (hl-line-unhighlight)))

(require 'hl-line)

(defun my-org-table-highlight ()
  (setf hl-line-sticky-flag nil)
  (hl-line-mode 1)
  (remove-hook 'post-command-hook #'hl-line-highlight t)
  (remove-hook 'pre-command-hook #'hl-line-unhighlight t)
  (add-hook 'post-command-hook #'my-highlight-org-table-line nil t)
  (add-hook 'pre-command-hook #'my-unhighlight-org-table-line nil t)
  (hl-line-unhighlight)
  (my-highlight-org-table-line))

(add-hook 'org-mode-hook #'my-org-table-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
(setenv "PAGER" "cat")
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name user-emacs-directory) "bin"))

(defadvice pwd (before kill-pwd activate)
  (kill-new default-directory))

(require 'bash-completion)
(bash-completion-setup)

;; Use bash as shell if it is in the path
(require 'shell)
(let ((shell (executable-find "bash")))
  (when shell
    (setf explicit-shell-file-name shell)))

(defun my-comint-disable-echoing ()
  (setq comint-process-echoes t))

(add-hook 'shell-mode-hook #'my-comint-disable-echoing)

(require 'company)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comint
(require 'comint)
(setf comint-completion-addsuffix t
      comint-completion-autolist t
      comint-input-ignoredups t
      ;; Do not set to non-nil; breaks clisp
      comint-process-echoes nil)

;; Makes it slightly less slow
(setf comint-move-point-for-output nil
      comint-scroll-show-maximum-output nil)

(setf comint-buffer-maximum-size 10000)
(add-hook 'comint-output-filter-functions #'comint-truncate-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME

(require 'slime)
(setf inferior-lisp-program "sbcl")
(slime-setup '(slime-company))

(defun slime-eval-region-dwim ()
  "Evaluate region if it is active, otherwise evaluate the entire buffer."
  (interactive)
  (if (region-active-p)
      (slime-eval-region (region-beginning) (region-end))
    (slime-eval-buffer)))
(define-key slime-mode-map (kbd "C-c C-r")  'slime-eval-region-dwim)

(defun close-comint-hook ()
  "Automatically close the comint buffer."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        (lambda (process event)
                          (when (string= event "finished\n")
                            (kill-buffer (current-buffer))))))
(add-hook 'comint-exec-hook #'close-comint-hook)

;; Don't use yellow or white in comint. Use orange instead
(setf ansi-color-names-vector
      ["black" "red" "DarkGreen" "DarkOrange3" "blue" "magenta" "DarkCyan" "dim gray"])
(setf ansi-color-map (ansi-color-make-color-map))

(pending-delete-mode 1)

(defun my-param-format (str)
  "Create @param statements for the parameters in `str'. It is
assumed that `str' is contained within the function argument
list."
  (let ((param-regexp
         "\\(\\_<\\(?:\\(?:\\s_\\)\\|\\(?:\\sw\\)\\)+\\_>\\)\\s-*\\(?:\\[.*\\]\\s-*\\)?\\(,\\|$\\)"))
    (when (string-match param-regexp str)
      (concat " * @param[in] " (match-string 1 str) "\n"
              (my-param-format (substring str (match-end 0)))))))

(defun my-function-format (str)
  (when (string-match "\\(.*\\)\\(\\_<.*\\)(\\(.*\\))" str)
    (let ((qualifiers (match-string 1 str))
          (name (match-string 2 str))
          (param-list (match-string 3 str)))
      (concat (my-param-format param-list)
              (when (not (string-match "\\_<void\\_>" qualifiers))
                " * @return\n")))))

(defun my-doc-align (str)
  (with-temp-buffer
    (insert str)
    (align-regexp (point-min) (point-max) "\\* @param\\[.*?\\]\\(\\s-*\\)\\_<.*?\\_>" 1 1)
    (align-regexp (point-min) (point-max) "\\(?:\\(?:\\* @param\\[.*?\\]\\s-*\\_<.*?\\_>\\)\\|\\(?:* @return\\)\\)\\(\\ *\\)" 1 2)
    (delete-trailing-whitespace (point-min) (point-max))
    (buffer-string)))

(define-skeleton my-doxygen-function
  "Insert a doxygen comment for a function."
  nil
  "/**" \n
  "* " >
  (skeleton-read "Description: ") & \n & "*" & \n | -2
  >
  '(setf v1 (point))
  ("Direction: " "* @param[" str "]" " "
   (skeleton-read "Parameter: ") " "
   (skeleton-read "Description: ")
   \n
   '(align-regexp v1 (point) "\\* @param\\[.*?\\]\\(\\s-*\\)\\_<" 1 1)
   '(align-regexp v1 (point) "\\* @param\\[.*?\\]\\s-*\\_<.*?\\_>\\( *\\)" 1 2))
  "* @return " >
  (skeleton-read "Return: ") & \n | -10
  resume:
  "*/" >)

(define-skeleton my-doxygen-function-sans-params
  "Insert a doxygen comment for a function."
  nil
  "/**" \n
  "* " > - \n
  "*" \n
  "* @return " >
  (skeleton-read "Return: ") & \n | -10
  resume:
  "*/" >)

(defun my-doxygen-variable ()
  (interactive)
  (let ((comment-start "/**< ")
        (comment-end " */"))
    (comment-dwim nil)))

(global-set-key (kbd "\C-c M-;") 'my-doxygen-function)
(global-set-key (kbd "\C-c;") 'my-doxygen-variable)


(require 'asm-mode)
(setq asm-comment-char ?\@)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL
(require 'sql)
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(let ((sqlite (cond ((executable-find "sqlite3"))
                    ((executable-find "sqlite")))))
  (when sqlite
    (setq-default sql-sqlite-program sqlite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp
(eval-after-load 'lisp-mode
  '(define-key lisp-mode-map (kbd "C-c C-c") 'eval-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;; (require 'jedi)
(require 'python)
(setq-default python-shell-interpreter
              (cl-flet ((exec-find (program)
                                   (when (executable-find program) program)))
                (cond ((exec-find "ipython3"))
                      ((exec-find "ipython"))
                      ((exec-find "python3"))
                      ((exec-find "python")))))

(when (or (string= python-shell-interpreter "ipython3")
          (string= python-shell-interpreter "ipython"))
  (setq-default python-shell-interpreter-args  "--matplotlib --classic"
                python-shell-completion-setup-code
                "from IPython.core.completerlib import module_completion"
                python-shell-completion-string-code
                "'                                   ;'.join(module_completion('''%s'''))\n"  ;
                python-shell-completion-string-code
                "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(setq-default comment-inline-offset 2)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'cc-mode)

(add-hook 'c++-mode-hook
          (defun my-c++-flycheck-hook ()
            ;; Use clang checker on OS-X
            ;; (flycheck-select-checker (case system-type
            ;;                            ('darwin 'c/c++-clang)
            ;;                            (t 'c/c++-gcc)))
            (setq-local flycheck-clang-language-standard "c++14")
            (setq-local flycheck-gcc-language-standard "c++14")))

(add-hook 'c-mode-hook
          (defun my-c-flycheck-hook()
            ;; (flycheck-select-checker 'c/c++-gcc)
            (setq-local flycheck-clang-language-standard "gnu11")
            (setq-local flycheck-gcc-language-standard "gnu11")))

(require 'company-clang)
(add-hook 'c++-mode-hook
          (defun my-c++-company-hook ()
            (setq-local company-clang-arguments '("-std=c++11")
            (setq-local flycheck-clang-language-standard "c++14")
            (setq-local flycheck-gcc-language-standard "c++14"))))
(add-hook 'c-mode-hook
          (defun my-c-company-hook ()
            (setq-local company-clang-arguments '("-std=gnu11"))
            (setq-local flycheck-clang-language-standard "c++14")
            (setq-local flycheck-gcc-language-standard "c++14")))

(require 'irony)
(require 'flycheck-irony)
(require 'company-irony)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

(current-local-map)
c++-mode-map
(require 'cc-mode)
(defun my-c-mode-hook ()
  "My C mode hook."
  (eldoc-mode -1)
  (define-key (current-local-map) (kbd "M-.") #'semantic-ia-fast-jump)
  (define-key (current-local-map) (kbd "C-M-.") #'semantic-symref))
(add-hook 'c-mode-common-hook #'my-c-mode-hook)

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

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (up-list -1)
    (or (looking-back "enum[ \t]+class[ \t]+\\_<.+?\\_>[ \t]*")
        (looking-back "enum[ \t]+\\(?:class[ \t]+\\)?[ \t]*\\_<.*?\\_>[ \t]*:[ \t]*\\_<.*?\\_>[ \t]*"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "[ \t]*}"))
          '-
        0)
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "[ \t]*}"))
          '-
        0)
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

(defun my/update-gtags ()
  "Update gtags for the current buffer file."
  (interactive)
  (cl-flet ((call (prog &rest args)
                  (with-temp-buffer
                    (unless (zerop (apply #'call-process prog nil t nil args))
                      (error (replace-regexp-in-string "\n+$"
                                                       ""
                                                       (buffer-string)))))))
    (when (buffer-file-name)
      (call "global" "--print-dbpath")	; Make sure we are in a gtags project.
      (call "global" "-u"))))

(defun my/update-gtags-on-save-hook ()
  "Update GTAGS for the current file."
  (when (or (equal major-mode 'c++-mode)
            (equal major-mode 'c-mode))
    (my/update-gtags)))

(add-hook 'after-save-hook #'my/update-gtags-on-save-hook)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(require 'cc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
(require 'org)
(require 'org-capture)
(require 'org-agenda)

(require 'flyspell)

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(add-hook 'text-mode-hook #'flyspell-mode)
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

(global-set-key "\C-cl" 'org-store-link)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spelling
(require 'ispell)
(require 'rw-hunspell)
(let ((hunspell (executable-find "hunspell")))
  (when hunspell
    (setenv "DICTIONARY" "en_US")
    (add-to-list 'ispell-local-dictionary-alist '("en_US"
                                                  "[[:alpha:]]"
                                                  "[^[:alpha:]]"
                                                  "[']"
                                                  t
                                                  ("-d" "en_US")
                                                  nil
                                                  iso-8859-1))
    (setq ispell-program-name hunspell)
    (ispell-change-dictionary "en_US" t)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
(require 'dired-aux)
(setf dired-listing-switches "-lAh"
      dired-dwim-target t
      dired-isearch-filenames 'dwim
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)
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

(require 'diredful)
(diredful-mode 1)


(setq dired-omit-files (mapconcat #'identity
                                  '("^\\.DS_Store$"
                                    "^\\.localized$"
                                    "^\\.com.apple.timemachine.supported$")
                                  "\\|"))
;; (add-hook 'dired-mode-hook #'dired-omit-mode)

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
            ((t (error "Unknown system type: %s" system-type))))
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
(require 'preview)
(require 'reftex)
(require 'latex)
(require 'font-latex)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

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
        (?\C-b "\\textbf{"     "}" "\\bm{"     "}")  ;Use bm
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

(define-key TeX-mode-map (kbd "C-c k")  #'my-insert-latex-package)

(require 'helm)
(global-set-key (kbd "C-x C-w") #'write-file)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-c i") #'helm-mini)
(global-set-key (kbd "M-x") #'helm-M-x)
(require 'helm-mode)
(setq helm-completing-read-handlers-alist
      '((describe-function . helm-completing-read-symbols)
        (describe-variable . helm-completing-read-symbols)
        (completion-at-point . nil))
      helm-mode-handle-completion-in-region nil)
(helm-mode 1)

(require 'projectile)
(projectile-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Irony
(require 'irony)
(require 'flycheck-irony)
(require 'company-irony)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook #'my-irony-mode-hook)
(add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(eval-after-load 'company
  '(add-to-list 'company-backends #'company-irony))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp

(require 'tramp)
(setf password-cache-expiry (* 60 60))

(require 'company)
(require 'company-anaconda)
(require 'slime-company)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
;; eldoc doesn't play well with flycheck
;; (add-hook 'python-mode-hook 'eldoc-mode)
(add-to-list 'company-backends 'company-anaconda)
(setf company-idle-delay 0.3
      company-minimum-prefix-length 1
      company-show-numbers t
      company-require-match nil)
(add-hook 'prog-mode-hook 'company-mode-on)


;; Finally start emacs server
(server-start)
(setenv "EDITOR" "emacsclient")
;; (setenv "PAGER" "cat /dev/stdin > /tmp/epage; emacsclient -n /tmp/epage")

(require 'semantic)
;; Does not work well with flycheck
;; (global-semantic-idle-summary-mode 1)
(semantic-mode 1)

(require 'cc-vars)

(c-add-style "my-c-style"
             '("k&r"
               (c-offsets-alist . ((innamespace . 0)
                                   (inline-open . 0)))
               (c-doc-comment-style . ((java-mode . javadoc)
                                       (pike-mode . autodoc)
                                       (c-mode . gtkdoc)
                                       (c++-mode . gtkdoc)))
               (comment-start . "/* ")
               (comment-end . " */")
               (c-basic-offset . 4)))

(setf c-default-style "my-c-style")

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

;; Fix redraw error in virtualbox
;; (defun my-scroll-hook (window pos)
;;   (redraw-display))

;; (defun my-window-size-hook (frame)
;;   (redraw-display))

;; (add-to-list 'window-scroll-functions #'my-scroll-hook)
;; (add-hook 'window-configuration-change-hook #'redraw-display)
;; (remove-to-list 'window-size-change-functions #'my-window-size-hook)

;; Needs to be done twice for some reason
;; (dotimes (i 2)
;;   (set-face-attribute 'default nil :font "Menlo Regular"
;;                       :height 140))

;; (diredful-apply (mapconcat (lambda (file-regexp)
;;                              (concat "\\(?:" file-regexp "\\)"))
;;                            (remove-if-not #'stringp
;;                                           (mapcar #'car auto-mode-alist))
;;                            "\\|")
;;                 font-lock-comment-face nil t)

;; (add-to-list 'dired-font-lock-keywords
;;              (list
;;               (concat "\\("
;;                       (mapconcat (lambda (file-regexp)
;;                                    (concat "\\(?:" file-regexp "\\)"))
;;                                  (remove-if-not #'stringp
;;                                                 (mapcar #'car auto-mode-alist))
;;                                  "\\|")
;;                       "\\)")
;;               '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))))

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
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-persistent-highlight t)
 '(initial-buffer-choice t)
 '(package-selected-packages
   (quote
    (cmake-mode slime-company rw-hunspell pdf-tools openwith monokai-theme magit llvm-mode helm-projectile flycheck-irony exec-path-from-shell diredful company-irony company-anaconda bash-completion auctex alect-themes))))
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
