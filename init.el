;;; .emacs -- My emacs config file
;;; Commentary:
;;; Code:

(setq-local lexical-binding t)

(require 'package)
(require 'cl)
(package-initialize)
(setf package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
(require 'cl)
(cl-flet ((disable (mode) (when (fboundp mode) (funcall mode -1))))
  (disable 'menu-bar-mode)
  (disable 'tool-bar-mode)
  (disable 'scroll-bar-mode))
(setf use-dialog-box nil)

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
(setf redisplay-dont-pause t)           ;Makes redisplay faster
(setq-default indent-tabs-mode nil)
(setf comment-auto-fill-only-comments t
      auto-fill-function nil)
(setq-default indent-tabs-mode nil
              align-default-spacing 2)
(add-hook 'prog-mode-hook #'turn-on-auto-fill)
(require 'eldoc)
(add-hook 'prog-mode-hook #'turn-on-eldoc-mode)

(require 'ediff)
(setq-default ediff-window-setup-function #'ediff-setup-windows-plain)

(require 'gdb-mi)
(setf gdb-show-main t)

(require 'ido)
(ido-mode 'buffers)

(dotimes (i 10)
  (global-set-key (kbd (format "M-%d" i))
                  `(lambda ()
                     (interactive)
                     (select-window (window-at 0 0))
                     (other-window (- ,i 1)))))

(let ((backup-dir (expand-file-name (concat user-emacs-directory "backups"))))
  (setf backup-directory-alist `((".*" . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,backup-dir t))))

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
(setf magit-last-seen-setup-instructions "1.4.0")

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
(global-set-key (kbd "\C-cp") #'grep)
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

(require 'bash-completion)
(bash-completion-setup)

;; Use bash as shell if it is in the path
(require 'shell)
(let ((shell (executable-find "bash")))
  (when shell
    (setf explicit-shell-file-name shell)))

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
(setf python-shell-interpreter
      (cl-flet ((exec-find (program)
                           (when (executable-find program) program)))
        (cond ((exec-find "ipython3"))
              ((exec-find "ipython"))
              ((exec-find "python3"))
              ((exec-find "python")))))

(when (or (string= python-shell-interpreter "ipython3")
          (string= python-shell-interpreter "ipython"))
  (setf python-shell-interpreter-args  "--matplotlib --classic"
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code
        "'                                   ;'.join(module_completion('''%s'''))\n"  ;
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(setq-default comment-inline-offset 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C


(require 'cc-mode)
(defun my-c-mode-hook ()
  "My C mode hook."
  (eldoc-mode -1)
  (define-key c-mode-map (kbd "M-.") 'semantic-ia-fast-jump)
  (define-key c-mode-map (kbd "C-M-.") 'semantic-symref)

  ;; C-style comments
  (setf comment-start-skip "\\(//+\\|/\\*+\\)\\s *"
        comment-start "/* "
        comment-end " */"))
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Org
(require 'org)
(defun readable-text ()
  "spell checking."
  (flyspell-mode 1))
(add-hook 'text-mode-hook 'readable-text)
(add-hook 'org-agenda-after-show-hook 'org-narrow-to-subtree)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(setf
 ;; Files
 org-default-notes-file (expand-file-name "~/Dropbox/org/notes.org")

 ;; Go to link on RET
 org-return-follows-link t

 ;; Indent text based on outline
 org-startup-indented t

 ;; Show time and add not when done
 org-log-done 'note

 ;; Start agenda on current day
 org-agenda-start-on-weekday nil

 ;; Show items with dates in the TODO buffer
 org-agenda-todo-ignore-scheduled nil
 org-agenda-todo-ignore-deadlines nil

 ;; TODO keywords
 org-todo-keywords '((type "TODO(t)" "READ(r)" "WAITING(w@)" "INPROGRESS(p@)"
                               "|" "DONE(d)" "CANCELLED(c)"))
 org-todo-keyword-faces '(("WAITING" . "orange")
                          ("INPROGRESS" . "orange"))

 ;; Capture templates
 org-capture-templates
      '(("t" "task" entry (file "~/Dropbox/org/agenda.org")
         "* TODO %^{Task Name} %^G\n/Entered on %U/\n%?")
        ("j" "journal" item (file+datetree "~/Dropbox/org/journal.org")
         "%U %?")))

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
(setf dired-listing-switches "-lAh"
      dired-dwim-target t
      dired-isearch-filenames 'dwim
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)
(require 'ibuffer)
;; Use Dired x
;; (require 'dired-x)
;; (add-hook 'dired-load-hook
;;        (lambda ()
;;          (load "dired-x")))

(add-hook 'dired-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; Macros
(global-set-key "\C-x(" 'kmacro-start-macro-or-insert-counter)


;; Use emac's ls program
(setf ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(require 'cl)

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
      (unless (file-directory-p fname)
        (recentf-add-file fname)))))

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
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'reftex-mode)

(setf preview-default-option-list '("displaymath" "floats" "graphics" "textmath")
      preview-auto-cache-preamble t
      preview-auto-reveal t
      preview-preserve-counters t
      TeX-PDF-mode t
      reftex-cite-format 'natbib
      font-latex-match-reference-keywords '(("citep" "*[{") ("citet" "*[{")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp

(require 'tramp)
(setf password-cache-expiry (* 60 60))

(require 'company)
(require 'company-anaconda)
(require 'slime-company)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
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
(global-semantic-idle-summary-mode 1)
(semantic-mode 1)

(require 'cc-vars)
(setf c-default-style "k&r"
           c-basic-offset 4)

(c-add-style "my-cc-mode"
                  '("cc-mode"
                    (c-offsets-alist . ((innamespace . 0)
                                        (inline-open . 0)))))

(defun my-cpp-setup()
  (c-set-style "my-cc-mode"))

(add-hook 'c++-mode-hook #'my-cpp-setup)

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
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-persistent-highlight t)
 '(initial-buffer-choice t)
 '(org-agenda-files (quote ("~/Dropbox/org/agenda.org"))))
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
