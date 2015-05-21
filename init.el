;;; .emacs -- My emacs config file
;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq use-dialog-box nil)

;; Better mouse scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

(require 'face-remap)
(setq text-scale-mode-step 1.1)

(setq user-full-name "Peter Thompson"
      user-mail-address "peter.thompson92@gmail.com")

(set-face-attribute 'default nil :height 105)

(auto-insert-mode 1)
(setq set-mark-command-repeat-pop t
      ring-bell-function 'ignore)
(setq require-final-newline t)
(setq-default fill-column 79)
(menu-bar-enable-clipboard)
(column-number-mode t)
(setq kill-whole-line t)
(show-paren-mode 1)
(electric-pair-mode 1)
(add-to-list 'revert-without-query ".+\\.pdf$")
(setq sentence-end-double-space t)
(setq gc-cons-threshold 20000000)
(setq redisplay-dont-pause t)
(setq-default indent-tabs-mode nil)
(setq comment-auto-fill-only-comments t
      auto-fill-function nil)
(setq indent-tabs-mode nil
      align-default-spacing 2)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(require 'eldoc)
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)

(setq ediff-setup-windows-default 'ediff-setup-windows-plain)

(require 'gdb-mi)
(setq gdb-show-main t)

(require 'ido)
(ido-mode 'buffers)

(dotimes (i 10)
  (global-set-key (kbd (format "M-%d" i))
                  (eval
                   `(lambda ()
                      (interactive)
                      (select-window (window-at 0 0))
                      (other-window (- ,i 1))))))

(let ((backup-dir (expand-file-name "~/backup")))
  (setq backup-directory-alist `(("." . ,backup-dir))
        backup-directory-alist
        `((".*" . ,backup-dir))
        auto-save-file-name-transforms
        `((".*" ,backup-dir t))))

;; Swap keys for regex and regular isearch/replace
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)
(setq isearch-allow-scroll t)

(defun my-exchange-point-and-mark (&optional arg)
  "Change when region is activated in `exchange-point-and-mark'.
The state of the region will not altered without a prefix ARG,
otherwise it is enabled."
  (interactive "p")
  (exchange-point-and-mark (and (not (use-region-p))
                                (= 1 arg))))

(global-set-key (kbd "C-x C-x") 'my-exchange-point-and-mark)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'calc)
(setq math-additional-units '(
  (GiB "1024 * MiB" "Giga Byte")
  (MiB "1024 * KiB" "Mega Byte")
  (KiB "1024 * B" "Kilo Byte")
  (B nil "Byte")
  (Gib "1024 * Mib" "Giga Bit")
  (Mib "1024 * Kib" "Mega Bit")
  (Kib "1024 * b" "Kilo Bit")
  (b "B / 8" "Bit")))

(define-skeleton my-configure-ac-skeleton
  "Simple configure.ac skeleton."
  nil
  "AC_INIT([" (skeleton-read "Package name: ") "], [1.0], [" user-mail-address "])\n"
  "AM_INIT_AUTOMAKE([-Wall -Werror foreign])\n"
  "AC_PROG_CC\n"
  "AC_CONFIG_HEADERS([config.h])\n"
  "AC_CONFIG_FILES([\n"
  " Makefile\n"
  " src/Makefile\n"
  "])\n"
  "AC_OUTPUT\n")

(add-to-list 'auto-insert-alist '("configure.ac" . my-configure-ac-skeleton))

(define-skeleton my-makefile-am-skeleton
  "Simple Makefile.am skeleton."
  nil
  '(setq v1 nil)
  "bin_PROGRAMS = " ("Program name: " str " " '(push str v1)) -1 \n
  '(setq v1 (nreverse v1))
  ((skeleton-read "Program name: " (pop v1)) str "_SOURCES = " ("Source name: " str " ") -1 \n)
  "SUBDIRS = "  ("Subdirectory: " str " ") & -1 | -10 \n)

(add-to-list 'auto-insert-alist '("[Mm]akefile.am" . my-makefile-am-skeleton))

(defun pop-mark2 ()
  "Pop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (push (copy-marker (mark-marker)) mark-ring)
  (let ((last-mark (car (last mark-ring))))
    (set-marker (mark-marker) (+ 0 last-mark) (current-buffer))
    (move-marker last-mark nil)
    (if (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))))

(defun pop-to-mark-command2 ()
  "Jump to mark, and pop a new position for mark off the ring.
\(Does not affect global mark ring\)."
  (interactive)
  (if (null (mark t))
      (error "No mark set in this buffer")
    (if (= (point) (car (last mark-ring 2)))
        (message "Mark popped"))
    (goto-char (or (car (last mark-ring 2)) (mark t)))
    (pop-mark2)))

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

;; (global-set-key (kbd "M-n") 'pop-to-mark-command2)
;; (global-set-key (kbd "M-p") 'pop-to-mark-command)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'doc-view)
(require 'autorevert)
(setq auto-revert-interval 1)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(remove-hook 'doc-view-mode-hook 'doc-view-fit-page-to-window)
(setq doc-view-resolution 300)

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 1000)

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
                    (locate-dominating-file
                     default-directory
                     (lambda (dir)
                       (file-equal-p dir compilation-directory))))
               compilation-directory
             default-directory))))
    (call-interactively 'compile)))

(require 'grep)
(let ((ack-cmd
       (cond ((executable-find "ack-grep") "ack-grep")
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
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'files)
(defun my-revert-buffer()
  "Call `revert-buffer' without query."
  (interactive)
  (revert-buffer nil t))

(global-set-key "\C-cs" 'shell)
(global-set-key "\C-cr" 'recompile)
(global-set-key "\C-cx" 'my-compile)
(global-set-key "\C-cd" 'gdb)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-cf" 'recentf-open-files)
(global-set-key "\C-cg" 'magit-status)
(global-set-key "\C-cb" 'grep-global)
(global-set-key "\C-cp" 'grep)
(global-set-key (kbd "\C-cv") 'my-revert-buffer)


(require 'org-bibtex)
(defun my-org-bibtex-transfer ()
  "Read Bibtex entry when in Bibtex mode, paste when in org mode."
  (interactive)
  (cond ((eq major-mode 'bibtex-mode) (org-bibtex-read))
        ((eq major-mode 'org-mode) (org-bibtex-write))
        (t (message "Need to be in bibtex-mode or org-mode."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
(setenv "PAGER" "cat")

(require 'bash-completion)
(bash-completion-setup)

;; Use bash as shell if it is in the path
(require 'shell)
(let ((shell (executable-find "bash")))
  (when shell
    (setq explicit-shell-file-name shell)))

(require 'grep)
(let ((ack-cmd
       (or (executable-find "ack-grep")
	   (executable-find "ack")))
      (ack-args " --nogroup -H "))
  (when ack-cmd
      (grep-apply-setting 'grep-command (concat ack-cmd ack-args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comint
(require 'comint)
(setq
 comint-completion-addsuffix t
 comint-completion-autolist t
 comint-input-ignoredups t
 ;; Do not set to non-nil; breaks clisp
 comint-process-echoes nil)

;; Makes it slightly less slow
(setq comint-move-point-for-output nil
      comint-scroll-show-maximum-output nil)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
;; (add-hook 'comint-mode-hook (lambda () (text-scale-set -2)))


(defun close-comint-hook ()
  "Automatically close the comint buffer."
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
                (kill-buffer ,buff))))))

(add-hook 'comint-exec-hook 'close-comint-hook)


;; Don't use yellow or white in comint. Use orange instead
(setq ansi-color-names-vector
      ["black" "red" "DarkGreen" "DarkOrange3" "blue" "magenta" "DarkCyan" "dim gray"])
(setq ansi-color-map (ansi-color-make-color-map))

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
  '(setq v1 (point))
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



(defvar my-doc-state nil)

;; (defun my-complete-doc ()
;;   (message "testing")
;;   (cond ((eq my-doc-state 'expand-doc)
;;          (yas-expand-snippet
;;           (replace-regexp-in-string
;;            "/\\*\\*\n \\* Description"
;;            "/**\n * ${1:Description}"
;;            (let* ((n 2)
;;                   (param-regexp "^ \\* @param\\[in\\] \\(.*\\)$")
;;                   (param-replaced
;;                    (replace-regexp-in-string
;;                     param-regexp
;;                     (lambda (match)
;;                       (prog1
;;                           (format " * @param[${%d:in}] %s $%d"
;;                                   n
;;                                   (progn
;;                                     (string-match param-regexp match)
;;                                     (match-string 1 match))
;;                                   (1+ n))
;;                         (setq n (+ 2 n))))
;;                     (buffer-substring yas-snippet-beg yas-snippet-end))))
;;              (replace-regexp-in-string
;;               "\\* @return"
;;               (format "* @return $%d"
;;                       (prog1
;;                           n
;;                         (setq n (1+ n))))
;;               param-replaced)))
;;           yas-snippet-beg (point))
;;          (setq my-doc-state 'align))
;;         ((eq my-doc-state 'align)
;;          (let ((aligned (my-doc-align
;;                          (buffer-substring yas-snippet-beg
;;                                            yas-snippet-end))))
;;            (goto-char yas-snippet-beg)
;;            (delete-region yas-snippet-beg yas-snippet-end)
;;            (insert aligned)
;;          (setq my-doc-state nil)))))

;; (add-hook 'yas-after-exit-snippet-hook 'my-complete-doc)

;; (defun my-function-doc ()
;;   (interactive)
;;   (let ((yas-good-grace nil))
;;     (yas-expand-snippet "/**
;;  * Description
;; ${1:$(my-function-format yas-text)} */
;; $1")
;;     (setq my-doc-state 'expand-doc)))

;; (defun my-file-doc-with-name ()
;;   (interactive)
;;   (let ((yas-good-grace nil)
;;         (base-dir
;;          (with-temp-buffer
;;            (when (eq 0 (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
;;              (replace-regexp-in-string "\n+$" "" (buffer-string))))))
;;     (yas-expand-snippet
;;      (format "/**
;;  * @file %s
;;  *
;;  * $0
;;  */
;; "
;;              (if base-dir
;;                  (file-relative-name buffer-file-name base-dir)
;;                (file-name-nondirectory (buffer-file-name)))))))

;; (defun my-file-doc-without-name ()
;;   (interactive)
;;   (let ((yas-good-grace nil))
;;     (yas-expand-snippet "/**
;;  * @file
;;  *
;;  * $0
;;  */")))


;; (defun my-doc-dwim ()
;;   (interactive)
;;   (if (eq (point) 1)
;;       (my-file-doc-without-name)
;;     (my-function-doc)))

;; (global-set-key (kbd "\C-cm") 'my-doc-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL
(require 'sql)
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(setq sql-sqlite-program "/usr/bin/sqlite3")
(setq-default sql-sqlite-program "/usr/bin/sqlite3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp
(eval-after-load 'lisp-mode
  '(define-key lisp-mode-map (kbd "C-c C-c") 'eval-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;; (require 'jedi)
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args "--matplotlib --classic"
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "'                                   ;'.join(module_completion('''%s'''))\n" ;
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(setq-default comment-inline-offset 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C
(require 'gtags)
(define-key gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
(define-key gtags-select-mode-map (kbd "o") 'gtags-select-tag-other-window)
(define-key gtags-select-mode-map (kbd "q") '(lambda () (interactive) (kill-buffer)))


(require 'cc-mode)
(defun my-c-mode-hook ()
  "My C mode hook."
  (gtags-mode 1)
  (eldoc-mode 1)
  (define-key c-mode-map (kbd "M-.") 'gtags-find-tag)
  (define-key c-mode-map (kbd "M-*") 'gtags-pop-stack)

  ;; C-style comments
  (setq comment-start-skip "\\(//+\\|/\\*+\\)\\s *"
      comment-start "/* "
      comment-end " */"))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

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
        (when (not (string-equal guard expected))
          (message "Warning: header guard not consistent with filename."))))))

(defun my-header-hook ()
  "Header file save hook."
  (when (string-equal (file-name-extension buffer-file-name) "h")
    (my-check-header-guards)))

(add-hook 'after-save-hook #'my-header-hook)

;; Fix c++ enum

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
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
  (cl-flet ((call (program &rest args)
                  (with-temp-buffer
                    (unless (zerop (apply 'call-process program nil t nil args))
                      (error (replace-regexp-in-string "\n+$" "" (buffer-string)))))))
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
(setq
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
(require 'rw-hunspell)
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary-alist
      '(
        (nil       "[A-Za-z]" "[^A-Za-z]" "" nil ("-d" "en_US") nil utf-8)
        ("american" "[A-Za-z]" "[^A-Za-z]" "" nil ("-d" "en_US") nil utf-8)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
(setq dired-listing-switches "-lAh"
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
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(require 'cl)

(defun open-in-external-app (file-name)
  "Open the file given by FILE-NAME in external app.
The app is chosen from your OS's preference."
  (interactive (list (read-file-name "File: " nil nil t "")))
  (let ((abs-file-name (expand-file-name file-name)))
    (cond
     ((string-equal system-type "windows-nt")
      (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" abs-file-name t t)))

     ((string-equal system-type "darwin")
      (shell-command (format "open \"%s\"" abs-file-name)))

     ((string-equal system-type "gnu/linux")
      (let ((process-connection-type nil))
        (start-process "" nil "xdg-open" abs-file-name)))

     ((t (error "Unknown system type: %s" system-type))))))

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

(setq preview-default-option-list '("displaymath" "floats" "graphics" "textmath")
      preview-auto-cache-preamble t
      preview-auto-reveal t
      preview-preserve-counters t
      TeX-PDF-mode t
      reftex-cite-format 'natbib
      font-latex-match-reference-keywords '(("citep" "*[{") ("citet" "*[{")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp

(require 'tramp)
(setq password-cache-expiry (* 60 60))

(require 'company)
(require 'company-anaconda)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-to-list 'company-backends 'company-anaconda)
(setq company-idle-delay 0.3
      company-minimum-prefix-length 1
      company-show-numbers t
      company-require-match nil)
(add-hook 'prog-mode-hook 'company-mode-on)

;; Finally start emacs server
(server-start)
(setenv "EDITOR" "emacsclient")
;; (setenv "PAGER" "cat /dev/stdin > /tmp/epage; emacsclient -n /tmp/epage")

(require 'cc-vars)
(setq c-default-style "k&r"
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


;; Scratch

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
