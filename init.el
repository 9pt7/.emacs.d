
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
(abbrev-mode -1)

(require 'face-remap)
(setq text-scale-mode-step 1.1)

(setq user-full-name "Peter Thompson"
      user-mail-address "peter.thompson92@gmail.com")

(push (expand-file-name "~/.emacs.d/manual") load-path)
(require 'fic-mode)			; Highlights TODO keywords
(add-hook 'prog-mode-hook #'turn-on-fic-mode)
(face-spec-set 'font-lock-fic-face
			'((t (:background nil
					  :foreground "red3"
					  :weight bold))))


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
(require 'flycheck)
(global-flycheck-mode 0)
(add-to-list 'revert-without-query ".+\\.pdf$")
(setq sentence-end-double-space t)
(setq gc-cons-threshold 20000000)
(setq redisplay-dont-pause t)
(pending-delete-mode 1)
(setq indent-tabs-mode nil)
(setq comment-auto-fill-only-comments t
      auto-fill-function nil)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(require 'eldoc)
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)

(require 'gdb-mi)
(setq gdb-many-windows t)
(windmove-default-keybindings)
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

(defun my/gdb ()
  "Run gdb and restore windows."
  (interactive)
  (call-interactively 'gdb)
  (gdb-restore-windows))

(require 'compile)

(defun my-compile ()
  "Call `compile' interactively from a chosen directory."
  (interactive)
  (let ((default-directory
	  (read-directory-name
	   "Compilation directory: "
	   ;; Use current compilation directory if the default directory is a subdirectory
	   (if (and compilation-directory
		    (locate-dominating-file
		     default-directory
		     (lambda (dir)
		       (file-equal-p dir compilation-directory))))
	       compilation-directory
	     default-directory))))
    (call-interactively 'compile)))

(let ((ack-cmd
       (cond ((executable-find "ack-grep") "ack-grep")
	     ((executable-find "ack") "ack")))
      (ack-args " --nogroup -H "))
  (when ack-cmd
    (grep-apply-setting 'grep-command (concat ack-cmd ack-args))))

(defun grep-override (grep-cmd)
  (let ((old-cmd grep-command))
    (grep-apply-setting 'grep-command grep-cmd)
    (call-interactively #'grep)
    (grep-apply-setting 'grep-command old-cmd)))

(defun grep-global ()
  (interactive)
  (grep-override "global --result grep -xi "))

(global-set-key "\C-ce" 'flycheck-list-errors)
(global-set-key "\C-cs" 'shell)
(global-set-key "\C-cr" 'recompile)
(global-set-key "\C-cx" 'my-compile)
(global-set-key "\C-cd" 'my/gdb)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-cf" 'recentf-open-files)
(global-set-key "\C-cg" 'magit-status)
(global-set-key "\C-cb" 'grep-global)
(global-set-key "\C-cp" 'grep)

(require 'org-bibtex)
(defun my-org-bibtex-transfer ()
  "Read Bibtex entry when in Bibtex mode, paste when in org mode."
  (interactive)
  (cond ((eq major-mode 'bibtex-mode) (org-bibtex-read))
	((eq major-mode 'org-mode) (org-bibtex-write))
	(t (message "Need to be in bibtex-mode or org-mode."))))
(global-set-key "\C-cb" 'my-org-bibtex-transfer)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;; (require 'helm)
;; (require 'helm-config)

;; (global-set-key (kbd "C-c m") 'helm-semantic-or-imenu)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (define-key helm-map (kbd "C-z")  'helm-select-action)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time modeline
;; (require 'time)
;; (setq display-time-day-and-date t
;;       display-time-load-average-threshold 5.0
;;       display-time-format "%a %d-%m,%H:%M")
;; (display-time)

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
(add-hook 'comint-mode-hook (lambda () (text-scale-set -2)))

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
;; Auto-Complete
;; (require 'auto-complete)
;; (setq ac-auto-start 2
;;       ac-override-local-map nil
;;       ac-use-menu-map t
;;       ac-candidate-limit 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encryption
;; (require 'epa-file)
;; (epa-file-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;; (require 'jedi)
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args "--matplotlib --classic"
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n" ;
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(setq-default comment-inline-offset 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C
(require 'cc-mode)
(defun my-c-mode-hook ()
  "My C mode hook."
  (semantic-mode 1)
  (semantic-idle-summary-mode 1)
  (eldoc-mode -1)

  ;; C-style comments
  (setq comment-start-skip "\\(//+\\|/\\*+\\)\\s *"
      comment-start "/* "
      comment-end " */"))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)
(define-key c-mode-map (kbd "M-C-.") 'semantic-symref-symbol)
(define-key c++-mode-map (kbd "M-C-.") 'semantic-symref-symbol)

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


;; Make \\[pop-tag-mark] work with \\[semantic-ia-fast-jump].
(require 'etags)
(unless (fboundp 'push-tag-mark)
  (defun push-tag-mark ()
    "Push the current position to the ring of markers so that
    \\[pop-tag-mark] can be used to come back to current position."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))))

(defun my/update-gtags ()
  "Update gtags for the current buffer file."
  (interactive)
  (cl-flet ((call (program &rest args)
		  (with-temp-buffer
		    (unless (zerop (apply 'call-process program nil t nil args))
		      (error (string-trim (buffer-string)))))))
    (when (buffer-file-name)
      (call "global" "--print-dbpath")	; Make sure we are in a gtags project.
      (call "global" "-u"))))

(defun my/update-gtags-on-save-hook ()
  "Update GTAGS for the current file."
  (when (or (equal major-mode 'c++-mode)
	    (equal major-mode 'c-mode))
    (my/update-gtags)))

(add-hook 'after-save-hook #'my/update-gtags-on-save-hook)


(defun my/flycheck-on-save-hook ()
  "Update GTAGS for the current file."
  (when (and (memq 'flycheck-mode minor-mode-list)
	     flycheck-mode)
    (flycheck-buffer)))

(add-hook 'before-save-hook #'my/flycheck-on-save-hook)

(require 'cc-mode)
(define-key c-mode-base-map (kbd "M-.") 'semantic-ia-fast-jump)
(define-key c-mode-base-map (kbd "C-c o") 'semantic-ia-show-doc)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Org
(require 'org)
(defun readable-text ()
  "Variable width font, spell checking."
  (visual-line-mode 1)
  (flyspell-mode 1)
  (setq cursor-type 'box)
  ;; (variable-pitch-mode 1)
  (setq line-spacing 5)
  (text-scale-set 0)
  ;; Turn off variable width font in tables for better alignment
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))
(add-hook 'text-mode-hook 'readable-text)
(add-hook 'org-agenda-after-show-hook 'org-narrow-to-subtree)

;; (require 'org-latex)
;; (unless (boundp 'org-export-latex-classes)
;;   (setq org-export-latex-classes nil))
;; (add-to-list 'org-export-latex-classes
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")))

;; (add-to-list 'TeX-view-program-list (cons "Preview" "open -a Preview %s.pdf"))
;; (setq TeX-view-program-selection (cons ))

;; (defun my-org-goto-and-narrow (show-all)
;;   "Run org-agenda-goto, and narrow the resulting subtree if SHOW-ALL is nil."
;;   (interactive "P")
;;   (if show-all
;;       (org-agenda-goto t)
;;     (org-agenda-goto nil)
;;     (org-narrow-to-subtree)))
;; (eval-after-load 'org-agenda
;;   '(define-key org-agenda-mode-map [(tab)] 'my-org-goto-and-narrow))
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
 org-todo-keywords '((type "TODO(t)" "READ(r)" "WAITING(w@)"
			       "|" "DONE(d)" "CANCELLED(c)"))
 org-todo-keyword-faces '(("WAITING" . "orange"))

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
;; 	  (lambda ()
;; 	    (load "dired-x")))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (toggle-truncate-lines t)
	    (text-scale-set -2)))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME

(require 'slime)
(setq inferior-lisp-program "sbcl")
(slime-setup)

(defun slime-eval-region-dwim ()
  "Evaluate region if it is active, otherwise evaluate the entire buffer."
  (interactive)
  (if (region-active-p)
      (slime-eval-region (region-beginning) (region-end))
    (slime-eval-buffer)))
(define-key slime-mode-map (kbd "C-c C-r")  'slime-eval-region-dwim)

;; (require 'projectile)
;; (projectile-global-mode)
;; (setq projectile-enable-caching t)
;; (setq projectile-mode-line '(:eval
;; 			     (format " Prj[%s]"
;; 				     (projectile-project-name))))

;; (require 'helm-projectile)
;; (helm-projectile-on)

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
;; (add-to-list 'load-path "~/.emacs.d/themes")
;; (add-to-list 'load-path "~/.emacs.d/pymacs")

;; (require 'pymacs)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")
;; (pymacs-load "ropemacs" "rope-")

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
  (c-set-style "my-cc-mode")
  (setq flycheck-clang-language-standard "c++11"
	flycheck-gcc-language-standard "c++11"))

(setq flycheck-clang-language-standard "c++11"
	flycheck-gcc-language-standard "c++11")

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


;; ;; Yas-snippet
;; (require 'yasnippet)
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets" yas-installed-snippets-dir))
;; (dolist (dir yas-snippet-dirs)
;;   (yas-load-directory (expand-file-name (if (symbolp dir) (eval dir) dir))))

;; (defun my/prev-test-case ()
;;   "Search for the previous gtest test case.  Return nil if not found."
;;   (save-excursion
;;     (let ((search-str "TEST(\\(\\(\\s_\\|\\s.\\|\\sw\\)+\\), *\\(\\s_\\|\\s.\\|\\sw\\)+)"))
;;       (when (search-backward-regexp search-str nil t)
;; 	  (substring-no-properties (match-string 1))))))

;; Scratch

(setq initial-scratch-message "")
(defun init-scratch-buffer ()
  "Initialize the scratch buffer."
  (with-current-buffer (get-buffer "*scratch*")
    (text-mode)))
(init-scratch-buffer)

(provide '.emacs)
;;; .emacs ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-extra-options "-shell-escape")
 '(custom-safe-themes (quote ("fac19bbfd0257ca0257f55b8284ead2b55cc156b9b0204e15cfd6587f0066ddd" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "b5483a0dc11b5cd3a49a19c2b7e9cc85c3764fb96457f540c34bd5dbf6e32bfc" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
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
