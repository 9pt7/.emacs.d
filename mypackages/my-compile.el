;;; my-compile.el --- Tools for compiling projects.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Peter Thompson

;; Author: Peter Thompson <peter.thompson92@gmail.com>
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; (require 'cl)
;; (require 'gdb-mi)
;; (require 'compile)


;; (defvar-local my-compile-dir nil)


;; (defvar my-compile-mode-line-face nil)
;; (defvar my-compile-mode-line-stage nil)
;; (defvar my-compile-mode-line-status nil)
;; (setq minor-mode-alist
;;       (cons '(my-compile-mode-line-stage
;;               (" "
;;                my-compile-mode-line-stage
;;                "["
;;                (:eval `(:propertize my-compile-mode-line-status
;;                                     face ,my-compile-mode-line-face))
;;                "]"))
;;             (reduce (lambda (alist key) (assq-delete-all key alist))
;;                     '(my-compile-mode-line-stage compilation-in-progress)
;;                     :initial-value minor-mode-alist)))


;; (defvar my-compile-makefile-regex "^[mM]akefile$")
;; (defvar my-compile-build-dir-rel-regex "^\\(\\.\\|build\\)$")

;; (message "%s" 32)
;; (defun my-compile-find-dir (test &optional relative-dirs-regex)
;;   "Guess the build directory.
;; The parents of `default-directory' are visited recursively, while searching
;; for a make-file matching `my-compile-makefile-regex' from the
;; relative paths in RELATIVE-DIRS."
;;   (let ((value nil)
;;         (dir-test (apply-partially #'string-match (or relative-dirs-regex "^\\.$"))))
;;     (cl-loop
;;      for par = default-directory then (file-name-directory (directory-file-name par))
;;      for dir = (dolist (rel (remove-if-not dir-test (directory-files par)))
;;                  (let* ((dir (expand-file-name (concat par "/" rel))))
;;                    (when (and (file-exists-p dir) (setq value (funcall test dir)))
;;                      (return dir))))
;;      until (or dir (equal par "/")))
;;     value))

;; (defclass my-project-data ()
;;   ((build-dir :initarg :build-dir :type string)
;;    (build-cmd :initarg :build-cmd :type string)
;;    (debug-cmd :initarg :debug-cmd :type string)

;;    (tests-passed :initform nil)
;;    (tests-failed :initform nil)
;;    (exec-exit :initform nil)

;;    (gud-io-point :initform nil)))
;; (defvar my-compile-command-history (make-hash-table :test 'equal))
;; (defvar-local my-local-project-data nil)
;; (defvar my-compile-compilation-buffer nil)
;; (defvar my-compile-last-project nil)
;; (defvar my-compile-last-project-data nil)


;; (defun my-compile-setup-compile-windows ()
;;   (with-current-buffer my-compile-compilation-buffer
;;     (delete-other-windows)
;;     (set-window-buffer (window-normalize-window nil) my-compile-compilation-buffer)
;;     (next-error-follow-minor-mode 1)))
;; (defvar my-compile-compile-stage (list "Comp" #'my-compile-setup-compile-windows))

;; (defun my-compile-setup-exec-windows ()
;;   (gdb-restore-windows))
;; (defvar my-compile-exec-stage (list "Exec" #'my-compile-setup-exec-windows))

;; (defvar my-compile-restore-windows-function nil)
;; (defun my-compile-set-stage (stage)
;;   (setq my-compile-mode-line-stage (first stage)
;;         my-compile-restore-windows-function (second stage))
;;   (my-compile-set-status))

;; (defvar my-compile-reverting nil)
;; (defvar my-compile-previous-window-configs nil)
;; (defun my-compile-setup-windows (revert)
;;   (interactive "P")
;;   (setq revert (or revert (and (eq real-last-command #'my-compile-setup-windows)
;;                                my-compile-reverting)))
;;   (setq my-compile-reverting revert)
;;   (message "revert: %s" revert)
;;   (if (not revert)
;;       (when my-compile-restore-windows-function
;;         (setq my-compile-previous-window-configs (cons (current-window-configuration)
;;                                                        my-compile-previous-window-configs))
;;         (funcall my-compile-restore-windows-function))
;;     (when my-compile-previous-window-configs
;;       (set-window-configuration (car my-compile-previous-window-configs))
;;       (setq my-compile-previous-window-configs (cdr my-compile-previous-window-configs)))))

;; (defvar my-compile-status-values
;;   '((nil "..." nil)
;;     (ok "done" compilation-info)
;;     (fail "FAIL" compilation-error)
;;     (break "BREAK" compilation-error)))
;; (defun my-compile-set-status (&optional status-string stat)
;;   (let* ((a (cdr (assoc stat my-compile-status-values))))
;;     (setq my-compile-mode-line-status (or status-string (first a))
;;           my-compile-mode-line-face (second a)))
;;   (force-mode-line-update t))

;; (setq gdb-display-io-nopopup t
;;       gdb-many-windows nil
;;       gdb-show-main nil)

;; (defun my-compile-compilation-finish (buffer stat)
;;   (let* ((ok (string-match "^finished" stat)))
;;     (my-compile-set-status nil (if ok 'ok 'fail))
;;     (with-current-buffer buffer
;;       (when (and ok my-local-project-data)
;;         (with-slots (build-dir build-cmd debug-cmd gud-io-point)
;;             my-local-project-data
;;           (save-window-excursion
;;             (let ((default-directory build-dir)
;;                   (gdb-comint (get-buffer-process gud-comint-buffer))
;;                   (gdb-many-windows nil)
;;                   (data my-local-project-data))
;;               (my-compile-set-stage my-compile-exec-stage)
;;               (unless gdb-comint
;;                 (gdb debug-cmd)
;;                 (setf gdb-comint (get-buffer-process gud-comint-buffer)))
;;               (let ((message-log-max nil)
;;                     (inhibit-message t))
;;                 (gud-run nil))
;;               (setf gud-io-point (with-current-buffer (gdb-get-buffer 'gdb-inferior-io)
;;                                    (point-max)))
;;               (with-current-buffer gud-comint-buffer
;;                 (setq my-local-project-data data)))))))))
;; (add-hook 'compilation-finish-functions #'my-compile-compilation-finish)

;; (defun my-compile-after-change ()
;;   (unless (search-backward-regexp "^." nil t)
;;     (goto-char (point-min)))
;;   (cond ((search-forward-regexp "^\\[ *\\([0-9]+\\)% *\\]" nil t)
;;          (my-compile-set-status (format "%3d%%" (string-to-number (match-string 1)))))))
;; (add-hook 'compilation-filter-hook #'my-compile-after-change)

;; (defun my-compile-debug-set-status (&optional stop-reason)
;;   (with-slots (gud-io-point tests-passed tests-failed exec-exit) my-compile-last-project-data
;;     (when stop-reason
;;       (pcase stop-reason
;;         ("breakpoint-hit"
;;          (my-compile-set-status nil 'break))
;;         ("exited-normally"
;;          (setf exec-exit 'ok))
;;         ("exited"
;;          (setf exec-exit 'fail))
;;         (_ (message "something else... %s" stop-reason))))
;;     (with-current-buffer (gdb-get-buffer 'gdb-inferior-io)
;;       (save-excursion
;;         (goto-char (or gud-io-point (point-min)))
;;         (when (save-excursion (search-forward-regexp "\\[ *PASSED *\\] \\([0-9]+\\) tests?" nil t))
;;           (setf tests-passed (string-to-number (match-string 1))))
;;         (when (save-excursion (search-forward-regexp "\\[ *FAILED *\\] \\([0-9]+\\) tests?" nil t))
;;           (setf tests-failed (string-to-number (match-string 1))))
;;         (setf gud-io-point (save-excursion
;;                              (goto-char (point-max))
;;                              (beginning-of-line)
;;                              (point)))))
;;     (when (or gud-running exec-exit)
;;       (my-compile-set-status
;;        (cond (tests-passed (format "%s/%s" tests-passed (+ tests-passed (or tests-failed 0)))))
;;        exec-exit))))

;; (defun my-gdb-output-filter (&rest args) (my-compile-debug-set-status))
;; (add-hook 'comint-output-filter-functions #'my-gdb-output-filter)




;; (defun my-gdb-stopped-function (response)
;;   (with-current-buffer gud-comint-buffer
;;     (let ((stop-reason (cdr (assoc 'reason response))))
;;       (my-compile-debug-set-status stop-reason))))
;; (add-hook 'gdb-stopped-functions #'my-gdb-stopped-function)


;; (defun my-compile (project-directory build-directory build-command debug-command)
;;   (interactive
;;    (let* ((force-reread current-prefix-arg)
;;           (current-prefix-arg nil)
;;           (project-dirs (let ((dirs nil))
;;                           (cl-flet ((add-dir (dir) (setq dirs (cons dir dirs))))
;;                             (condition-case nil
;;                                 (add-dir (projectile-project-root))
;;                               (error nil))
;;                             (cl-loop
;;                              for dir = (expand-file-name default-directory)
;;                              then (file-name-directory (directory-file-name dir))
;;                              do (add-dir dir)
;;                              until (equal dir "/")))
;;                           (reverse dirs)))
;;           (args nil)
;;           (project-directory
;;            (or (and (not force-reread)
;;                     (or
;;                      (find-if (lambda (dir)
;;                                 (let ((data (gethash dir my-compile-command-history)))
;;                                   (when data
;;                                     (with-slots (build-dir build-cmd debug-cmd) data
;;                                       (setq args (list build-dir build-cmd debug-cmd))))))
;;                               project-dirs)
;;                      (when my-compile-last-project-data
;;                        (with-slots (build-dir build-cmd debug-cmd) my-compile-last-project-data
;;                          (setq args (list build-dir build-cmd debug-cmd)))
;;                        my-compile-last-project)))
;;                (read-directory-name "Project Directory: "
;;                                     nil
;;                                     nil
;;                                     t
;;                                     (car project-dirs))))
;;           (default-directory project-directory))
;;      (unless args
;;        (let* ((build-directory (read-directory-name "Build Directory: "
;;                                                     nil
;;                                                     nil
;;                                                     t
;;                                                     (let ((build (concat project-directory "build/")))
;;                                                       (when (file-exists-p build) build))))
;;               (default-directory build-directory)
;;               (build-command (read-shell-command "Compile command: "
;;                                                  compile-command))
;;               (debug-command (gud-query-cmdline 'gdb)))
;;          (setq args (list build-directory build-command debug-command))))
;;      (cons project-directory args)))
;;   (let* ((data (my-project-data :build-dir build-directory
;;                                 :build-cmd build-command :debug-cmd debug-command))
;;          (default-directory build-directory)
;;          (display-buffer-overriding-action '(((lambda (buffer alist) t)))))
;;     (puthash project-directory data my-compile-command-history)
;;     (my-compile-set-stage my-compile-compile-stage)
;;     (setq my-compile-last-project project-directory
;;           my-compile-last-project-data data)
;;     (with-current-buffer (compile build-command)
;;       (setq my-compile-compilation-buffer (current-buffer))
;;       (setq my-local-project-data data))))


(provide 'my-compile)
;;; my-compile.el ends here
