;;; c++-doxygen-mode.el --- Doxygen highlighting and commenting for C++.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Peter Thompson

;; Author: Peter Thompson <peter.thompson92@gmail.com>
;; Keywords: extensions

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

(require 'compile)
(require 'cc-fonts)
(require 'xwidget)

(defun c++-doxygen-font-lock-match-data ()
  "Apply font lock faces to the region defined by match data."
  (let ((doc-face c-doc-face-name)
        (markup-face c-doc-markup-face-name)
        (p (+ (match-beginning 0) 3)))
    (c-put-font-lock-face (match-beginning 0) (match-end 0) doc-face)
    (save-restriction
      (narrow-to-region p (match-end 0))
      (save-match-data
        ;; Highlight @foo and \bar keywords.
        (goto-char p)
        (while (re-search-forward "\\([@\\\\]\\)\\([a-zA-z]*\\)" nil t)
          (c-put-font-lock-face (match-beginning 2) (match-end 2) markup-face))
        ;; Highlight HTML tags.
        (goto-char p)
        (while (re-search-forward "\\(</?\\)\\([^<\n]*?\\)\\(>\\)" nil t)
          (c-put-font-lock-face (match-beginning 2) (match-end 2) markup-face))
        ;; Highlight member reference < [in,out]
        (goto-char p)
        (while (re-search-forward "^\\(<\\)\s*\\(?:\\[\\([^\n\\[]*\\)\\]\\)?" nil t)
          (c-put-font-lock-face (match-beginning 1) (match-end 1) markup-face)
          (when (match-beginning 2)
            (c-put-font-lock-face (match-beginning 2) (match-end 2) markup-face)))))))

(defun c++-doxygen-font-lock (limit)
  "Font lock matcher for Doxygen.
LIMIT is the extent of the match."
  (let* ((p (point))
         (s (parse-partial-sexp p limit nil nil (syntax-ppss p) t))
         (p2 (point)))
    (when (nth 4 s)                     ;Inside comment
      (if (nth 7 s)                     ;// style or /* */ style
          (when (looking-at "/\\|!")    ;Doxygen /// or //!
            (goto-char (- p2 2))
            (re-search-forward ".*$")
            (c++-doxygen-font-lock-match-data)
            (goto-char p2)
            t)
        (when (looking-at "\\*\\|!")    ;Doxygen /** or /*! style
          nil)))))

(defun c++-doxygen-comment-dwim (arg)
  "Call `comment-dwim' with a Doxygen comment.
Prefix ARG is forwarded to `comment-dwim'."
  (interactive "*P")
  (let* ((comment-end "")
         (line (buffer-substring (line-beginning-position) (line-end-position)))
         (comment-start (if (string-match ".*[,;]\s*$" line) "///< " "///")))
    (comment-dwim arg)))


(defun c++-doxygen-refresh-xwidgets ()
  "Refresh xwidget buffer."
  (let ((buf (find-if (lambda (buffer)
                        ;; TODO: This should only return a buffer known to be
                        ;; associated with a doxygen compile process
                        (with-current-buffer buffer
                          (equal #'xwidget-webkit-mode major-mode)))
                      (buffer-list)))
        (win-conf (current-window-configuration)))
    (when buf
      (save-current-buffer
        (with-current-buffer buf
          (xwidget-webkit-reload)))
      (set-window-configuration win-conf))))

(defadvice xwidget-webkit-callback (around c++-doxygen-save-windows activate)
  "Prevent xwidget callback from changing the window layout."
  (let ((win-conf (current-window-configuration)))
    ad-do-it
    (set-window-configuration win-conf)))

(defun c++-doxygen-refresh-xwidgets-on-process-completion (proc)
  "Refresh xwidget buffer when PROC finishes."
  (let ((old-sent (process-sentinel proc)))
    (set-process-sentinel proc (lambda (process event)
                                 (c++-doxygen-refresh-xwidgets)
                                 (when old-sent
                                   (funcall old-sent process event))))))

(defun c++-doxygen-compile-from-dominating-file ()
  "Attempt to call doxygen is a Doxygen configuration file is found in a dominating directory."
  (let ((doxy-dir (locate-dominating-file default-directory "Doxyfile")))
    (when doxy-dir
      (let ((compilation-buffer-name-function (lambda (mmode) (concat "*doxygen-" mmode "*")))
            (compile-command nil)
            (default-directory doxy-dir)
            (win-conf (current-window-configuration)))
        (compile "doxygen-quiet")
        (set-window-configuration win-conf)))))

(define-minor-mode c++-doxygen-mode
  "Toggle c++ doxygen mode."
  nil
  " C++Dox"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-;") #'c++-doxygen-comment-dwim)
    map)
  (let ((keyword-action (if c++-doxygen-mode
                            #'font-lock-add-keywords
                          #'font-lock-remove-keywords)))
    (funcall keyword-action nil '((c++-doxygen-font-lock . font-lock-warning-face)))
    (font-lock-flush))
  ;; (if c++-doxygen-mode
  ;;     (add-to-list 'after-save-hook
  ;;                  #'c++-doxygen-compile-from-dominating-file)
  ;;   (setq after-save-hook (delete 'after-save-hook
  ;;                                 #'c++-doxygen-compile-from-dominating-file)))
  (if c++-doxygen-mode
      (add-to-list 'compilation-start-hook #'c++-doxygen-refresh-xwidgets-on-process-completion)
    (setq compilation-start-hook (delete 'compilation-start-hook
                                         #'c++-doxygen-refresh-xwidgets-on-process-completion))))

(provide 'c++-doxygen-mode)
;;; c++-doxygen-mode.el ends here
