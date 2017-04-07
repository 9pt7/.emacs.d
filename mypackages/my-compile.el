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

(require 'cl)
(require 'compile)

(defvar my-compile-makefile-regex "^[mM]akefile$")
(defvar my-compile-build-dir-rel '("./" "build/" "build/debug"))

(defun my-compile-find-build-dir ()
  "Guess the build directory.
The parents of `default-directory' are visited recursively, while searching
for a make-file matching `my-compile-makefile-regex' from the
relative paths in `my-compile-build-dir-rel'."
  (cl-loop
   for par = default-directory then (file-name-directory (directory-file-name par))
   for build-dir = (dolist (rel my-compile-build-dir-rel)
                     (let* ((build-dir (concat par rel))
                            (file (and (file-exists-p build-dir)
                                       (find-if (apply-partially #'string-match
                                                                 my-compile-makefile-regex)
                                                (directory-files build-dir)))))
                       (when file (return build-dir))))
   until (or build-dir (equal par "/"))
   finally return build-dir))

(defvar-local my-compile-dir nil)

(defun my-compile ()
  "Call `compile' interactively from a chosen directory."
  (interactive)
  (let* ((dir (read-directory-name
               "Compilation directory: "
               (or (my-compile-find-build-dir) default-directory)))
         (compilation-directory dir))
    (setq-local my-compile-dir dir)
    (let ((default-directory dir))
         (call-interactively #'compile))))

(defun my-compile-recompile ()
  "Call `recompile' from a chosen directory."
  (interactive)
  (let* ((dir (or my-compile-dir
                  (read-directory-name
                   "Compilation directory: "
                   (or (my-compile-find-build-dir) default-directory))))
         (compilation-directory dir))
    (setq-local my-compile-dir dir)
    (call-interactively #'recompile)))

(provide 'my-compile)
;;; my-compile.el ends here
