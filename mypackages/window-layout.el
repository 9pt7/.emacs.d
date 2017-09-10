;;; window-layout.el --- Window Layout Management    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Peter Thompson

;; Author: Peter Thompson <peter.thompson92@gmail.com>
;; Keywords: convenience, frames

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
(require 'eieio)

(defclass window-layout ()
  ((configuration :type window-configuration :initform (current-window-configuration))
   (window-marker :type marker :initform (set-marker (make-marker) (window-point) nil))))

(defvar current-window-layout (make-instance window-layout))
(defvar window-layout-vector (vector current-window-layout))
(defvar window-idx 0)
(defvar max-window-layouts 8)

(defun window-layout-set (new-layout)
  "Set the current layout to NEW-LAYOUT."
  (let ((current current-window-layout))
    (unless (equal current new-layout)
        (with-slots ((cur-config configuration)
                     (cur-marker window-marker))
            current
          (setf cur-config (current-window-configuration))
          (setf cur-marker (set-marker (make-marker) (window-point) nil)))))
  (with-slots ((new-config configuration)
               (new-marker window-marker))
      new-layout
    (set-window-configuration new-config)
    (set-window-point nil (marker-position new-marker)))
  (setf current-window-layout new-layout))

(defun window-layout-set-at-head (layout)
  "Move LAYOUT to head of queue."
  (setf window-layout-vector (vconcat (vector layout) (cl-remove layout
                                                                 window-layout-vector
                                                                 :test #'eq)))
  (setf window-idx 0))

(defun window-layout-push ()
  "Create a new layout and push in onto the queue."
  (interactive)
  (window-layout-set-at-head current-window-layout)
  (let ((layout (make-instance window-layout)))
    (window-layout-set-at-head layout)
    (window-layout-set layout)))

(defun window-layout-pop (inc continue)
  "Pop a layout off of the queue.
INC specifies the direction from the current queue pointer.
CONTINUE specifies if the queue pointer should be reset."
  (unless continue
    (window-layout-set-at-head current-window-layout))

  (setf window-idx (mod (+ window-idx inc)
                            (length window-layout-vector)))
  (window-layout-set (aref window-layout-vector window-idx))
  (message "%s" window-idx))

(defun window-layout-pop-should-continue ()
  "Check if the queue pointer should be reset."
  (or (eq last-command #'window-layout-pop-forward)
      (eq last-command #'window-layout-pop-backward)))

(defun window-layout-pop-forward ()
  "Pop a layout in the forward direction."
  (interactive)
  (window-layout-pop 1 (window-layout-pop-should-continue)))

(defun window-layout-pop-backward ()
  "Pop a layout in the backward direction."
  (interactive)
  (window-layout-pop -1 (window-layout-pop-should-continue)))


(provide 'window-layout)
;;; window-layout.el ends here
