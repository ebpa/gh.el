;;; github-comments.el --- support for comment-enabled APIs

;; Copyright (C) 2014-2015  Yann Hodique

;; Author: Yann Hodique <hodiquey@vmware.com>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'github-common)
(require 'github-api)

(defun github-comments-list (base) ;; (api github-comments-api-mixin)
  (github-api-authenticated-request
   (github-object-list-reader github-comment) "GET"
   (format "%s/comments" (github-ref-object-base base))))

(defun github-comments-get (base comment-id) ;; (api github-comments-api-mixin)
  (github-api-authenticated-request
   (github-object-reader github-comment) "GET"
   (format "%s/comments/%s" (github-ref-object-base base) comment-id)))

(defun github-comments-update (base comment-id comment) ;; (api github-comments-api-mixin)
  (github-api-authenticated-request
   (github-object-reader github-comment) "PATCH"
   (format "%s/comments/%s" (github-ref-object-base base) comment-id)
   (github-comment-req-to-update comment)))

(defun github-comments-new (base comment) ;; (api github-comments-api-mixin)
  (github-api-authenticated-request
   (github-object-reader github-comment) "POST"
   (format "%s/comments" (github-ref-object-base base))
   (github-comment-req-to-update comment)))

(defun github-comments-delete (base comment-id) ;; (api github-comments-api-mixin)
  (github-api-authenticated-request
   nil "DELETE"
   (format "%s/comments/%s" (github-ref-object-base base) comment-id)))

(provide 'github-comments)
;;; github-comments.el ends here
