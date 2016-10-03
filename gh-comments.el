;;; gh-comments.el --- support for comment-enabled APIs

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

(require 'gh-common)
(require 'gh-api)

;;;###autoload
(defclass gh-comments-api-mixin ()
  ((comment-cls :allocation :class :initform gh-comment))
  :abstract t)

(defun gh-comments-list (base) ;; (api gh-comments-api-mixin)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api comment-cls)) "GET"
   (format "%s/comments" (gh-ref-object-base base))))

(defun gh-comments-get (base comment-id) ;; (api gh-comments-api-mixin)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api comment-cls)) "GET"
   (format "%s/comments/%s" (gh-ref-object-base base) comment-id)))

(defun gh-comments-update (base comment-id comment) ;; (api gh-comments-api-mixin)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api comment-cls)) "PATCH"
   (format "%s/comments/%s" (gh-ref-object-base base) comment-id)
   (gh-comment-req-to-update comment)))

(defun gh-comments-new (base comment) ;; (api gh-comments-api-mixin)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api comment-cls)) "POST"
   (format "%s/comments" (gh-ref-object-base base))
   (gh-comment-req-to-update comment)))

(defun gh-comments-delete (base comment-id) ;; (api gh-comments-api-mixin)
  (gh-api-authenticated-request
   api nil "DELETE"
   (format "%s/comments/%s" (gh-ref-object-base base) comment-id)))

(provide 'gh-comments)
;;; gh-comments.el ends here
