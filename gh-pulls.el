;;; github-pulls.el --- pull requests module for github.el

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
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

(require 'github-api)
(require 'github-auth)
(require 'github-comments)
(require 'github-common)

(require 'github-repos)

;;;###autoload
(defclass github-pulls-cache (github-cache)
  ((invalidation-chain :allocation :class
                       :initform '(("^/repos/.*/.*/pulls$" . "\0")
                                   ("^/repos/.*/.*/pulls/.*$" . "\0")))))

;;;###autoload
(github-defclass github-pulls-comment (github-comment)
  ((path :initarg :path)
   (diff-hunk :initarg :diff-hunk)
   (position :initarg :position)
   (original-position :initarg :original-position)
   (commit-id :initarg :commit-id)
   (original-commit-id :initarg :original-commit-id)
   (in-reply-to :initarg :in-reply-to :initform nil)))

(defmethod github-pulls-comment-req-to-create ((req github-pulls-comment))
  (let ((in-reply-to (oref req in-reply-to))
	(to-update `(("body" . ,(oref req body)))))
    (if in-reply-to
	(nconc to-update `(("in_reply_to" . ,in-reply-to)))
      (nconc to-update `(("commit_id" . ,(oref req commit-id))
			 ("path" . ,(oref req path))
			 ("position" . ,(oref req position)))))
    to-update))

;;;###autoload
(github-defclass github-pulls-request-stub (github-ref-object)
  ((diff-url :initarg :diff-url)
   (patch-url :initarg :patch-url)
   (issue-url :initarg :issue-url)
   (number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (body :initarg :body)
   (created-at :initarg :created-at)
   (updated-at :initarg :updated-at)
   (closed-at :initarg :closed-at)
   (merged-at :initarg :merged-at)
   (head :initarg :head :initform nil :marshal-type github-repos-ref)
   (base :initarg :base :initform nil :marshal-type github-repos-ref)))

;;;###autoload
(github-defclass github-pulls-request (github-pulls-request-stub)
  ((merged :initarg :merged)
   (mergeable :initarg :mergeable)
   (merged-by :initarg :merged-by)
   (comments :initarg :comments)
   (user :initarg :user :initform nil :marshal-type github-user)
   (commits :initarg :commits)
   (additions :initarg :additions)
   (deletions :initarg :deletions)
   (changed-files :initarg :changed-files))
  "Git pull requests API")

(defmethod github-pulls-req-to-new ((req github-pulls-request))
  (let ((head (oref req :head))
        (base (oref req :base)))
    `(("title" . ,(oref req :title))
      ("body" . ,(oref req :body))
      ("head" . ,(or (oref head :ref) (oref head :sha)))
      ("base" . ,(or (oref base :ref) (oref base :sha))))))

(defmethod github-pulls-req-to-update ((req github-pulls-request-stub))
  `(("title" . ,(oref req :title))
    ("body" . ,(oref req :body))
    ("state" . ,(oref req :state))))

(defun github-pulls-list (user repo) ;; (api github-pulls-api)
  (github-api-authenticated-request
   (github-object-list-reader github-pulls-request) "GET"
   (format "/repos/%s/%s/pulls" user repo)))

(defun github-pulls-get (user repo id) ;; (api github-pulls-api)
  (github-api-authenticated-request
   (github-object-reader github-pulls-request) "GET"
   (format "/repos/%s/%s/pulls/%s" user repo id)))

(defun github-pulls-new (user repo req) ;; (api github-pulls-api)
  (github-api-authenticated-request
   (github-object-reader github-pulls-request) "POST"
   (format "/repos/%s/%s/pulls" user repo)
   (github-pulls-req-to-new req)))

(defun github-pulls-update (user repo id req) ;; (api github-pulls-api)
  (github-api-authenticated-request
   (github-object-reader github-pulls-request) "PATCH"
   (format "/repos/%s/%s/pulls/%s" user repo id)
   (github-pulls-req-to-update req)))

;;; Comments

(defun github-pulls-comments-list (user repo pull-id) ;; (api github-pulls-api)
  (github-comments-list (format "/repos/%s/%s/pulls/%s" user repo pull-id)))

(defun github-pulls-comments-get (user repo comment-id) ;; (api github-pulls-api)
  (github-comments-get (format "/repos/%s/%s/pulls" user repo) comment-id))

(defun github-pulls-comments-update (
                                      user repo comment-id comment)
  (github-comments-update (format "/repos/%s/%s/pulls" user repo)
                      comment-id (github-comment-req-to-update comment)))

(defun github-pulls-comments-new (
                                   user repo pull-id comment)
  (github-comments-new (format "/repos/%s/%s/pulls/%s" user repo pull-id)
                   (github-pulls-comment-req-to-create comment)))

(defun github-pulls-comments-delete (user repo comment-id) ;; (api github-pulls-api)
  (github-comments-delete (format "/repos/%s/%s/pulls" user repo) comment-id))

(provide 'github-pulls)
;;; github-pulls.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
