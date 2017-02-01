;;; github-issues.el --- issues api for github

;; Copyright (C) 2014-2015  Yann Hodique
;; Copyright (C) 2014 Travis Thieman
;; Copyright (C) 2012  Raimon Grau

;; Author: Raimon Grau <raimonster@gmail.com>
;; Keywords:

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

;; Basic usage:

;; (setf github-api-session (github-issues-api "api" :sync nil :cache nil :num-retries 1))
;; (setf issues (github-issues-list "user" "repo"))
;; (last (oref issues data)) ; get one issue
;; (setq mi (make-instance 'github-issues-issue :body "issue body" :title "issue title"))
;; (github-issues-issue-new "user" "repo" mi)
;; (setf comments (github-issues-comments-list "user" "repo" "issue id"))
;; (setq my-comment (make-instance 'github-issues-comment :body "This is great!"))
;; (github-issues-comments-new "user" "repo" "issue id" my-comment)

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

;; TODO should be named "...request"?
;;;###autoload
(github-defclass github-issues-issue (github-ref-object)
  ((number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (body :initarg :body)
   (user :initarg :user :initform nil :marshal-type github-user)
   (labels :initarg :labels :initform nil :marshal-type (list github-issues-label))
   (assignees :initarg :assignees :initform nil :marshal-type (list github-user))
   (assignee :initarg :assignee :initform nil :marshal-type github-user)
   (milestone :initarg :milestone :initform nil :marshal-type github-issues-milestone)
   (comments :initarg :comments :initform 0)
   (pull-request :initarg :pull-request :marshal-type github-issues-pull-request)
   (closed-at :initarg :closed-at)
   (created-at :initarg :created-at)
   (updated-at :initarg :updated-at))
  "issues request")

;;;###autoload
(github-defclass github-issues-pull-request (github-object)
  ((html-url :initarg :html-url)
   (diff-url :initarg :diff-url)
   (patch-url :initarg :patch-url)))

;;;###autoload
(github-defclass github-issues-label (github-ref-object)
  ((name :initarg :name)
   (color :initarg :color)))

(defmethod github-issues-label-req-to-update ((label github-issues-label))
  `(("name" . ,(oref label :name))
    ("color" . ,(oref label :color))))

;;;###autoload
(github-defclass github-issues-milestone (github-ref-object)
  ((number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (description :initarg :description)
   (creator :initarg :creator :initform nil :marshal-type github-user)
   (open-issues :initarg :open-issues )
   (closed-issues :initarg :closed-issues)
   (created-at :initarg :created-at)
   (due-on :initarg :due-on))
  "github milestone")

;;;###autoload
(github-defclass github-issues-comment (github-comment)
  ())

(defun github-issues-issue-list (user repo)
  (let* ((response (github-api-authenticated-request
                   ;;(github-object-list-reader github-issues-issue) "GET"
                   nil "GET"
                   (format "/repos/%s/%s/issues" user repo)))
         (data (oref response :data)))
    data))

(defun github-issues-milestone-list (user repo)
  (github-api-authenticated-request
   (github-object-list-reader github-issues-milestone) "GET"
   (format "/repos/%s/%s/milestones" user repo)))

(defun github-issues-milestone-get (user repo id)
  (github-api-authenticated-request
   (github-object-reader github-issues-milestone) "GET"
   (format "/repos/%s/%s/milestones/%s" user repo id)))

(defun github-issues-milestone-new (user repo milestone)
  (github-api-authenticated-request
   (github-object-reader github-issues-milestone) "POST"
   (format "/repos/%s/%s/milestones" user repo)
   (github-issues-milestone-req-to-update milestone)))

(defun github-issues-milestone-update (user repo
                                       id milestone)
  (github-api-authenticated-request
   (github-object-reader github-issues-milestone) "PATCH"
   (format "/repos/%s/%s/milestones/%s" user repo id)
   (github-issues-milestone-req-to-update milestone)))

(defmethod github-issues-milestone-req-to-update ((milestone github-issues-milestone))
  (let ((state (oref milestone :state))
        (description (oref milestone :description))
        (due-on (oref milestone :due-on))
        (to-update `(("title" . ,(oref milestone :title)))))
    (when state (nconc to-update `(("state" . ,state))))
    (when description (nconc to-update `(("description" . ,description))))
    (when due-on (nconc to-update `(("due_on" . ,due-on))))
    to-update))

(defun github-issues-issue-get (user repo id)
  (github-api-authenticated-request
   (github-object-reader github-issues-issue) "GET"
   (format "/repos/%s/%s/issues/%s" user repo id)))

(defmethod github-issues-issue-req-to-update ((req github-issues-issue))
  (let ((assignee (oref req :assignee))
        ;; (labels (oref req labels))
        (milestone (oref req :milestone))
        (to-update `(("title" . ,(oref req :title))
                     ("state" . ,(oref req :state))
                     ("body" . ,(oref req :body)))))

    ;; (when labels (nconc to-update `(("labels" . ,(oref req labels) ))))
    (when milestone
      (nconc to-update `(("milestone" . ,(oref milestone :number)))))
    (when assignee
      (nconc to-update `(("assignee" . ,(oref assignee :login)))))
    to-update))

(defun github-issues-issue-update (user repo id req)
  (github-api-authenticated-request
   (github-object-reader github-issues-issue) "PATCH"
   (format "/repos/%s/%s/issues/%s" user repo id)
   (github-issues-issue-req-to-update req)))

(defun github-issues-issue-new (user repo issue)
  (github-api-authenticated-request
   (github-object-reader github-issues-issue) "POST"
   (format "/repos/%s/%s/issues" user repo)
   (github-issues-issue-req-to-update issue)))

;;; Labels

(defun github-issues-label-get (user repo name)
  (github-api-authenticated-request
   (github-object-reader github-issues-label) "GET"
   (format "/repos/%s/%s/labels/%s" user repo name)))

(defun github-issues-label-list (user repo)
  (github-api-authenticated-request
   (github-object-list-reader github-issues-label) "GET"
   (format "/repos/%s/%s/labels" user repo )))

(defun github-issues-label-new (user repo req)
  (github-api-authenticated-request
   (github-object-reader github-issues-label) "POST"
   (format "/repos/%s/%s/labels" user repo)
   (github-issues-label-req-to-update req)))

(defun github-issues-label-update (user repo req)
  (github-api-authenticated-request
   (github-object-reader github-issues-label) "POST"
   (format "/repos/%s/%s/labels/%s" user repo (oref req :name))
   (github-issues-label-req-to-update req)))

(defun github-issues-label-delete (user repo name)
  (github-api-authenticated-request
   (github-object-reader github-issues-label) "DELETE"
   (format "/repos/%s/%s/labels/%s" user repo name)))


(defun github-issues-labels-in-issue (user repo
                                      issue-or-issue-id)
  (let ((issue-id (github-issues--issue-id issue-or-issue-id)))
   (github-api-authenticated-request
    (github-object-list-reader github-issues-label) "GET"
    (format "/repos/%s/%s/issues/%s/labels" user repo issue-id))))

(defun github-issues-labels-add-to-issue (user repo
                                          issue-or-issue-id labels)
  (let ((issue-id (github-issues--issue-id issue-or-issue-id)))
    (github-api-authenticated-request
     (github-object-list-reader github-issues-label) "PUT"
     (format "/repos/%s/%s/issues/%s/labels" user repo issue-id)
     (mapcar #'github-issues--label-name labels))))

(defun github-issues-labels-remove-all-from-issue (user repo
                                                   issue-or-issue-id )
  (let ((issue-id (github-issues--issue-id issue-or-issue-id)))
    (github-api-authenticated-request
     (lambda (x) x) "DELETE"
     (format "/repos/%s/%s/issues/%s/labels" user repo issue-id))))

(defun github-issues-labels-in-milestone (user repo
                                          milestone-or-milestone-id)
  (let ((milestone-id (github-issues--milestone-id milestone-or-milestone-id)))
   (github-api-authenticated-request
    (github-object-list-reader github-issues-label) "GET"
    (format "/repos/%s/%s/milestones/%s/labels" user repo milestone-id))))

;;; Comments

(defun github-issues-comments-list (user repo issue-id)
  (github-comments-list (format "/repos/%s/%s/issues/%s" user repo issue-id)))

(defun github-issues-comments-get (user repo comment-id)
  (github-comments-get (format "/repos/%s/%s/issues" user repo) comment-id))

(defun github-issues-comments-update (user repo comment-id comment)
  (github-comments-update (format "/repos/%s/%s/issues" user repo)
                      comment-id (github-comment-req-to-update comment)))

(defun github-issues-comments-new (user repo issue-id comment)
  (github-comments-new (format "/repos/%s/%s/issues/%s" user repo issue-id)
                   (github-comment-req-to-update comment)))

(defun github-issues-comments-delete (user repo comment-id)
  (github-comments-delete (format "/repos/%s/%s/issues" user repo) comment-id))

;;; helpers

(defun github-issues--issue-id (issue-or-issue-id)
  (if (eieio-object-p issue-or-issue-id)
      (oref issue-or-issue-id :id)
    issue-or-issue-id))

(defun github-issues--milestone-id (milestone-or-milestone-id)
  (if (eieio-object-p milestone-or-milestone-id)
      (oref milestone-or-milestone-id :id)
    milestone-or-milestone-id))

(defun github-issues--label-name (label-or-label-name)
  (if (eieio-object-p label-or-label-name)
      (oref label-or-label-name :name)
    label-or-label-name))


(provide 'github-issues)
;;; github-issues.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
