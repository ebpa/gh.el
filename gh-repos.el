;;; github-repos.el --- repos module for github.el

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
(require 'github-common)

;;;###autoload
(github-defclass github-repos-repo-stub (github-object)
  ((name :initarg :name)
   (description :initarg :description)
   (homepage :initarg :homepage)
   (private :initarg :private))
  "Class for user-created repository objects")

;;;###autoload
(github-defclass github-repos-repo (github-ref-object github-repos-repo-stub)
  ((clone-url :initarg :clone-url)
   (git-url :initarg :git-url)
   (ssh-url :initarg :ssh-url)
   (svn-url :initarg :svn-url)
   (mirror-url :initarg :mirror-url)
   (owner :initarg :owner :initform nil :marshal-type github-user)
   (full-name :initarg :full-name)
   (language :initarg :language)
   (fork :initarg :fork)
   (forks :initarg :forks)
   (forks-count :initarg :forks-count)
   (watchers :initarg :watchers)
   (watchers-count :initarg :watchers-count)
   (stargazers-count :initarg :stargazers-count)
   (size :initarg :size)
   (master-branch :initarg :master-branch)
   (open-issues :initarg :open-issues)
   (pushed-at :initarg :pushed-at)
   (created-at :initarg :created-at)
   (updated-at :initarg :updated-at)
   (organisation :initarg :organisation :initform nil :marshal-type github-user)
   (parent :initarg :parent :marshal-type github-repos-repo)
   (source :initarg :source :marshal-type github-repos-repo)
   (has-issues :initarg :has-issues)
   (has-wiki :initarg :has-wiki)
   (has-downloads :initarg :has-downloads))
  "Class for GitHub repositories")

;;;###autoload
(github-defclass github-repos-ref (github-object)
  ((label :initarg :label)
   (ref :initarg :ref :initform nil)
   (sha :initarg :sha :initform nil)
   (user :initarg :user :initform nil :marshal-type github-user)
   (repo :initarg :repo :initform nil :marshal-type github-repos-repo)))

(defun github-repos-user-list (&optional username)
  (let* ((response (github-api-authenticated-request
                    nil "GET" ;; (github-object-list-reader github-repos-repo)
                    (format "/users/%s/repos" (or username (github-api-get-username api)))))
         (data (oref response :data)))
    data))

(defun github-repos-org-list (org) ;; (api github-repos-api)
  (github-api-authenticated-request
   (github-object-list-reader github-repos-repo) "GET"
   (format "/orgs/%s/repos" org)))

(defmethod github-repos-repo-to-obj ((repo github-repos-repo-stub)
                                 &rest caps)
  (let ((has_issues (plist-member caps :issues))
        (has_wiki (plist-member caps :wiki))
        (has_downloads (plist-member caps :downloads)))
    `(("name" . ,(oref repo :name))
      ,@(when (slot-boundp repo :homepage)
          (list (cons "homepage" (oref repo :homepage))))
      ,@(when (slot-boundp repo :description)
          (list (cons "description" (oref repo :description))))
      ,@(when (slot-boundp repo :private)
          (list (cons "public" (not (oref repo :private)))))
      ,@(when has_issues
          (list (cons "has_issues" (plist-get caps :issues))))
      ,@(when has_wiki
          (list (cons "has_wiki" (plist-get caps :wiki))))
      ,@(when has_downloads
          (list (cons "has_downloads" (plist-get caps :downloads)))))))

(defun github-repos-repo-new (repo-stub ;; (api github-repos-api)
                              &optional org &rest caps)
  (github-api-authenticated-request
   (github-object-reader github-repos-repo) "POST"
   (if org (format "/orgs/%s/repos" org)
     "/user/repos")
   (apply 'github-repos-repo-to-obj repo-stub caps)))

(defun github-repos-repo-get (repo-id &optional user) ;; (api github-repos-api)
  (github-api-authenticated-request
   (github-object-reader github-repos-repo) "GET"
   (format "/repos/%s/%s"
           (or user (github-api-get-username api))
           repo-id)))

(defun github-repos-repo-update (repo-stub ;; (api github-repos-api)
                                 &optional user &rest caps)
  (github-api-authenticated-request
   (github-object-reader github-repos-repo) "PATCH"
   (format "/repos/%s/%s"
           (or user (github-api-get-username api))
           (oref repo-stub :name))
   (apply 'github-repos-repo-to-obj repo-stub caps)))

(defun github-repos-repo-rename (repo-stub new-name ;; (api github-repos-api)
                                 &optional user)
  (let ((new-stub (make-instance 'github-repos-repo-stub :name new-name)))
    (github-api-authenticated-request
     (github-object-reader github-repos-repo) "PATCH"
     (format "/repos/%s/%s"
             (or user (github-api-get-username api))
             (oref repo-stub :name))
     (github-repos-repo-to-obj new-stub))))

(defun github-repos-repo-delete (repo-id ;; (api github-repos-api)
                                 &optional user)
  (github-api-authenticated-request
   (github-object-reader github-repos-repo) "DELETE"
   (format "/repos/%s/%s"
           (or user (github-api-get-username api))
           repo-id)))

;; TODO github-repos-repo-move

(defun github-repos-repo-contributors (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   (github-object-reader github-repos-repo) "GET"
   (format "/repos/%s/%s/contributors"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

;;; TODO: generate some useful objects with the return values

(defun github-repos-repo-languages (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "GET" (format "/repos/%s/%s/languages"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defun github-repos-repo-teams (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "GET" (format "/repos/%s/%s/teams"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defun github-repos-repo-tags (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "GET" (format "/repos/%s/%s/tags"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defun github-repos-repo-branches (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "GET" (format "/repos/%s/%s/branches"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

;;; TODO github-repos-repo-branch-commits

;;; Collaborators sub-API

(defun github-repos-collaborators-list (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   (github-object-list-reader github-user) "GET" (format "/repos/%s/%s/collaborators"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defun github-repos-collaborators-p (repo user) ;; (api github-repos-api)
  (eq (oref (github-api-authenticated-request
             nil "GET"
             (format "/repos/%s/%s/collaborators/%s"
                     (oref (oref repo :owner) :login)
                     (oref repo :name)
                     user))
            :http-status)
      204))

(defun github-repos-collaborators-add (repo user) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "PUT"
   (format "/repos/%s/%s/collaborators/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name)
           user)))

(defun github-repos-collaborators-delete (repo user) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "DELETE"
   (format "/repos/%s/%s/collaborators/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name)
           user)))

;;; TODO Comments sub-API
;;; TODO Commits sub-API
;;; TODO Contents sub-API
;;; TODO Downloads sub-API

;;; Forks sub-API

(defun github-repos-forks-list (repo &optional recursive) ;; (api github-repos-api)
  (let ((resp (github-api-authenticated-request
               (github-object-list-reader github-repos-repo) "GET"
               (format "/repos/%s/%s/forks"
                       (oref (oref repo :owner) :login)
                       (oref repo :name)))))
    (when recursive
      (let ((forks (oref resp :data)))
        (oset resp :data
              (apply 'nconc forks
                     (mapcar
                      (lambda (f)
                        (oref (github-repos-forks-list f t) data))
                      forks)))))
    resp))

(defun github-repos-fork (repo &optional org) ;; (api github-repos-api)
  (github-api-authenticated-request
   (github-object-reader github-repos-repo) "POST"
   (format "/repos/%s/%s/forks"
           (oref (oref repo :owner) :login)
           (oref repo :name))
   nil (when org `(("org" . ,org)))))

;;; TODO Keys sub-API
;;; TODO Hooks sub-API
;;; TODO Merging sub-API

;;; Starring sub-API

(defun github-repos-stargazers (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   (github-object-list-reader github-user) "GET"
   (format "/repos/%s/%s/stargazers"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(defun github-repos-starred-list (&optional username) ;; (api github-repos-api)
  (github-api-authenticated-request
   (github-object-list-reader github-repos-repo) "GET"
   (format "/users/%s/starred" (or username (github-api-get-username api)))))

(defun github-repos-starred-p (repo) ;; (api github-repos-api)
  (eq (oref (github-api-authenticated-request
             nil "GET"
             (format "/user/starred/%s/%s"
                     (oref (oref repo :owner) :login)
                     (oref repo :name)))
            :http-status)
      204))

(defun github-repos-star (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "PUT"
   (format "/user/starred/%s/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(defun github-repos-unstar (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "DELETE"
   (format "/user/starred/%s/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

;;; TODO Statuses sub-API

;;; Watching sub-API

(defun github-repos-watchers (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   (github-object-list-reader github-user) "GET"
   (format "/repos/%s/%s/subscribers"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(defun github-repos-watched-list (&optional username) ;; (api github-repos-api)
  (github-api-authenticated-request
   (github-object-list-reader github-repos-repo) "GET"
   (format "/users/%s/subscriptions"
           (or username (github-api-get-username api)))))

(defun github-repos-watched-p (repo) ;; (api github-repos-api)
  (eq (oref (github-api-authenticated-request
             nil "GET"
             (format "/user/subscriptions/%s/%s"
                     (oref (oref repo :owner) :login)
                     (oref repo :name)))
            :http-status)
      204))

(defun github-repos-watch (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "PUT"
   (format "/user/subscriptions/%s/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(defun github-repos-unwatch (repo) ;; (api github-repos-api)
  (github-api-authenticated-request
   nil "DELETE"
   (format "/user/subscriptions/%s/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(provide 'github-repos)
;;; github-repos.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
