;;; gh-repos.el --- repos module for gh.el

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

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

;;;###autoload
(gh-defclass gh-repos-repo-stub (gh-object)
  ((name :initarg :name)
   (description :initarg :description)
   (homepage :initarg :homepage)
   (private :initarg :private))
  "Class for user-created repository objects")

;;;###autoload
(gh-defclass gh-repos-repo (gh-ref-object gh-repos-repo-stub)
  ((clone-url :initarg :clone-url)
   (git-url :initarg :git-url)
   (ssh-url :initarg :ssh-url)
   (svn-url :initarg :svn-url)
   (mirror-url :initarg :mirror-url)
   (owner :initarg :owner :initform nil :marshal-type gh-user)
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
   (organisation :initarg :organisation :initform nil :marshal-type gh-user)
   (parent :initarg :parent :marshal-type gh-repos-repo)
   (source :initarg :source :marshal-type gh-repos-repo)
   (has-issues :initarg :has-issues)
   (has-wiki :initarg :has-wiki)
   (has-downloads :initarg :has-downloads))
  "Class for GitHub repositories")

;;;###autoload
(gh-defclass gh-repos-ref (gh-object)
  ((label :initarg :label)
   (ref :initarg :ref :initform nil)
   (sha :initarg :sha :initform nil)
   (user :initarg :user :initform nil :marshal-type gh-user)
   (repo :initarg :repo :initform nil :marshal-type gh-repos-repo)))

(defun gh-repos-user-list (&optional username)
  (let* ((response (gh-api-authenticated-request
                    nil "GET" ;; (gh-object-list-reader gh-repos-repo)
                    (format "/users/%s/repos" (or username (gh-api-get-username api)))))
         (data (oref response :data)))
    data))

(defun gh-repos-org-list (org) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   (gh-object-list-reader gh-repos-repo) "GET"
   (format "/orgs/%s/repos" org)))

(defmethod gh-repos-repo-to-obj ((repo gh-repos-repo-stub)
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

(defun gh-repos-repo-new (repo-stub ;; (api gh-repos-api)
                              &optional org &rest caps)
  (gh-api-authenticated-request
   (gh-object-reader gh-repos-repo) "POST"
   (if org (format "/orgs/%s/repos" org)
     "/user/repos")
   (apply 'gh-repos-repo-to-obj repo-stub caps)))

(defun gh-repos-repo-get (repo-id &optional user) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   (gh-object-reader gh-repos-repo) "GET"
   (format "/repos/%s/%s"
           (or user (gh-api-get-username api))
           repo-id)))

(defun gh-repos-repo-update (repo-stub ;; (api gh-repos-api)
                                 &optional user &rest caps)
  (gh-api-authenticated-request
   (gh-object-reader gh-repos-repo) "PATCH"
   (format "/repos/%s/%s"
           (or user (gh-api-get-username api))
           (oref repo-stub :name))
   (apply 'gh-repos-repo-to-obj repo-stub caps)))

(defun gh-repos-repo-rename (repo-stub new-name ;; (api gh-repos-api)
                                 &optional user)
  (let ((new-stub (make-instance 'gh-repos-repo-stub :name new-name)))
    (gh-api-authenticated-request
     (gh-object-reader gh-repos-repo) "PATCH"
     (format "/repos/%s/%s"
             (or user (gh-api-get-username api))
             (oref repo-stub :name))
     (gh-repos-repo-to-obj new-stub))))

(defun gh-repos-repo-delete (repo-id ;; (api gh-repos-api)
                                 &optional user)
  (gh-api-authenticated-request
   (gh-object-reader gh-repos-repo) "DELETE"
   (format "/repos/%s/%s"
           (or user (gh-api-get-username api))
           repo-id)))

;; TODO gh-repos-repo-move

(defun gh-repos-repo-contributors (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   (gh-object-reader gh-repos-repo) "GET"
   (format "/repos/%s/%s/contributors"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

;;; TODO: generate some useful objects with the return values

(defun gh-repos-repo-languages (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   nil "GET" (format "/repos/%s/%s/languages"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defun gh-repos-repo-teams (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   nil "GET" (format "/repos/%s/%s/teams"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defun gh-repos-repo-tags (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   nil "GET" (format "/repos/%s/%s/tags"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defun gh-repos-repo-branches (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   nil "GET" (format "/repos/%s/%s/branches"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

;;; TODO gh-repos-repo-branch-commits

;;; Collaborators sub-API

(defun gh-repos-collaborators-list (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   (gh-object-list-reader gh-user) "GET" (format "/repos/%s/%s/collaborators"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defun gh-repos-collaborators-p (repo user) ;; (api gh-repos-api)
  (eq (oref (gh-api-authenticated-request
             nil "GET"
             (format "/repos/%s/%s/collaborators/%s"
                     (oref (oref repo :owner) :login)
                     (oref repo :name)
                     user))
            :http-status)
      204))

(defun gh-repos-collaborators-add (repo user) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   nil "PUT"
   (format "/repos/%s/%s/collaborators/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name)
           user)))

(defun gh-repos-collaborators-delete (repo user) ;; (api gh-repos-api)
  (gh-api-authenticated-request
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

(defun gh-repos-forks-list (repo &optional recursive) ;; (api gh-repos-api)
  (let ((resp (gh-api-authenticated-request
               (gh-object-list-reader gh-repos-repo) "GET"
               (format "/repos/%s/%s/forks"
                       (oref (oref repo :owner) :login)
                       (oref repo :name)))))
    (when recursive
      (let ((forks (oref resp :data)))
        (oset resp :data
              (apply 'nconc forks
                     (mapcar
                      (lambda (f)
                        (oref (gh-repos-forks-list f t) data))
                      forks)))))
    resp))

(defun gh-repos-fork (repo &optional org) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   (gh-object-reader gh-repos-repo) "POST"
   (format "/repos/%s/%s/forks"
           (oref (oref repo :owner) :login)
           (oref repo :name))
   nil (when org `(("org" . ,org)))))

;;; TODO Keys sub-API
;;; TODO Hooks sub-API
;;; TODO Merging sub-API

;;; Starring sub-API

(defun gh-repos-stargazers (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   (gh-object-list-reader gh-user) "GET"
   (format "/repos/%s/%s/stargazers"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(defun gh-repos-starred-list (&optional username) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   (gh-object-list-reader gh-repos-repo) "GET"
   (format "/users/%s/starred" (or username (gh-api-get-username api)))))

(defun gh-repos-starred-p (repo) ;; (api gh-repos-api)
  (eq (oref (gh-api-authenticated-request
             nil "GET"
             (format "/user/starred/%s/%s"
                     (oref (oref repo :owner) :login)
                     (oref repo :name)))
            :http-status)
      204))

(defun gh-repos-star (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   nil "PUT"
   (format "/user/starred/%s/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(defun gh-repos-unstar (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   nil "DELETE"
   (format "/user/starred/%s/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

;;; TODO Statuses sub-API

;;; Watching sub-API

(defun gh-repos-watchers (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   (gh-object-list-reader gh-user) "GET"
   (format "/repos/%s/%s/subscribers"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(defun gh-repos-watched-list (&optional username) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   (gh-object-list-reader gh-repos-repo) "GET"
   (format "/users/%s/subscriptions"
           (or username (gh-api-get-username api)))))

(defun gh-repos-watched-p (repo) ;; (api gh-repos-api)
  (eq (oref (gh-api-authenticated-request
             nil "GET"
             (format "/user/subscriptions/%s/%s"
                     (oref (oref repo :owner) :login)
                     (oref repo :name)))
            :http-status)
      204))

(defun gh-repos-watch (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   nil "PUT"
   (format "/user/subscriptions/%s/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(defun gh-repos-unwatch (repo) ;; (api gh-repos-api)
  (gh-api-authenticated-request
   nil "DELETE"
   (format "/user/subscriptions/%s/%s"
           (oref (oref repo :owner) :login)
           (oref repo :name))))

(provide 'gh-repos)
;;; gh-repos.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
