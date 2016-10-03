;;; gh-gist.el --- gist module for gh.el

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
(defclass gh-gist-api (gh-api-v3)
  ((gist-cls :allocation :class :initform gh-gist-gist))
  "Gist API")

;;;###autoload
(gh-defclass gh-gist-gist-stub (gh-object)
  ((files :initarg :files :type list :initform nil :marshal-type (list gh-gist-gist-file))
   (public :initarg :public :marshal-type bool)
   (description :initarg :description))
  "Class for user-created gist objects")

;;;###autoload
(gh-defclass gh-gist-history-change (gh-object)
  ((total :initarg :total)
   (additions :initarg :additions)
   (deletions :initarg :deletions)))

;;;###autoload
(gh-defclass gh-gist-history-entry (gh-object)
  ((user :initarg :user :initform nil :marshal-type gh-user)
   (version :initarg :version)
   (committed :initarg :committed :marshal ((alist . committed_at)))
   (change :initarg :change :marshal ((alist . change_status))
           :marshal-type gh-gist-history-change)
   (url :initarg :url)))

;;;###autoload
(gh-defclass gh-gist-fork-entry (gh-ref-object)
  ((user :initarg :user :initform nil :marshal-type gh-user)
   (created :initarg :created :marshal ((alist . created_at)))
   (updated :initarg :updated :marshal ((alist . updated_at)))))

;;;###autoload
(gh-defclass gh-gist-gist (gh-ref-object gh-gist-gist-stub)
  ((date :initarg :date :marshal ((alist . created_at)))
   (update :initarg :update :marshal ((alist . updated_at)))
   (push-url :initarg :push-url :marshal ((alist . git_push_url)))
   (pull-url :initarg :pull-url :marshal ((alist . git_pull_url)))
   (comments :initarg :comments)
   (user :initarg :user :initform nil :marshal-type gh-user :marshal ((alist . owner)))
   (history :initarg :history :initform nil :type list :marshal-type (list gh-gist-history-entry))
   (forks :initarg :forks :initform nil :type list :marshal-type (list gh-gist-fork-entry)))
  "Gist object")

;;;###autoload
(gh-defclass gh-gist-gist-file (gh-object)
  ((filename :initarg :filename)
   (size :initarg :size)
   (url :initarg :url :marshal ((alist . raw_url)))
   (content :initarg :content)))

(defmethod gh-gist-gist-to-obj ((gist gh-gist-gist-stub))
  (let ((files (mapcar #'gh-gist-gist-file-to-obj (oref gist :files))))
    `(("description" . ,(oref gist :description))
      ("public" . ,(oref gist :public))
      ,@(and files (list (cons "files"  files))))))

(defmethod gh-gist-gist-has-files ((gist gh-gist-gist-stub))
  (not (memq nil (mapcar (lambda (f)
                           (oref f :content)) (oref gist :files)))))

(defmethod gh-gist-gist-file-to-obj ((file gh-gist-gist-file))
  (let* ((filename (oref file :filename))
        (content (oref file :content))
        (file (if content
                  `(("filename" . ,filename)
                    ("content"  . ,content))
                nil)))
    (cons filename file)))

(defun gh-gist-list (&optional username) ;; (api gh-gist-api)
  (gh-api-authenticated-request
   gh-api-session (gh-object-list-reader (oref gh-api-session gist-cls)) "GET"
   (format "/users/%s/gists" (or username (gh-api-get-username api)))))

(defun gh-gist-list-public ( ) ;; (api gh-gist-api)
  (gh-api-authenticated-request
   gh-api-session (gh-object-list-reader (oref gh-api-session gist-cls)) "GET" "/gists/public"))

(defun gh-gist-list-starred ( ) ;; (api gh-gist-api)
  (gh-api-authenticated-request
   gh-api-session (gh-object-list-reader (oref gh-api-session gist-cls)) "GET" "/gists/starred"))

(defun gh-gist-get (gist-or-id) ;; (api gh-gist-api)
  (let (id transformer)
    (if (stringp gist-or-id)
        (setq id gist-or-id
              transformer (gh-object-reader (oref gh-api-session gist-cls)))
      (setq id (oref gist-or-id :id)
            transformer (gh-object-reader gist-or-id)))
    (gh-api-authenticated-request
     gh-api-session transformer "GET" (format "/gists/%s" id))))

(defun gh-gist-new (gist-stub) ;; (api gh-gist-api)
  (gh-api-authenticated-request
   gh-api-session (gh-object-reader (oref gh-api-session gist-cls)) "POST" "/gists"
   (gh-gist-gist-to-obj gist-stub)))

(defun gh-gist-edit (gist) ;; (api gh-gist-api)
  (gh-api-authenticated-request
   gh-api-session (gh-object-reader (oref gh-api-session gist-cls)) "PATCH"
   (format "/gists/%s"
           (oref gist :id))
   (gh-gist-gist-to-obj gist)))

(defun gh-gist-set-star (gist-or-id star) ;; (api gh-gist-api)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     gh-api-session 'ignore (if star "PUT" "DELETE")
     (format "/gists/%s/star" id))))

(defun gh-gist-get-star (gist-or-id) ;; (api gh-gist-api)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     gh-api-session 'ignore "GET" (format "/gists/%s/star" id))))

(defun gh-gist-fork (gist-or-id) ;; (api gh-gist-api)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     gh-api-session (gh-object-reader (oref gh-api-session gist-cls)) "POST"
     (format "/gists/%s/forks" id))))

(defun gh-gist-delete (gist-or-id) ;; (api gh-gist-api)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     gh-api-session 'ignore "DELETE" (format "/gists/%s" id))))

(provide 'gh-gist)
;;; gh-gist.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
