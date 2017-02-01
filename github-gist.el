;;; github-gist.el --- gist module for github.el

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
(github-defclass github-gist-gist-stub (github-object)
  ((files :initarg :files :type list :initform nil :marshal-type (list github-gist-gist-file))
   (public :initarg :public :marshal-type bool)
   (description :initarg :description))
  "Class for user-created gist objects")

;;;###autoload
(github-defclass github-gist-history-change (github-object)
  ((total :initarg :total)
   (additions :initarg :additions)
   (deletions :initarg :deletions)))

;;;###autoload
(github-defclass github-gist-history-entry (github-object)
  ((user :initarg :user :initform nil :marshal-type github-user)
   (version :initarg :version)
   (committed :initarg :committed :marshal ((alist . committed_at)))
   (change :initarg :change :marshal ((alist . change_status))
           :marshal-type github-gist-history-change)
   (url :initarg :url)))

;;;###autoload
(github-defclass github-gist-fork-entry (github-ref-object)
  ((user :initarg :user :initform nil :marshal-type github-user)
   (created :initarg :created :marshal ((alist . created_at)))
   (updated :initarg :updated :marshal ((alist . updated_at)))))

;;;###autoload
(github-defclass github-gist-gist (github-ref-object github-gist-gist-stub)
  ((date :initarg :date :marshal ((alist . created_at)))
   (update :initarg :update :marshal ((alist . updated_at)))
   (push-url :initarg :push-url :marshal ((alist . git_push_url)))
   (pull-url :initarg :pull-url :marshal ((alist . git_pull_url)))
   (comments :initarg :comments)
   (user :initarg :user :initform nil :marshal-type github-user :marshal ((alist . owner)))
   (history :initarg :history :initform nil :type list :marshal-type (list github-gist-history-entry))
   (forks :initarg :forks :initform nil :type list :marshal-type (list github-gist-fork-entry)))
  "Gist object")

;;;###autoload
(github-defclass github-gist-gist-file (github-object)
  ((filename :initarg :filename)
   (size :initarg :size)
   (url :initarg :url :marshal ((alist . raw_url)))
   (content :initarg :content)))

(defmethod github-gist-gist-to-obj ((gist github-gist-gist-stub))
  (let ((files (mapcar #'github-gist-gist-file-to-obj (oref gist :files))))
    `(("description" . ,(oref gist :description))
      ("public" . ,(oref gist :public))
      ,@(and files (list (cons "files"  files))))))

(defmethod github-gist-gist-has-files ((gist github-gist-gist-stub))
  (not (memq nil (mapcar (lambda (f)
                           (oref f :content)) (oref gist :files)))))

(defmethod github-gist-gist-file-to-obj ((file github-gist-gist-file))
  (let* ((filename (oref file :filename))
        (content (oref file :content))
        (file (if content
                  `(("filename" . ,filename)
                    ("content"  . ,content))
                nil)))
    (cons filename file)))

(defun github-gist-list (&optional username)
  (github-api-authenticated-request
   (github-object-list-reader github-gist-gist) "GET"
   (format "/users/%s/gists" (or username (github-api-get-username api)))))

(defun github-gist-list-public ( )
  (github-api-authenticated-request
   (github-object-list-reader github-gist-gist) "GET" "/gists/public"))

(defun github-gist-list-starred ( )
  (github-api-authenticated-request
   (github-object-list-reader github-gist-gist) "GET" "/gists/starred"))

(defun github-gist-get (gist-or-id)
  (let (id transformer)
    (if (stringp gist-or-id)
        (setq id gist-or-id
              transformer (github-object-reader github-gist-gist))
      (setq id (oref gist-or-id :id)
            transformer (github-object-reader gist-or-id)))
    (github-api-authenticated-request
     transformer "GET" (format "/gists/%s" id))))

(defun github-gist-new (gist-stub)
  (github-api-authenticated-request
   (github-object-reader github-gist-gist) "POST" "/gists"
   (github-gist-gist-to-obj gist-stub)))

(defun github-gist-edit (gist)
  (github-api-authenticated-request
   (github-object-reader github-gist-gist) "PATCH"
   (format "/gists/%s"
           (oref gist :id))
   (github-gist-gist-to-obj gist)))

(defun github-gist-set-star (gist-or-id star)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (github-api-authenticated-request
     'ignore (if star "PUT" "DELETE")
     (format "/gists/%s/star" id))))

(defun github-gist-get-star (gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (github-api-authenticated-request
     'ignore "GET" (format "/gists/%s/star" id))))

(defun github-gist-fork (gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (github-api-authenticated-request
     (github-object-reader github-gist-gist) "POST"
     (format "/gists/%s/forks" id))))

(defun github-gist-delete (gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (github-api-authenticated-request
     'ignore "DELETE" (format "/gists/%s" id))))

(provide 'github-gist)
;;; github-gist.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
