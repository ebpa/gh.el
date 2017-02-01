;;; github-common.el --- common objects for github.el

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

(require 'dash)
(require 'marshal)
(require 's)
(require 'github-profile)

(defgroup gh nil
  "Github API client libraries."
  :group 'applications)

(defcustom gh-use-local-git-config nil
  "If `t' use git configuration from the machine running
Emacs. This makes a difference when running with TRAMP."
  :type 'boolean
  :group 'gh)

;;; Helper functions

(defun github-read (obj field)
  (cdr (assoc field obj)))

(defun github-namespaced-key (key)
  (let ((profile (github-profile-current-profile)))
    (concat "github."
            (if (string= profile github-profile-default-profile)
                ""
              (concat profile "."))
            key)))

(defun github-config (key)
  "Returns a GitHub specific value from the global Git config."
  (let ((strip (lambda (string)
                 (if (> (length string) 0)
                     (substring string 0 (- (length string) 1))))))
    (funcall strip (github-command-to-string "config" (github-namespaced-key key)))))

(defun github-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (github-command-to-string "config" "--global" (github-namespaced-key key) value))

(defun github-command-to-string (&rest args)
  (let ((git (executable-find "git"))
        (runner (if gh-use-local-git-config
                    'call-process
                  'process-file)))
    (with-output-to-string
      (apply runner git nil standard-output nil args))))

;;; Base classes for common objects

;;;###autoload
(defun github-marshal-default-spec (slot)
  (let ((slot-name (symbol-name slot)))
    (list (cons 'alist
                (intern (s-replace "-" "_" slot-name))))))

;;;###autoload
(defmacro github-defclass (name superclass slots &rest options-and-doc)
  `(marshal-defclass ,name ,superclass ,slots ,@options-and-doc
                     :marshal-default-spec github-marshal-default-spec))

;;;###autoload
(github-defclass github-object ()
  ())

(defmethod github-object-read :static ((obj github-object) data)
  (let ((target (if (object-p obj) obj
                    (make-instance obj))))
    (when data
      (github-object-read-into target data))
    target))

(defmethod github-object-reader :static ((obj github-object))
  (apply-partially 'github-object-read obj))

(defmethod github-object-list-read :static ((obj github-object) data)
  (mapcar (github-object-reader obj) data))

(defmethod github-object-list-reader :static ((obj github-object))
  (apply-partially 'github-object-list-read obj))

(defmethod github-object-read-into ((obj github-object) data)
  (unmarshal obj data 'alist))

(defmethod slot-unbound ((obj github-object) cls slot-name fn)
  (if (eq fn 'oref) nil
      (call-next-method)))

;;;###autoload
(github-defclass github-ref-object (github-object)
  ((id :initarg :id)
   (url :initarg :url)
   (html-url :initarg :html-url)))

(defmethod github-ref-object-base ((obj github-ref-object))
  (let ((url (oref obj :url)))
    (--> (s-split "/" url t)
      (-slice it 2)
      (s-join "/" it)
      (concat "/" it))))

(defmethod github-ref-object-base (obj)
  (if (stringp obj) obj
    (error "illegal input for `github-ref-object-base'")))

;;;###autoload
(github-defclass github-user (github-ref-object)
  ((login :initarg :login)
   (gravatar-url :initarg :gravatar-url))
  "Github user object")

;;;###autoload
(github-defclass github-comment (github-ref-object)
  ((body :initarg :body)
   (user :initarg :user :initform nil :marshal-type github-user)
   (created-at :initarg :created_at)
   (updated-at :initarg :updated_at))
  "Github comment object")

(defmethod github-comment-req-to-update ((req github-comment))
  `(("body" . ,(oref req :body))))

(provide 'github-common)
;;; github-common.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
