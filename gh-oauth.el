;;; github-oauth.el --- oauth module for github.el

;; Copyright (C) 2012  Yann Hodique

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
(defclass github-oauth-password-authenticator (github-password-authenticator)
  ((remember :allocation :class :initform nil)))

(defun initialize-instance (&rest args) ;; (api github-oauth-api)
  ;; force password authentication for this API
  (let ((github-api-v3-authenticator 'github-oauth-password-authenticator))
    (call-next-method)))

;;;###autoload
(github-defclass github-oauth-authorization (github-ref-object)
  ((scopes :initarg :scopes)
   (token :initarg :token)
   (app :initarg :app :initform nil :marshal-type github-oauth-app)
   (updated-at :initarg :updated-at)
   (created-at :initarg :created-at)))

;;;###autoload
(github-defclass github-oauth-app (github-object)
  ((url :initarg :url)
   (name :initarg :name)))

(defun github-oauth-auth-list ( ) ;; (api github-oauth-api)
  (github-api-authenticated-request
   (github-object-list-reader github-oauth-authorization) "GET"
   (format "/authorizations")))

(defun github-oauth-auth-get (id) ;; (api github-oauth-api)
  (github-api-authenticated-request
   (github-object-reader github-oauth-authorization) "GET"
   (format "/authorizations/%s" id)))

(defun github-oauth-auth-new (&optional scopes) ;; (api github-oauth-api)
  (github-api-authenticated-request
   (github-object-reader github-oauth-authorization) "POST"
   (format "/authorizations") (list (cons 'scopes scopes)
                                    (cons 'note (format "github.el - %s"
                                                        (system-name))))))

(defun github-oauth-auth-update (id &optional scopes) ;; (api github-oauth-api)
  (github-api-authenticated-request
   (github-object-reader github-oauth-authorization) "PATCH"
   (format "/authorizations/%s" id) (list (cons 'scopes scopes))))

(defun github-oauth-auth-delete (id) ;; (api github-oauth-api)
  (github-api-authenticated-request
   nil "DELETE" (format "/authorizations/%s" id)))

(provide 'github-oauth)
;;; github-oauth.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
