;;; github-api.el --- api definition for github.el

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

(require 'json)

(require 'github-profile)
(require 'github-url)
(require 'github-auth)
(require 'github-cache)

(require 'logito)

(defgroup github-api nil
  "Github API."
  :group 'github)

(defvar github-api-api-session nil "Current GitHub API session.")
;;(setq github-api-session (github-api "API"))

(defcustom github-api-username-filter 'github-api-enterprise-username-filter
  "Filter to apply to usernames to build URL components"
  :type 'function
  :group 'github-api)

;;;###autoload
(defclass github-api ()
  ((sync :initarg :sync :initform t)
   (cache :initarg :cache :initform nil)
   (base :initarg :base :type string)
   (profile :initarg :profile :type string)
   (auth :initarg :auth :initform nil)
   (data-format :initarg :data-format :initform :json)
   (num-retries :initarg :num-retries :initform 0)
   (log :initarg :log :initform nil))
  "Github API")

(defun logito-log (level tag string &rest objects) ;; (api github-api)
  (apply 'logito-log (oref github-api-session :log) level tag string objects))

(defun github-api-set-default-auth (api auth)
  (let ((auth (or (oref api :auth) auth))
        (cache (oref api :cache))
        (classname (symbol-name (funcall (if (fboundp 'eieio-object-class)
                                             'eieio-object-class
                                           'object-class)
                                         api))))
    (oset api :auth auth)
    (unless (or (null cache)
                (and (eieio-object-p cache)
                     (object-of-class-p cache 'github-cache)))
      (oset api :cache (make-instance
                             github-cache
                        :object-name
                        (format "gh/%s/%s"
                                classname
                                (github-api-get-username api)))))))

(defun github-api-expand-resource (resource)
  resource)

(defun github-api-enterprise-username-filter (username)
  (replace-regexp-in-string (regexp-quote ".") "-" username))

(defun github-api-get-username () ;; (api github-api)
  (let ((username (oref (oref github-api-session :auth) :username)))
    (funcall github-api-username-filter username)))

(defcustom github-api-v3-authenticator 'github-oauth-authenticator
  "Authenticator for Github API v3"
  :type '(choice (const :tag "Password" github-password-authenticator)
                 (const :tag "OAuth" github-oauth-authenticator))
  :group 'github-api)

(defmethod initialize-instance ((api github-api) &rest args)
  (call-next-method)
  (let ((github-profile-current-profile (github-profile-current-profile)))
    (oset api :profile (github-profile-current-profile))
    (oset api :base (github-profile-url))
    (github-api-set-default-auth api
                             (or (oref api :auth)
                                 (funcall github-api-v3-authenticator "auth")))))

;;;###autoload
(defclass github-api-response (github-url-response)
  ())

(defun github-api-json-decode (repr)
  (if (or (null repr) (string= repr ""))
      'empty
    (let ((json-array-type 'list))
      (json-read-from-string repr))))

(defun github-api-json-encode (json)
  (encode-coding-string (json-encode-list json) 'utf-8))

(defmethod github-url-response-set-data ((resp github-api-response) data)
  (call-next-method resp (github-api-json-decode data)))

;;;###autoload
(defclass github-api-request (github-url-request)
  ())

;;;###autoload
(defclass github-api-paged-request (github-api-request)
  ((page-limit :initarg :page-limit :initform -1)))

;;;###autoload
(defclass github-api-paged-response (github-api-response)
  ())

(defmethod github-api-paging-links ((resp github-api-paged-response))
  (let ((links-header (cdr (assoc "Link" (oref resp :headers)))))
    (when links-header
      (loop for item in (split-string links-header ", ")
            when (string-match "^<\\(.*\\)>; rel=\"\\(.*\\)\"" item)
            collect (cons (match-string 2 item)
                          (match-string 1 item))))))

(defmethod github-url-response-set-data ((resp github-api-paged-response) data)
  (let ((previous-data (oref resp :data))
        (next (cdr (assoc "next" (github-api-paging-links resp)))))
    (call-next-method)
    (oset resp :data (append previous-data (oref resp :data)))
    (when (and next (not (equal 304 (oref resp :http-status))))
      (let* ((req (oref resp :-req))
             (last-page-limit (oref req :page-limit))
             (this-page-limit (if (numberp last-page-limit) (- last-page-limit 1) -1)))
        (oset req :page-limit this-page-limit)
        (unless (eq (oref req :page-limit) 0)
          ;; We use an explicit check for 0 since -1 indicates that
          ;; paging should continue forever.
          (oset resp :data-received nil)
          (oset req :url next)
          ;; Params need to be set to nil because the next uri will
          ;; already have query params. If params are non-nil this will
          ;; cause another set of params to be added to the end of the
          ;; string which will override the params that are set in the
          ;; next link.
          (oset req :query nil)
          (github-url-run-request req resp))))))

(defun github-api-authenticated-request (transformer method resource &optional data params page-limit)
  (let* ((api github-api-session)
         (fmt (oref api :data-format))
         (headers (cond ((eq fmt :form)
                         '(("Content-Type" .
                            "application/x-www-form-urlencoded")))
                        ((eq fmt :json)
                         '(("Content-Type" .
                            "application/json")))))
         (cache (oref api :cache))
         (key (list resource
                         method
                         (sha1 (format "%s" transformer))))
         (cache-key (and cache
                         (member method (oref cache safe-methods))
                         key))
         (has-value (and cache-key (pcache-has cache cache-key)))
         (value (and has-value (pcache-get cache cache-key)))
         (is-outdated (and has-value (github-cache-outdated-p cache cache-key)))
         (etag (and is-outdated (github-cache-etag cache cache-key)))
         (req
          (and (or (not has-value)
                   is-outdated)
               (github-auth-modify-request
                (oref api :auth)
                ;; TODO: use github-api-paged-request only when needed
                (make-instance 'github-api-paged-request
                               :method method
                               :url (concat (oref api :base)
                                            (github-api-expand-resource resource))
                               :query params
                               :headers (if etag
                                            (cons (cons "If-None-Match" etag)
                                                  headers)
                                            headers)
                               :data (or (and (eq fmt :json)
                                              (github-api-json-encode data))
                                         (and (eq fmt :form)
                                              (github-url-form-encode data))
                                         "")
                               :page-limit page-limit)))))
    (cond ((and has-value ;; got value from cache
                (not is-outdated))
           (make-instance 'github-api-response :data-received t :data value))
          (cache-key ;; no value, but cache exists and method is safe
           (let ((resp (make-instance github-api-response
                                      :transform transformer)))
             (github-url-run-request req resp)
             (github-url-add-response-callback
              resp (make-instance 'github-api-callback :cache cache :key cache-key
                                  :revive etag))
             resp))
          (cache ;; unsafe method, cache exists
           (pcache-invalidate cache key)
           (github-url-run-request req (make-instance
                                    github-api-response
                                    :transform transformer)))
          (t ;; no cache involved
           (github-url-run-request req (make-instance
                                    github-api-response
                                    :transform transformer))))))

;;;###autoload
(defclass github-api-callback (github-url-callback)
  ((cache :initarg :cache)
   (key :initarg :key)
   (revive :initarg :revive)))

(defmethod github-url-callback-run ((cb github-api-callback) resp)
  (let ((cache (oref cb :cache))
        (key (oref cb :key)))
    (if (and (oref cb :revive) (equal (oref resp :http-status) 304))
        (progn
          (github-cache-revive cache key)
          (oset resp :data (pcache-get cache key)))
      (pcache-put cache key (oref resp :data))
      (github-cache-set-etag cache key
                         (cdr (assoc "ETag" (oref resp :headers)))))))

(define-obsolete-function-alias 'github-api-add-response-callback
  'github-url-add-response-callback "0.6.0")

(provide 'github-api)
;;; github-api.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
