;;; github-auth.el --- authentication for github.el

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

(require 'github-profile)
(require 'github-common)
(require 'github-url)

(defgroup github-auth nil
  "Github authentication."
  :group 'github)

(defvar github-auth-alist nil)

(defun github-auth-remember (profile key value)
  (let ((cell (assoc profile github-auth-alist)))
    (when (not cell)
      (setq cell (cons profile nil))
      (setq github-auth-alist (append github-auth-alist (list cell))))
    (setcdr cell (plist-put (cdr cell) key value))))

(defun github-auth-get-username ()
  (let* ((profile (github-profile-current-profile))
         (user (or (plist-get (cdr (assoc profile github-auth-alist)) :username)
                   (plist-get (cdr (assoc profile github-profile-alist)) :username)
                   (github-config "user"))))
    (when (not user)
      (setq user (read-string "GitHub username: "))
      (github-set-config "user" user))
    (github-auth-remember profile :username user)
    user))

(defun github-auth-get-password (&optional remember)
  (let* ((profile (github-profile-current-profile))
         (pass (or (plist-get (cdr (assoc profile github-auth-alist)) :password)
                   (plist-get (cdr (assoc profile github-profile-alist)) :password)
                   (github-config "password"))))
    (when (not pass)
      (setq pass (read-passwd "GitHub password: "))
      (when remember
        (github-set-config "password" pass)))
    (when remember
      (github-auth-remember profile :password pass))
    pass))

(declare-function 'github-oauth-auth-new "github-oauth")

(defun github-auth-get-oauth-token ()
  (let* ((profile (github-profile-current-profile))
         (token (or (plist-get (cdr (assoc profile github-auth-alist)) :token)
                    (plist-get (cdr (assoc profile github-profile-alist)) :token)
                    (github-config "oauth-token"))))
    (when (not token)
      (let* ((api (make-instance 'github-oauth-api))
             (tok (and (fboundp 'github-oauth-auth-new)
                       (oref (oref (funcall 'github-oauth-auth-new api
                                            '(user repo gist)) :data)
                             :token))))
        (setq token (or tok (read-string "GitHub OAuth token: ")))
        (github-set-config "oauth-token" token)))
    (github-auth-remember profile :token token)
    token))

;;;###autoload
(defclass github-authenticator ()
  ((username :initarg :username :initform nil))
  "Abstract authenticator")

(defmethod initialize-instance ((auth github-authenticator) &rest args)
  (call-next-method)
  (or (oref auth :username)
      (oset auth :username (github-auth-get-username))))

(defmethod github-auth-modify-request ((auth github-authenticator) req)
  req)

;;;###autoload
(defclass github-auth-2fa-callback (github-url-callback)
  ((req :initarg :req :initform nil))
  "2-factor callback")

(defmethod github-url-callback-run ((cb github-auth-2fa-callback) resp)
  (when (equal (oref resp :http-status) 401)
    (let* ((otp-header "X-GitHub-OTP")
           (h (assoc otp-header (oref resp :headers))))
      (when (and h (string-prefix-p "required;" (cdr h)))
        (let ((otp (read-from-minibuffer "Enter dual-factor auth code: "))
              (req (oref cb :req)))
          ;; reset resp
          (oset resp :data nil)
          (oset resp :data-received nil)

          (object-add-to-list req :headers
                              (cons otp-header otp))
          (github-url-run-request req resp))))))

;;;###autoload
(defclass github-password-authenticator (github-authenticator)
  ((password :initarg :password :protection :private :initform nil)
   (remember :allocation :class :initform t))
  "Password-based authenticator")

(defmethod initialize-instance ((auth github-password-authenticator) &rest args)
  (call-next-method)
  (or (oref auth :password)
      (oset auth :password (github-auth-get-password (oref auth remember)))))

(defmethod github-auth-modify-request ((auth github-password-authenticator) req)
  (object-add-to-list req :headers
                      (cons "Authorization"
                            (concat "Basic "
                                    (base64-encode-string
                                     (format "%s:%s" (oref auth :username)
                                             (encode-coding-string
                                              (oref auth :password) 'utf-8))))))
  (object-add-to-list req :install-callbacks
                      (make-instance github-auth-2fa-callback :req req))
  req)

;;;###autoload
(defclass github-oauth-authenticator (github-authenticator)
  ((token :initarg :token :protection :private :initform nil))
  "Oauth-based authenticator")

(defmethod initialize-instance ((auth github-oauth-authenticator) &rest args)
  (call-next-method)
  (or (oref auth :token)
      (oset auth :token (github-auth-get-oauth-token))))

(defmethod github-auth-modify-request ((auth github-oauth-authenticator) req)
  (object-add-to-list req :headers
                      (cons "Authorization"
                            (encode-coding-string
                             (format "token %s" (oref auth :token)) 'utf-8)))
  req)

(provide 'github-auth)
;; to avoid circular dependencies...
(require 'github-oauth)
;;; github-auth.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
