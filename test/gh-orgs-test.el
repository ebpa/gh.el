;;; github-orgs-test.el --- test for github-orgs.el

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

(require 'github-test)
(require 'github-orgs)

(defun github-orgs-test:test-regular-org-stub (org)
  (should (equal (oref org :id) 1))
  (should (equal (oref org :login) "github"))
  (should (equal "https://github.com/images/error/octocat_happy.gif" (oref org :avatar-url))))

(defun github-orgs-test:test-regular-org (org)
  (github-orgs-test:test-regular-org-stub org)
  (should (equal (oref org :public-gists) 1))
  (should (equal (oref org :public-repos) 2)))

(ert-deftest github-orgs-test:regular-list ()
  (let* ((api (github-test-mock-api 'github-orgs-api))
         (orgs
          (github-test-with-traces-buffers ((orgs-buf "list_orgs_sample.txt"))
            (github-test-mock-url ((:record-cls mocker-stub-record
                                             :output orgs-buf))
                               (oref (github-orgs-list "dummy") :data)))))
    (should (equal (length orgs) 1))
    (let ((org (car orgs)))
      (should (object-of-class-p org 'github-orgs-org-stub))
      (github-orgs-test:test-regular-org-stub org))))

(ert-deftest github-orgs-test:regular-get ()
  (let* ((api (github-test-mock-api 'github-orgs-api))
         (org
          (github-test-with-traces-buffers ((orgs-buf "get_org_sample.txt"))
            (github-test-mock-url ((:record-cls mocker-stub-record
                                             :output orgs-buf))
                               (oref (github-orgs-get "github") :data)))))
    (should (object-of-class-p org 'github-orgs-org))
    (github-orgs-test:test-regular-org org)))

(ert-deftest github-orgs-test:regular-update ()
  (let* ((api (github-test-mock-api 'github-orgs-api))
         (org-stub
          (make-instance 'github-orgs-org
                         :login "github"
                         :id 1
                         :url "https://api.github.com/orgs/1"
                         :avatar-url "https://github.com/images/error/octocat_happy.gif"))
         (org
          (github-test-with-traces-buffers ((orgs-buf "get_org_sample.txt"))
            (github-test-mock-url ((:record-cls mocker-stub-record
                                             :output orgs-buf))
                               (oref (github-orgs-update org-stub) :data)))))
    (should (object-of-class-p org 'github-orgs-org))
    (github-orgs-test:test-regular-org org)))

(provide 'github-orgs-test)
;;; github-orgs-test.el ends here
