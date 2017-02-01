;;; github-issues-test.el --- test fir github-issues.el

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
(require 'github-issues)

(defun github-issues-test:test-regular-issue (issue)
  (should (equal (oref issue :number) 1347))
  (should (equal (oref issue :state) "open")))

(ert-deftest github-issues-test:regular-list ()
  (let* ((api (github-test-mock-api 'github-issues-api))
         (issues
          (github-test-with-traces-buffers ((gists-buf "list_issues_sample.txt"))
            (github-test-mock-url ((:record-cls mocker-stub-record
                                             :output gists-buf))
                               (oref
                                (github-issues-issue-list "octocat"
                                                      "Hello-World")
                                :data)))))
    (should (equal (length issues) 1))
    (let ((issue (car issues)))
      (should (object-of-class-p issue 'github-issues-issue))
      (github-issues-test:test-regular-issue issue))))

(provide 'github-issues-test)
;;; github-issues-test.el ends here
