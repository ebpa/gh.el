;;; github-issue-comments.el --- issue comments api for github

;; Copyright (C) 2014 Travis Thieman

;; Author: Travis Thieman <travis.thieman@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODOS:
;;   * Support listing all comments in a repository

;; Basic usage:

;; (setf github-api-session (github-issue-comments-api "api" :sync nil :cache nil :num-retries 1))
;; (setf comments (github-issue-comments-list api "user" "repo" "issue id"))
;; (setq my-comment (make-instance 'github-issue-comments-comment :body "This is great!"))
;; (github-issue-comments-new api "user" "repo" "issue id" my-comment)

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'github-api)
(require 'github-auth)
(require 'github-common)

(require 'github-issues)

(let ((ver "1.0.0"))
  (define-obsolete-function-alias
      'github-issue-comments-api 'github-issues-api ver)
  (define-obsolete-function-alias
      'github-issue-comments-comment 'github-issues-comment ver)

  (define-obsolete-function-alias
      'github-issue-comments-req-to-update 'github-comment-req-to-update ver)

  (define-obsolete-function-alias
      'github-issue-comments-list 'github-issues-comments-list ver)
  (define-obsolete-function-alias
      'github-issue-comments-get 'github-issues-comments-get ver)
  (define-obsolete-function-alias
      'github-issue-comments-update 'github-issues-comments-update ver)
  (define-obsolete-function-alias
      'github-issue-comments-new 'github-issues-comments-new ver)
  (define-obsolete-function-alias
      'github-issue-comments-delete 'github-issues-comments-delete ver))

(provide 'github-issue-comments)
;;; github-issue-comments.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
