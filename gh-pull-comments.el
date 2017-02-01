;;; github-pull-comments.el --- pull request comments api for github

;; Copyright (C) 2014 Toni Reina

;; Author: Toni Reina <areina0@gmail.com>
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

;; (setf github-api-session (github-pull-comments-api "api" :sync nil :cache nil :num-retries 1))
;; (setf comments (github-pull-comments-list api "user" "repo" "pull request id"))
;; (setq my-comment (make-instance 'github-pull-comments-comment
;; 				:body "This is great!"
;; 				:path "README.md"
;; 				:position 2
;; 				:commit-id "commit sha"))
;; (github-pull-comments-new api "user" "repo" "pull request id" my-comment)

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'github-api)
(require 'github-auth)
(require 'github-common)

(require 'github-pulls)

(let ((ver "1.0.0"))
  (define-obsolete-function-alias
      'github-pull-comments-api 'github-pulls-api ver)
  (define-obsolete-function-alias
      'github-pull-comments-comment 'github-pulls-comment ver)

  (define-obsolete-function-alias
      'github-pull-comments-req-to-update 'github-comment-req-to-update ver)
  (define-obsolete-function-alias
      'github-pull-comments-req-to-create 'github-pulls-comment-req-to-create)

  (define-obsolete-function-alias
      'github-pull-comments-list 'github-pulls-comments-list ver)
  (define-obsolete-function-alias
      'github-pull-comments-get 'github-pulls-comments-get ver)
  (define-obsolete-function-alias
      'github-pull-comments-update 'github-pulls-comments-update ver)
  (define-obsolete-function-alias
      'github-pull-comments-new 'github-pulls-comments-new ver)
  (define-obsolete-function-alias
      'github-pull-comments-delete 'github-pulls-comments-delete ver))

(provide 'github-pull-comments)
;;; github-pull-comments.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
