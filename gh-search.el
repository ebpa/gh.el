;;; gh-search.el --- repository search for gh.el
;; Copyright (C) 2016  Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>

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

;;

;;; Code:

(require 'gh-users)
(require 'gh-repos)

(defun gh-search-repos (query-string &optional page-limit &rest additional-arguments)
  ""
  (let* ((result (gh-api-authenticated-request
                  nil
                  "GET" "/search/repositories" nil
                  `((q . ,query-string) ,@additional-arguments) page-limit))
         (data (oref result :data)))
    data))

(defun gh-search-users (query-string &optional page-limit &rest additional-arguments)
  ""
  (let* ((result (gh-api-authenticated-request
                  nil
                  "GET" "/search/users" nil
                  `((q . ,query-string) ,@additional-arguments) page-limit))
         (data (oref result :data)))
    data))

(gh-search-users "ebpa")

;; (unless (and (stringp query-string) (> (length query-string) 1))
;;   (error "a non-empty query string must be provided to github search"))

(defun gh-search-process-search-result (data)
  (unless (listp data)
    (error "Did not recieve a list from the search query"))
  (let ((items (assoc 'items data)))
    (unless items
      (error "Search query did not return items"))
    (gh-object-list-read ,class (cdr items))))

(provide 'gh-search)
;;; gh-search.el ends here
