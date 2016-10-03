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

(defmacro gh-search-method-builder (method-name uri process-result-function)
  `(defmethod ,method-name ((search-api gh-search-api)
                               query-string &optional page-limit
                               &rest additional-arguments)
     (unless (and (stringp query-string) (> (length query-string) 1))
       (error "a non-empty query string must be provided to github search"))
     (gh-api-authenticated-request
      search-api
      (apply-partially (quote ,process-result-function) search-api)
      "GET" ,uri nil
      `((q . ,query-string) ,@additional-arguments) page-limit)))

(defmacro gh-search-process-method-builder (method-name class)
  `(defmethod ,method-name ((search-api gh-search-api) data)
     (unless (listp data)
       (error "Did not recieve a list from the search query"))
     (let ((items (assoc 'items data)))
       (unless items
         (error "Search query did not return items"))
       (gh-object-list-read ,class (cdr items)))))

(gh-search-process-method-builder gh-process-repo-search-result gh-repos-repo)
(gh-search-process-method-builder gh-process-user-search-result gh-users-user)
(gh-search-method-builder gh-search-repos "/search/repositories"
                          gh-process-repo-search-result)
(gh-search-method-builder gh-search-users "/search/users"
                          gh-process-user-search-result)

(provide 'gh-search)
;;; gh-search.el ends here
