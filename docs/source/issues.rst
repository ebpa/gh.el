========
 Issues
========

github.el is built on top of Eieio. The scope of this client library is to provide
plumbing primitives that will allow full use of the GitHub API.


github.el allows access to GitHub issues.

First, connect to the API::

  (github-issues-api "api")

This will OAuth connect to GitHub and return an API connection object.

The API connection object can be passed to issues methods::

  (github-issues-issue-list (github-issues-api "API") "sigma" "github.el")

The issue list has a class `github-api-paged-response` which has a member
`data` which can be used to retrieve the data sent back from GitHub::

  (oref
    (github-issues-issue-list (github-issues-api "API") "sigma" "github.el")
    data)

This returns a list of items of class `github-issues-issue`. You can
further `oref` those to get data. Putting it all together we might have::


  (defun fill-string (str)
    (with-temp-buffer
      (insert str)
      (fill-paragraph)
      (buffer-string)))

  (mapcar
     (lambda (issue)
       (insert
        (format
         "#%s %s -- %s\n%s\n\n"
         (oref it number) ; the issue number
         (oref it created_at) ; the data
         (fill-string (oref it title)) ; the title, filled
         (fill-string
           (replace-regexp-in-string
              "\r" "\n" (oref it body)))))  ; the body filled
       (oref
        (github-issues-issue-list ghcon "sigma" "github.el")
        data)))


