#!/usr/bin/env sbcl --script

(require "asdf")
(require "uiop")
(asdf:load-system "inferior-shell")
(asdf:load-system "trivia")

(defpackage #:script
  (:use #:cl)
  (:local-nicknames (#:sh #:inferior-shell)))

(in-package #:script)

(defparameter known-types '(string bool integer float array dictionary))
(defparameter atom-types '(string bool integer float))

(defparameter type-map
  (mapcar (lambda (s) (cons (string-downcase (symbol-name s)) s)) known-types))

(deftype atom-type ()
  `(member ,@atom-types))

(defun list-of-strings-p (l)
  (and (consp l) (every #'stringp l)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))

(defun copy-value (from to path)
  "Copy a value from one plist to another"
  (let ((type (get-type from path)))
    (etypecase type
      (atom-type
       (let ((val (sh:run/ss `(plutil -extract ,path raw -expect ,type #\o - -- ,from))))
         (sh:run `(plutil -replace ,path ,(format NIL "-~(~A~)" type) ,val -- ,to)))))))

(defun get-type (plist path)
  (declare (type string plist)
           (type string path))
  (let ((type (sh:run/ss `(plutil -type ,path -o - -- ,plist))))
    (or (cdr (assoc type type-map :test #'equal))
        (error "Unknown type ~A at ~A:~A" type plist path))))

(defun copy-paths (from to paths)
  (declare (type list-of-strings paths))
  (dolist (path paths)
    (copy-value from to path)))

(defun main ()
  (trivia:match (uiop:command-line-arguments)
    ((list* from to paths)
     (copy-paths from to paths))
    (_ (error "Usage: app-plist-copy FROM TO PATH..."))))

(main)
