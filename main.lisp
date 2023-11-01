#!/usr/bin/env sbcl --script

(require "asdf")
(require "uiop")
(asdf:load-system "inferior-shell")
(asdf:load-system "trivia")

(defpackage #:script
  (:use #:cl)
  (:local-nicknames (#:sh #:inferior-shell)))

(in-package #:script)

(defun leafp (type)
  (member type '("bool" "string" "integer" "float") :test #'equal))

(deftype leaf ()
  `(satisfies leafp))

(defun list-of-strings-p (l)
  (and (consp l) (every #'stringp l)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))

(defun copy-atom (from to path type)
  "Copy a single leaf value (string, boolean, ..) from one plist to the other"
  (declare (type leaf type))
  (let ((val (sh:run/ss `(plutil -extract ,path raw -expect ,type #\o - -- ,from))))
    (sh:run `(plutil -replace ,path ,(format NIL "-~A" type) ,val -- ,to))))

(defun get-type (plist path)
  (declare (type string plist)
           (type string path))
  (sh:run/ss `(plutil -type ,path -o - -- ,plist)))

(defun copy-atoms (from to paths)
  (declare (type list-of-strings paths))
  (dolist (path paths)
    (let ((type (get-type from path)))
      (trivia:ematch type
        ((satisfies leafp)
         (copy-atom from to path type))))))

(defun main ()
  (trivia:match (uiop:command-line-arguments)
    ((list* from to paths)
     (copy-atoms from to paths))
    (_ (error "Usage: app-plist-copy FROM TO PATH..."))))

(main)
