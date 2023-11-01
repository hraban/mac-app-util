#!/usr/bin/env sbcl --script

(require "asdf")
(require "uiop")

(asdf:load-system "alexandria")
(asdf:load-system "inferior-shell")
(asdf:load-system "str")
(asdf:load-system "trivia")

(defpackage #:script
  (:use #:cl)
  (:local-nicknames (#:alex #:alexandria)
                    (#:sh #:inferior-shell)))

(in-package #:script)

(defparameter known-types '(string bool integer float array dictionary))
(defparameter atom-types '(string bool integer float))
(defvar plutil "/usr/bin/plutil")

(defparameter type-map
  (mapcar (lambda (s) (cons (string-downcase (symbol-name s)) s)) known-types))

(deftype atom-type ()
  `(member ,@atom-types))

(defun list-of-strings-p (l)
  (and (consp l) (every #'stringp l)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))

(defun copy-atom (from to path type)
  "Copy a value from one plist to another"
  (let ((val (sh:run/ss `(,plutil -extract ,path raw -expect ,type #\o - -- ,from))))
    (sh:run `(,plutil -replace ,path ,(format NIL "-~(~A~)" type) ,val -- ,to))))

(defun copy-array (from to path)
  (let ((size (parse-integer (sh:run/ss `(,plutil -extract ,path raw -- ,from)))))
    ;; Initialize with a fresh array (idempotent)
    (sh:run `(,plutil -replace ,path -array -- ,to))
    (dolist (index (alex:iota size))
      (let* ((nested (format NIL "~A.~A" path index))
             (type (get-type from nested)))
        (copy-path from to nested type)))))

(defun copy-dict (from to path)
  (let ((keys (sh:run/lines `(,plutil -extract ,path raw -- ,from))))
    (sh:run `(,plutil -replace ,path -dictionary -- ,to))
    (dolist (key keys)
      (let* ((nested (format NIL "~A.~A" path (str:replace-all "." "\\." key)))
             (type (get-type from nested)))
        (copy-path from to nested type)))))

(defun get-type (plist path)
  (declare (type string plist)
           (type string path))
  (multiple-value-bind (type _ status) (sh:run/ss `(,plutil -type ,path -o - -- ,plist)
                                                  :on-error nil)
    (when (eql 0 status)
      (or (cdr (assoc type type-map :test #'equal))
          (error "Unknown type ~A at ~A:~A" type plist path)))))

(defun copy-path (from to path &optional (type (get-type from path)))
  (cond
    ((member type atom-types)
     (copy-atom from to path type))
    ((eql 'array type)
     (copy-array from to path))
    ((eql 'dictionary type)
     (copy-dict from to path))
    ((null type)
     ;; skip
     )))

(defun copy-paths (from to paths)
  (declare (type list-of-strings paths))
  (dolist (path paths)
    (copy-path from to path)))

(defun print-usage ()
  (format T "Usage: plist-copy FROM-FILE TO-FILE KEY1 KEY2 KEY3..."))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (if (intersection args '("-h" "--help") :test #'equal)
        (progn
          (print-usage)
          (uiop:quit 0))
        (trivia:match args
          ((list* from to paths)
           (copy-paths from to paths))
          (_
           (print-usage)
           (uiop:quit 1))))))

(main)
