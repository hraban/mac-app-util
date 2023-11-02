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

(defparameter *known-types* '(string bool integer float array dictionary))
(defparameter *atom-types* '(string bool integer float))
(defvar *plutil* "/usr/bin/plutil")

(defparameter *type-map*
  (mapcar (lambda (s) (cons (string-downcase (symbol-name s)) s)) *known-types*))

(defparameter *copyable-app-props*
  '("CFBundleDevelopmentRegion"
    "CFBundleDocumentTypes"
    "CFBundleGetInfoString"
    "CFBundleIconFile"
    "CFBundleIdentifier"
    "CFBundleInfoDictionaryVersion"
    "CFBundleName"
    "CFBundleShortVersionString"
    "CFBundleURLTypes"
    "NSAppleEventsUsageDescription"
    "NSAppleScriptEnabled"
    "NSDesktopFolderUsageDescription"
    "NSDocumentsFolderUsageDescription"
    "NSDownloadsFolderUsageDescription"
    "NSPrincipalClass"
    "NSRemovableVolumesUsageDescription"
    "NSServices"
    "UTExportedTypeDeclarations")
  "Based on a hunch, nothing scientific.")


;; Utility wrappers for inferior-shell

(defun sh (&rest args)
  (apply #'sh:run `(,@args :show ,(uiop:getenv "DEBUGSH"))))

(defun sh/ss (&rest args)
  (apply #'sh `(,@args :output (:string :stripped t))))

(defun list-of-strings-p (l)
  (and (consp l) (every #'stringp l)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))

(defun copy-atom (from to path type)
  "Copy a value from one plist to another"
  (let ((val (sh/ss `(,*plutil* -extract ,path raw -expect ,type #\o - -- ,from))))
    (sh `(,*plutil* -replace ,path ,(format NIL "-~(~A~)" type) ,val -- ,to))))

(defun copy-array (from to path)
  (let ((size (parse-integer (sh/ss `(,*plutil* -extract ,path raw -- ,from)))))
    ;; Initialize with a fresh array (idempotent)
    (sh `(,*plutil* -replace ,path -array -- ,to))
    (dolist (index (alex:iota size))
      (let* ((nested (format NIL "~A.~A" path index))
             (type (get-type from nested)))
        (copy-path from to nested type)))))

(defun copy-dict (from to path)
  (let ((keys (sh `(,*plutil* -extract ,path raw -- ,from)
                  :output :lines)))
    (sh `(,*plutil* -replace ,path -dictionary -- ,to))
    (dolist (key keys)
      (let* ((nested (format NIL "~A.~A" path (str:replace-all "." "\\." key)))
             (type (get-type from nested)))
        (copy-path from to nested type)))))

(defun get-type (plist path)
  (declare (type string plist)
           (type string path))
  (multiple-value-bind (type _ status) (sh/ss `(,*plutil* -type ,path -o - -- ,plist)
                                              :on-error nil)
    (declare (ignore _))
    (when (eql 0 status)
      (or (cdr (assoc type *type-map* :test #'equal))
          (error "Unknown type ~A at ~A:~A" type plist path)))))

(defun copy-path (from to path &optional (type (get-type from path)))
  (cond
    ((member type *atom-types*)
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

(defun resources (app)
  (format NIL "~A/Contents/Resources/" app))

(defun infoplist (app)
  (format NIL "~A/Contents/Info.plist" app))

(defun sync-icons (from to)
  "Remove all icons from TO apps resources, and copy all icons FROM to it"
  (destructuring-bind (from-cnts to-cnts) (mapcar #'resources (list from to))
    ;; 🤷
    (sh `(sh:and
          (find ,to-cnts -name "*.icns" -delete)
          (rsync :include "*.icns" :exclude "*" :recursive ,from-cnts ,to-cnts)))))

(defun create-trampoline (app trampoline)
  (let* ((cmd (format NIL "do shell script \"open '~A'\"" app)))
    (sh `(sh:and
          (rm -rf ,trampoline)
          ("/usr/bin/osacompile" #\o ,trampoline #\e ,cmd)))
    (sync-icons app trampoline)
    (copy-paths (infoplist app) (infoplist trampoline) *copyable-app-props*)))

(defun print-usage ()
  (format T "Usage: plist-copy FROM.app TO.app"))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (cond
      ((intersection args '("-h" "--help") :test #'equal)
       (print-usage)
       (uiop:quit 0))
      ((eql 2 (length args))
       (apply #'create-trampoline args))
      (t
       (print-usage)
       (uiop:quit 1)))))

(main)
