#!/usr/bin/env sbcl --script

(require "asdf")
(require "uiop")

(asdf:load-system "alexandria")
(asdf:load-system "cl-interpol")
(asdf:load-system "cl-json")
(asdf:load-system "inferior-shell")
(asdf:load-system "str")
(asdf:load-system "trivia")

(defpackage #:script
  (:use #:cl)
  (:local-nicknames (#:alex #:alexandria)
                    (#:sh #:inferior-shell)))

(in-package #:script)

(named-readtables:in-readtable :interpol-syntax)

(defvar *plutil* "/usr/bin/plutil")

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

(defun sh (&rest args)
  ;; This is my personal convention; set DEBUGSH to anything to effect set -x
  (apply #'sh:run `(,@args :show ,(uiop:getenv "DEBUGSH"))))

(defun sh/ss (&rest args)
  (apply #'sh `(,@args :output (:string :stripped t))))

(defun rm-rf (p)
  (declare (string p))
  (uiop:delete-directory-tree
   (uiop:parse-native-namestring p :ensure-directory t)
   :validate t
   :if-does-not-exist :ignore))

(defmacro with-temp-dir ((dname) &body body)
  `(let ((,dname (sh/ss '(mktemp #\d))))
     (unwind-protect (progn ,@body)
       (rm-rf ,dname))))

(defun list-of-strings-p (l)
  (and (consp l) (every #'stringp l)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))

(defun copy-paths (from to paths)
  (declare (type list-of-strings paths))
  (let ((keys (cl-json:encode-json-to-string *copyable-app-props*))
        ;; For an object, keep only those keys from list “keys”
        (jqfilter "to_entries |[.[]| select(.key as $item| $keys | index($item) >= 0) ] | from_entries"))
    (with-temp-dir (d)
      (uiop:with-current-directory (d)
        (uiop:copy-file from "from")
        (uiop:copy-file to "bare-wrapper")
        (sh `(sh:and (,*plutil* -convert json -- from)
                     (,*plutil* -convert json -- bare-wrapper)
                     (jq :argjson keys ,keys ,jqfilter (< from) (> filtered))
                     (sh:pipe (cat bare-wrapper filtered)
                              (jq #\s add (> to)))
                     (,*plutil* -convert xml1 -- to)))
        (uiop:copy-file "to" to)))))

(defun resources (app)
  #?"${app}/Contents/Resources/")

(defun infoplist (app)
  #?"${app}/Contents/Info.plist")

(defun sync-icons (from to)
  "Remove all icons from TO apps resources, and copy all icons FROM to it"
  (destructuring-bind (from-cnts to-cnts) (mapcar #'resources (list from to))
    ;; 🤷
    (sh `(sh:and
          (find ,to-cnts -name "*.icns" -delete)
          (rsync :include "*.icns" :exclude "*" :recursive ,from-cnts ,to-cnts)))))

(defun create-trampoline (app trampoline)
  (let* ((cmd (format NIL "do shell script \"open '~A'\"" app)))
    (rm-rf trampoline)
    (sh `("/usr/bin/osacompile" #\o ,trampoline #\e ,cmd))
    (sync-icons app trampoline)
    (copy-paths (infoplist app) (infoplist trampoline) *copyable-app-props*)))


;;; CLI

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
