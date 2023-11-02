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

(defgeneric rm-rf (p))

(defmethod rm-rf ((p string))
  (rm-rf (uiop:parse-native-namestring p :ensure-directory t)))

(defmethod rm-rf ((p pathname))
  (uiop:delete-directory-tree
   (uiop:ensure-directory-pathname p)
   :validate t
   :if-does-not-exist :ignore))

(defmacro with-temp-dir ((dname) &body body)
  `(let ((,dname (uiop:ensure-directory-pathname (sh/ss '(mktemp #\d)))))
     (unwind-protect (progn ,@body)
       (rm-rf ,dname))))

(defun list-of-strings-p (l)
  (and (consp l) (every #'stringp l)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))


;;; mktrampoline

(defun copy-paths (from to paths)
  (declare (type list-of-strings paths))
  (let ((keys (cl-json:encode-json-to-string *copyable-app-props*))
        ;; For an object, keep only those keys from list “keys”
        (jqfilter "to_entries |[.[]| select(.key as $item| $keys | index($item) >= 0) ] | from_entries"))
    (with-temp-dir (d)
      (uiop:with-current-directory (d)
        (uiop:copy-file from "orig")
        (uiop:copy-file to "bare-wrapper")
        (sh `(sh:and (,*plutil* -convert json -- orig)
                     (,*plutil* -convert json -- bare-wrapper)
                     (jq :argjson keys ,keys ,jqfilter (< orig) (> filtered))
                     (sh:pipe (cat bare-wrapper filtered)
                              (jq #\s add (> final)))
                     (,*plutil* -convert xml1 -- final)))
        (uiop:copy-file "final" to)))))

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
  (let* ((aapp (merge-pathnames app (uiop:getcwd)))
         (atrampoline (merge-pathnames trampoline (uiop:getcwd)))
         (cmd (format NIL "do shell script \"open '~A'\"" aapp)))
    (rm-rf atrampoline)
    (sh `("/usr/bin/osacompile" #\o ,atrampoline #\e ,cmd))
    (sync-icons aapp atrampoline)
    (copy-paths (infoplist aapp) (infoplist atrampoline) *copyable-app-props*)))


;;; sync-dock

(defun realpath (f)
  "Transform a string, optionally relative, into a an absolute path.

Also resolves symlinks, if relevant.
"
  (uiop:ensure-pathname f
                        :want-pathname t
                        :ensure-absolute t
                        :defaults (uiop:getcwd)
                        :want-existing t
                        :resolve-symlinks t))

(defun sync-dock (apps)
  "Every element must be a pathname to a real directory, not a symlink"
  ;; Filtering for /nix/store is not technically part of the docs but let’s be
  ;; conservative for now.
  (let ((persistents (sh '(sh:pipe (dockutil #\L)
                           (grep "file:///nix/store")
                           ;; Whatever, this works.
                           (grep "persistentApps")
                           ;; I feel like using the bundle ID would be
                           ;; cleaner (org.gnu.Emacs etc) but dockutil only
                           ;; works reliably when I use the “bundle name”,
                           ;; which is just the file’s basename without
                           ;; extension. Ok.
                           (cut #\f 1))
                         :output :lines)))
    (dolist (existing persistents)
      (alex:when-let ((app (find existing apps :test #'equal :key #'pathname-name)))
        ;; I was passed an app with the same name as an existing persistent dock
        ;; item.  Yes this restarts after every item but I don’t know how to
        ;; only restart exactly once.
        (sh `(dockutil :add ,(realpath app) :replacing ,existing))))))


;;; CLI

(defun print-usage ()
  (format T "Usage:

    mac-app-util mktrampoline FROM.app TO.app
    mac-app-util sync-dock Foo.app Bar.app ...

mktrampline creates a “trampoline” application launcher that immediately
launches another application.

sync-dock updates persistent items in your dock if any of the given apps has the
same name. This can be used to programmatically keep pinned items in your dock
up to date with potential new versions of an app outside of the /Applications
directory, without having to check which one is pinned etc.
"))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (if (intersection args '("-h" "--help") :test #'equal)
        (progn
          (print-usage)
          (uiop:quit 0))
        (trivia:match args
          ((list "mktrampoline" from to)
           (create-trampoline from to))
          ((list* "sync-dock" apps)
           (sync-dock apps))
          (_
           (print-usage)
           (uiop:quit 1))))))

(main)
