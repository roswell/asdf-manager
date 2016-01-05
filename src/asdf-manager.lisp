(in-package :cl-user)
(defpackage asdf-manager
  (:use :cl)
  (:shadowing-import-from :uiop
                          :merge-pathnames)
  (:export :+available-versions+
           :valid-version-p
           :version
           :manager
           :manager-directory
           :archive-url
           :versions-directory
           :archive-pathname
           :download
           :extract
           :download-extract-delete)
  (:documentation "ASDF manager interface."))
(in-package :asdf-manager)

;;; Constants

(defparameter +available-versions+
  #(:3.1.6.6
    :3.1.6.5
    :3.1.6.4
    :3.1.6.3
    :3.1.6.2
    :3.1.6.1
    :3.1.6
    :3.1.5.20
    :3.1.5.19
    :3.1.5.18
    :3.1.5.17
    :3.1.5.16
    :3.1.5.15
    :3.1.5.14
    :3.1.5.13
    :3.1.5.12
    :3.1.5.11
    :3.1.5.10
    :3.1.5.9
    :3.1.5.8
    :3.1.5.7
    :3.1.5.6
    :3.1.5.5
    :3.1.5.4
    :3.1.5.3
    :3.1.5.2
    :3.1.5.1
    :3.1.5
    :3.1.4.25
    :3.1.4.24
    :3.1.4.23
    :3.1.4.22
    :3.1.4.21
    :3.1.4.20
    :3.1.4.19
    :3.1.4.18
    :3.1.4.17
    :3.1.4.16
    :3.1.4.15
    :3.1.4.13
    :3.1.4.12
    :3.1.4.11
    :3.1.4.10
    :3.1.4.9
    :3.1.4.8
    :3.1.4.7
    :3.1.4.6
    :3.1.4.5
    :3.1.4.4
    :3.1.4.3
    :3.1.4.2
    :3.1.4.1
    :3.1.4
    :3.1.3.10
    :3.1.3.9
    :3.1.3.7
    :3.1.3.6
    :3.1.3.5
    :3.1.3.3
    :3.1.3.2
    :3.1.3.1
    :3.1.3
    :3.1.2.9
    :3.1.2.8
    :3.1.2.7
    :3.1.2.6
    :3.1.2.5
    :3.1.2.4
    :3.1.2.3
    :3.1.2.2
    :3.1.2.1
    :3.1.2
    :3.1.0.120
    :3.1.0.119
    :3.1.0.118
    :3.1.0.117
    :3.1.0.116
    :3.1.0.115
    :3.1.0.114
    :3.1.0.113
    :3.1.0.112
    :3.1.0.111
    :3.1.0.110
    :3.1.0.109
    :3.1.0.108
    :3.1.0.107
    :3.1.0.106
    :3.1.0.105
    :3.1.0.104
    :3.1.0.103
    :3.1.0.102
    :3.1.0.101
    :3.1.0.100)
  "A vector of available ASDF versions.")

;;; Classes and types

(defun valid-version-p (keyword)
  "Does this keyword represent a valid version?"
  (if (position keyword +available-versions+) t))

(deftype version ()
  "An ASDF version."
  `(and keyword (satisfies valid-version-p)))

(defclass manager ()
  ((directory :reader manager-directory
              :initarg :directory
              :type pathname
              :documentation "The absolute pathname to the directory where ASDF
              versions are stored."))
  (:documentation "A manager instance."))

;;; Functions and methods

(defun archive-url (version)
  "Return the URL of the archive containing the given ASDF version."
  (declare (type version version))
  (format nil "https://github.com/fare/asdf/archive/~A.zip"
          (symbol-name version)))

(defmethod versions-directory ((manager manager))
  "The absolute pathname to the directory where the manager downloads ASDF
versions."
  (merge-pathnames #p"versions/" (manager-directory manager)))

(defmethod archive-pathname ((manager manager) version)
  "The absolute pathname to the file where the archive of the given version will
be downloaded."
  (declare (type version version))
  (make-pathname :name (substitute #\- #\. (symbol-name version) :test #'char=)
                 :type "zip"
                 :defaults (versions-directory manager)))

(defmethod download ((manager manager) version)
  "Download an ASDF version. Returns the pathname to the archive."
  (declare (type version version))
  (let ((path (archive-pathname manager version)))
    (trivial-download:download (archive-url version) path :quiet t)
    path))

(defmethod extract ((manager manager) version)
  "Extract a previously downloaded archive. Returns the path to the directory."
  (let ((path (archive-pathname manager version)))
    (when (probe-file path)
      (trivial-extract:extract-zip path))
    (merge-pathnames (parse-namestring
                      (format nil "asdf-~A/" version))
                     (versions-directory manager))))

(defmethod download-extract-delete ((manager manager) version)
  "Download an archive for the given ASDF version, extract it, and delete the
archive file. Returns the path to the directory."
  (download manager version)
  (prog1
      (extract manager version)
    (delete-file (archive-pathname manager version))))
