(in-package :cl-user)
(defpackage asdf-manager-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :asdf-manager-test)

(def-suite tests
  :description "asdf-manager tests.")
(in-suite tests)

(defparameter +dir+
  (asdf:system-relative-pathname :asdf-manager
                                 #p"t/manager/"))

(test download
  (let ((manager (make-instance 'asdf-manager:manager
                                :directory +dir+))
        (version :3.1.6.6))
    (is
     (pathnamep (asdf-manager:versions-directory manager)))
    (let ((path (asdf-manager:archive-pathname manager version)))
      (is-false
       (probe-file path))
      (is
       (equal (asdf-manager:download manager :3.1.6.6)
              path))
      (is-true
       (probe-file path))))
  (uiop:delete-directory-tree +dir+ :validate t))

(test versions
  (let ((manager (make-instance 'asdf-manager:manager
                                :directory +dir+)))
    (loop for version across asdf-manager:+available-versions+ do
      (let ((dir))
        (finishes
         (setf dir (asdf-manager:download-extract-delete manager version)))
        (is-true (probe-file dir)))))
  (uiop:delete-directory-tree +dir+ :validate t))

(defun run-tests ()
  (run! 'tests))
