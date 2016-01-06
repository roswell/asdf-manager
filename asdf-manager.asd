(defsystem asdf-manager
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/roswell/asdf-manager"
  :bug-tracker "https://github.com/roswell/asdf-manager/issues"
  :source-control (:git "git@github.com:roswell/asdf-manager.git")
  :depends-on (:uiop
               :trivial-download
               :trivial-extract)
  :components ((:module "src"
                :serial t
                :components
                ((:file "asdf-manager"))))
  :description "Download and manage ASDF versions."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op asdf-manager-test))))
