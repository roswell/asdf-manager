(defsystem asdf-manager-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:asdf-manager
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "asdf-manager")))))
