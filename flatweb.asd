(in-package :asdf-user)

(defsystem :flatweb
  :description "Simple web framework."
  :version "0.0.1"
  :author "Donatas Petrauskas <donatas.petr@gmail.com>"
  :licence "All right reserved."
  :depends-on (:hunchentoot :cl-ppcre :alexandria)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "path")
                             (:file "core")
                             (:file "flatweb")))))

(defsystem :flatweb/test
  :description "Simple web framework."
  :version "0.0.1"
  :author "Donatas Petrauskas <donatas.petr@gmail.com>"
  :licence "All right reserved."
  :depends-on (:flatweb :should-test :drakma)
  :components ((:module "t" :serial t :components ((:file "flatweb")))))
