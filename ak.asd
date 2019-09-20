;;;; Copyright (c) Frank James 2019 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :ak
  :name "ak"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "AK Protocol"
  :license "MIT"
  :serial t
  :components
  ((:file "ak"))
  :depends-on (:fsocket :babel :bordeaux-threads :parse-float))
