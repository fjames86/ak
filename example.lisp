;;;; Copyright (c) Frank James 2019 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:ak/example
  (:use #:cl))

(in-package #:ak/example)

;; very simple device. just stores a list of 16 channels and lists them
;; individually with the AOBK command or all with AKON

(defclass simple-device (ak:device)
  ((channels :initform (make-list 16 :initial-element 0.0)
	     :reader simple-channels)))
   
(ak:defcommand :aobk ((d simple-device) channel args)
  (cond
    ((= channel 0)
     (simple-channels d))
    (t
     (nth channel (simple-channels d)))))

(ak:defcommand :akon ((d simple-device) channel args)
  (simple-channels d))
