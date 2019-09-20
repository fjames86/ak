;;;; Copyright (c) Frank James 2019 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:ak
  (:use #:cl)
  (:export #:connect
	   #:disconnect
	   #:call
	   #:device
	   #:device-status
	   #:device-mode
	   #:device-version
	   #:device-alarm
	   #:defcommand
	   #:start-device
	   #:stop-device
	   #:ak-error))
	   
	  
(in-package #:ak)


;; AK protocol is asymmetric. AK host works as a server that answers to clients. It accepts command telegrams and answers with acknowledge telegrams. For each command telegram AK host answers with exactly one acknowledge telegram. It does not send anything to the client without a request. AK telegrams have the following format:

;; AK command telegram format
;; Byte	Contains	Description
;; 1	STX (ASCII 02 )	This byte indicates start of a new telegram
;; 2	Ignored	This byte is ignored, but it must be a printable character
;; 3..6	Function code	The function code is a four byte identifier. It must contain only printable characters
;; 7	SP (space, ASCII 32)	Separator
;; 8..9	Channel number	The channel number. Usually this field contains Kn, where n is the number of the channel in ASCII format. It may contain more than one digit. If only one channel is supported it is K0 (ASCII 75,48). Note that some AK devices do not support the channel number, though it is not AK protocol conform.
;; 10	SP (space, ASCII 32)	Separator. It can be omitted if no data field present
;; 11..n	Data field	It is AK host specific. It can be any sequence of printable characters. The data field may be omitted
;; n+1	ETX (ASCII 03)	End byte. After the end byte CR (ASCII 13) may follow
 

;; AK acknowledge telegram format
;; Byte	Contains	Description
;; 1	STX (ASCII 02 )	This byte indicates start of a new telegram
;; 2	Ignored	This byte is ignored, but it must be a printable character
;; 3..6	Function code	The function code (echoed from the command telegram)
;; 7	SP (space, ASCII 32)	Separator
;; 8	General error code	The error code is a digit 0..9 (in ASCII format). Zero (ASCII 48) indicates no error
;; 9	SP (space, ASCII 32)	Separator. It can be omitted if no data field present
;; 10..n	Data field	It is AK host specific. It can be any sequence of printable characters. The data field may be empty
;; n+1	ETX (ASCII 03)	End byte. After the end byte CR (ASCII 13) may follow.

(defun command-string (fncode channel &rest data)
  (with-output-to-string (s)
    (format s "~C ~A K~D" #\stx fncode channel)
    (do ((d data (cdr d)))
	((null d))
      (format s " ~A" (car d)))
    (format s "~C" #\etx)))

(defun reply-string (fncode error-code &rest data)
  (with-output-to-string (s)
    (format s "~C ~A ~D" #\stx fncode error-code)
    (do ((d data (cdr d)))
	((null d))
      (format s " ~A" (car d)))
    (format s "~C" #\etx)))

(defvar *conn* nil)
(defun disconnect ()
  "Disconnect from device"
  (when *conn*
    (fsocket:close-socket *conn*)
    (setf *conn* nil)))

(defun connect (port &optional addr)
  "Establish connection to a device." 
  (disconnect)
  (setf *conn* (fsocket:open-socket :type :stream))
  (handler-bind ((error (lambda (e)
			  (declare (ignore e))
			  (disconnect))))
    (fsocket:socket-connect *conn* (fsocket:sockaddr-in (or addr #(127 0 0 1)) port))))

(defun read-command (fd buf start)
  "Read a command string i.e. read until ETX character" 
  (do ((offset start)
       (done nil))
      (done offset)
    (let ((n (fsocket:socket-recv fd buf :start offset)))
      (when (= n 0) (error "Graceful close"))
      (dotimes (i n)
	(when (= (aref buf (+ start i)) (char-code #\etx))
	  (setf done t)))
      (incf offset n))
    (when (>= offset (length buf))
      (error "Out of buffer space"))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun parse-command (buf start)
  ;; move to start character
  (let ((offset start)
	(fncode nil)
	(channel nil)
	(args nil))
    (while (not (= (aref buf offset) (char-code #\stx)))
      (incf offset))
    (unless (= (aref buf offset) (char-code #\stx))
      (error "Unexpected character ~C != STX" (aref buf offset)))
    (incf offset 2)
    (setf fncode (babel:octets-to-string buf :start offset :end (+ offset 4))
	  offset (+ offset 4))
    (incf offset) ;; space
    (incf offset) ;; K
    (setf channel (- (aref buf offset) (char-code #\0)))
    (incf offset)
    (while (not (= (aref buf offset) (char-code #\etx)))
      (do ((off (1+ offset) (1+ off)))
	  ((or (= (aref buf off) (char-code #\space))
	       (= (aref buf off) (char-code #\etx)))
	   (progn
	     (push (babel:octets-to-string (subseq buf (1+ offset) off)) args)
	     (setf offset off)))))
    (values fncode channel (nreverse args) offset)))

(defun parse-reply (buf start)
  ;; move to start character
  (let ((offset start)
	(fncode nil)
	(error-code nil)
	(args nil))
    (while (not (= (aref buf offset) (char-code #\stx)))
      (incf offset))
    (unless (= (aref buf offset) (char-code #\stx))
      (error "Unexpected character ~C != STX" (aref buf offset)))
    (incf offset 2)
    (setf fncode (babel:octets-to-string buf :start offset :end (+ offset 4))
	  offset (+ offset 4))
    (incf offset) ;; space
    (setf error-code (- (aref buf offset) (char-code #\0)))
    (incf offset)
    (while (not (= (aref buf offset) (char-code #\etx)))
      (do ((off (1+ offset) (1+ off)))
	  ((or (= (aref buf off) (char-code #\space))
	       (= (aref buf off) (char-code #\etx)))
	   (progn
	     (push (babel:octets-to-string (subseq buf (1+ offset) off)) args)
	     (setf offset off)))))
    (values fncode error-code args offset)))

(defun send-all (fd buf)
  (do ((offset 0))
      ((>= offset (length buf)))
    (let ((n (fsocket:socket-send fd buf :start offset)))
      (incf offset n))))

(define-condition ak-error (error)
  ((code :reader ak-error-code))
  (:report (lambda (c stream) (format stream "AK Error Status ~A" (ak-error-code c)))))

(defun call (fncode channel &rest args)
  (unless *conn* (error "Not connected"))
  (let ((cmd (babel:string-to-octets (apply #'command-string fncode channel args))))
    (send-all *conn* cmd))
  (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
    (read-command *conn* buf 0)      
    (multiple-value-bind (fncode error-code args offset) (parse-reply buf 0)
      (declare (ignore fncode offset))
      (unless (= error-code 0) (error 'ak-error :error-code error-code))
      (apply #'values args))))

;; ---------- server -------------

(defclass device ()
  ((port :initform 0 :initarg :port :accessor device-port)
   (thread :initform nil :accessor device-thread)
   (stop :initform nil :accessor device-stop)
   (mode :initform :sman :accessor device-mode)
   (status :initform :stby :accessor device-status)
   (version :initform "1.0.0" :initarg :version :accessor device-version)
   (alarm :initform 0 :accessor device-alarm)))
(defmethod print-object ((d device) stream)
  (print-unreadable-object (d stream :type t)
    (format stream ":PORT ~D :RUNNING ~A" (device-port d) (not (null (device-thread d))))))

(defgeneric process-command (device fncode channel args))

(defmacro defcommand (fncode ((device devicetype) channel args) &body body)
  "Define device command handler. 
FNCODE ::= 4 character keyword e.g. :ASTZ 
DEVICETYPE ::= device class
CHANNEL ::= variable receives integer channel number passed by client 
ARGS ::= receives a list of strings for each argument
Return a list of reply data or signal an AK-ERROR condition.
" 
  (let ((gfncode (gensym)))
    `(defmethod process-command ((,device ,devicetype) (,gfncode (eql ,fncode)) ,channel ,args)
       ,@body)))

;; common commands most devices support 
(defcommand :srem ((d device) channel args)
  (setf (device-mode d) :srem)
  nil)
(defcommand :sman ((d device) channel args)
  (setf (device-mode d) :sman)
  nil)
(defcommand :spau ((d device) channel args)
  (setf (device-status d) :spau)
  nil)
(defcommand :stby ((d device) channel args)
  (setf (device-status d) :stby)
  nil)
(defcommand :aver ((d device) channel args)
  (device-version d))
(defcommand :astz ((d device) channel args)
  (list (device-mode d) (device-status d)))
(defcommand :astf ((d device) channel args)
  (device-alarm d))
(defcommand :sres ((d device) channel args)
  nil)
(defcommand :snga ((d device) channel args)
  (setf (device-status d) :snga)
  nil)

(defun handle-command (buf offset device)
  (multiple-value-bind (fncode channel args end) (parse-command buf offset)
    #+nil(format t "-> ~A ~A ~A~%" fncode channel args)
    (let ((rstr (handler-case
		    (let ((rdata (process-command device (intern fncode :keyword) channel args)))
		      (apply #'reply-string fncode 0 (if (listp rdata) rdata (list rdata))))
		  (ak-error (e)
		    (format t "AK-ERROR: FNCODE=~A STATUS=~A~%" fncode (ak-error-code e))
		    (reply-string fncode (ak-error-code e)))
		  (error (e)
		    (format t "ERROR: FNCODE=~A ~S~%" fncode e)
		    (reply-string "????" 0)))))
      #+nil(format t "<- ~A~%" rstr)
      (values (babel:string-to-octets rstr) end))))

;; ------------- networking ----------------

(defun process-connection (fd device)
  (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
    (do ((offset 0)
	 (done nil))
	((or done (device-stop device)))
      (let ((n (read-command fd buf offset)))
	(when (= n 0) (return-from process-connection))
	(multiple-value-bind (rbuf end) (handle-command buf offset device)
	  (send-all fd rbuf)
	  (dotimes (i (- end n))
	    (setf (aref buf i) (aref buf (+ end i))
		  offset (- end n)))))))
  nil)

(defun run-device-server (device)
  (fsocket:with-tcp-socket (fd (device-port device))
    (setf (device-port device) (fsocket:sockaddr-in-port (fsocket:socket-name fd)))
    (let ((cfd (fsocket:socket-accept fd)))
      #+nil(format t "ACCEPT~%")
      (handler-case (process-connection cfd device)
	(error (e)
	  (format t "ERROR: ~A~%" e)
	  nil))
      #+nil(format t "CLOSE~%")
      (fsocket:close-socket cfd))))

(defun start-device (device)
  (setf (device-stop device) nil
	(device-thread device)
	(bt:make-thread (lambda () (run-device-server device))
			:name (format nil "AK Server ~A" (type-of device))))
  device)
 
(defun stop-device (device)
  (setf (device-stop device) t)
  (ignore-errors
    (connect (device-port device))
    (disconnect))
  (bt:join-thread (device-thread device))
  (setf (device-thread device) nil)
  device)

