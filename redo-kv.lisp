(in-package :redo)

(defvar *redodb-kv-socket*)

(defmethod open-db ((type (eql :kv)))
  (assert (absolute-file-path-p *redo-dbpath*))
  (unless (iolib/os:directory-exists-p *redo-dbpath*)
    (ensure-file-path-directory-exists *redo-dbpath* #o700))
  (if (iolib/os:environment-variable "REDO_DBSOCK")
      (setf *redodb-kv-socket* 
	    (iolib/sockets:make-socket-from-fd (parse-integer 
						(iolib/os:environment-variable "REDO_DBSOCK"))))
      (progn
	(setf *redodb-kv-socket* 
	      (iolib/sockets:make-socket :address-family :local :connect :active))
	(iolib/sockets:connect *redodb-kv-socket*
			       (iolib/sockets:make-address
				(file-path-namestring
				(merge-file-paths ".kvsock"
						  *redo-dbpath*))))
	(setf (iolib/os:environment-variable "REDO_DBSOCK")
	      (prin1-to-string (iolib/sockets:socket-os-fd *redodb-kv-socket*))))))

(defmethod %db-file-info ((type (eql :kv)) file)
  (error-fmt "dfi: ~S~%" file)
  (with-standard-io-syntax
    (format *redodb-kv-socket*
	    ":GET ~S~%" (file-path-namestring file))
    (finish-output *redodb-kv-socket*)
    (let ((v (read *redodb-kv-socket*)))
      (and
       v
       (read-from-string v)))))


(defmethod (setf %db-file-info) (value (type (eql :kv)) file)
  (error-fmt "sdfi: ~S ~S~%" (file-path-namestring file) value)
  (with-standard-io-syntax
    (format *redodb-kv-socket*
	    ":SET ~S ~S~%" (file-path-namestring file) (prin1-to-string value)))
    (finish-output *redodb-kv-socket*)
  value)

(defmethod %makunbound-db-file-info ((db-type (eql :kv)) file)
  (error-fmt "mudfi: ~S~%" file)
  (with-standard-io-syntax
    (format *redodb-kv-socket*
	    ":DEL ~S~%" (file-path-namestring file))
    (finish-output *redodb-kv-socket*)))
