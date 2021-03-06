(in-package :redo)

(defparameter *fsdb-cache* (make-hash-table :test #'equal))

(defmethod open-db ((type (eql :fs)))
  (unless (iolib/os:directory-exists-p *redo-dbpath*)
    (ensure-file-path-directory-exists *redo-dbpath* #o700))
  (assert (absolute-file-path-p *redo-dbpath*)))

(defmethod %db-file-info ((type (eql :fs)) file)
  (multiple-value-bind (v f) (gethash (file-path-namestring file) *fsdb-cache*)
    (if f v
	(let ((file (file-path file)))
	  (unless (iolib/pathnames:absolute-file-path-p file)
	    (error "Expected absolute path, got \"~A\"." file))
	  (let* ((components
		  (cdr (iolib/pathnames:file-path-components file)))
		 (dbfile
		  (make-file-path :components components :defaults *redo-dbpath*))
		 (dbfile
		  (merge-file-paths dbfile *redo-dbpath*)))
	    (when (iolib/os:file-exists-p dbfile)
	      (read-from-string
	       (let ((size (1+ (isys:stat-size
				(isys:stat (file-path-namestring dbfile))))))
		 (cffi:with-foreign-pointer-as-string (buf size)
		   (let ((fd (isys:open (file-path-namestring dbfile) isys:O-RDONLY)))
		     (assert (>= fd 0))
		     (unwind-protect
			  (let ((bytes-read (isys:read fd buf (1- size))))
			    (assert (= bytes-read (1- size))))
		       (isys:close fd)))
		   (setf (cffi:mem-aref buf :char (1- size)) 0))))))))))

(defmethod (setf %db-file-info) (value (type (eql :fs)) file)
  ;(setf (gethash (file-path-namestring file) *fsdb-cache*) value)
  (let ((file (file-path file)))
    (unless (iolib/pathnames:absolute-file-path-p file)
      (error "Expected absolute path, got \"~A\"." file))
    (let* ((components
	    (cdr (iolib/pathnames:file-path-components file)))
	   (dbfile
	    (make-file-path :components components :defaults *redo-dbpath*))
	   (dbfile
	    (merge-file-paths dbfile *redo-dbpath*)))
      (ensure-file-path-directory-exists dbfile #o755)
      ;TODO fix for paths that aren't valid lisp paths
      (let
	  ((octets (babel:string-to-octets (prin1-to-string value))))
	(cffi:with-pointer-to-vector-data (buf octets)
	  (let ((fd (isys:open (file-path-namestring dbfile)
			       (logior isys:O-TRUNC isys:O-CREAT isys:O-WRONLY))))
	    (assert (>= fd 0))
	    (unwind-protect
		 (isys:write fd buf (length octets))
	      (isys:close fd)))))))
  value)
      

(defmethod %makunbound-db-file-info ((db-type (eql :fs)) file)
  (remhash (file-path-namestring file) *fsdb-cache*)
  (let ((file (file-path file)))
    (unless (iolib/pathnames:absolute-file-path-p file)
      (error "Expected absolute path, got \"~A\"." file))
    (let* ((components
	    (cdr (iolib/pathnames:file-path-components file)))
	   (dbfile
	    (make-file-path :components components :defaults *redo-dbpath*))
	   (dbfile
	    (merge-file-paths dbfile *redo-dbpath*)))
      (when
	  (iolib/os:file-exists-p dbfile)
	(isys:unlink (file-path-namestring dbfile))))))
