(in-package :redo)

(defmethod open-db ((type (eql :fs)))
  (unless (iolib/os:directory-exists-p *redo-dbpath*)
    (ensure-file-path-directory-exists *redo-dbpath* #o700))
  (assert (absolute-file-path-p *redo-dbpath*)))

(defmethod %db-file-info ((type (eql :fs)) file)
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
	(with-open-file (f (file-path-namestring dbfile))
	  (read f))))))


(defmethod (setf %db-file-info) (value (type (eql :fs)) file)
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
      (with-open-file (f (file-path-namestring dbfile) :direction :output :if-exists :supersede)
	(prin1 value f)))))

(defmethod %makunbound-db-file-info ((db-type (eql :fs)) file)
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
