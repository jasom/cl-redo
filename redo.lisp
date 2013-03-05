;;;; redo.lisp

;; Things supported by apenwar's redo not supported yet:
;; All flags
;; Make Jobserver (obvious since we don't support -j anyway)

(in-package #:redo)
(declaim (optimize (speed 3)))

(defvar *out-of-date-cache* (make-hash-table :test #'equal))

(defvar *redo-current-target* nil)
(defvar *shell-command* "/bin/sh")
(defvar *redo-dbpath*)
(defvar *redo-start-time*)
(defparameter *redo-db-type* :fs)

(defmacro with-temp-fd ((fd-var name-var &optional (template "") ignore-unlink-errors) &body body)
    `(multiple-value-bind (,fd-var ,name-var) (isys:mkstemp ,template)
	 (unwind-protect
	      (progn
		,@body)
	 (isys:close ,fd-var)
	 ,(if ignore-unlink-errors
	     `(ignore-errors
	       (isys:unlink ,name-var))
	     `(isys:unlink ,name-var)))))

#|
(defun run-program (program arguments &key (search t) stdout stderr stdin environment)
  (iolib.os::with-posix-spawn-arguments (attributes file-actions pid)
    (iolib.os::with-argv (argv program arguments)
    (iolib.os::with-c-environment (environment)
      (when stdout
	(iolib.os::posix-spawn-file-actions-adddup2 file-actions stdout iolib.os::+stdout+))
      (when stdin
	(iolib.os::posix-spawn-file-actions-adddup2 file-actions stdin iolib.os::+stdin+))
      (when stderr
	(iolib.os::posix-spawn-file-actions-adddup2 file-actions stderr iolib.os::+stderr+))
      (cffi:with-foreign-string (cfile program)
	(if search
	    (iolib.os::posix-spawnp pid cfile file-actions attributes argv isys:*environ*)
	    (iolib.os::posix-spawn  pid cfile file-actions attributes argv isys:*environ*)))
      (let ((pid (cffi:mem-ref pid 'isys:pid-t)))
	(multiple-value-bind (_ rc)
	(isys:waitpid pid 0)
	  (declare (ignore _))
	  rc))))))
|#


;;; "redo" goes here. Hacks and glory await!

(defparameter *path-to-redo-dir* "/export/jmiller/home/src/lisp/redo/build")

(defun shebang (doname)
  (let*
      ((fd (isys:open (file-path-namestring doname) isys:O-RDONLY))
       (str
	(unwind-protect
	     (cffi:with-foreign-pointer-as-string (p 1024)
	       (isys:read fd p 1023)
	       (setf (cffi:mem-ref p :char 1023) 0))
	  (isys:close fd)))
       (newline
	(position #\Newline str))
       (line
	(and newline
	     (subseq str 0 newline))))
       (when
	   (and line
	   (string= "#!" (subseq line 0 2)))
	 (split-sequence #\Space (subseq line 2) :count 2))))


(defun execute-do-file (doname directory target basename)
  (error-write (format nil "~%execute-do-file: ~{ ~a~}~%" (list doname directory target basename)))
  (setf directory
	(iolib/os:file-exists-p 
	 (make-file-path :components (file-path-directory doname)
			 :defaults doname) :directory))
  (error-write (format nil "Directory: ~A~%" directory))
  (let* ((environment (iolib.os:environment))
	 (target-nsec (and (iolib/os:file-exists-p target)
			   (get-file-nsec target)))
	 (target
	  (merge-file-paths (file-path-file target)
			    (iolib/os:file-exists-p
			     (make-file-path :components (file-path-directory target)
					     :defaults target))))
	 (basename
	  (merge-file-paths (file-path-file basename)
			    (iolib/os:file-exists-p
			     (make-file-path :components (file-path-directory target)
					     :defaults basename)))))
    (error-write (format nil "Target: ~A~%" target))
    (setf (iolib.os:environment-variable "PATH" environment)
	  (format nil "~A:~A"
		  *path-to-redo-dir*
		  (iolib/os:environment-variable "PATH"))
	  (iolib.os:environment-variable "REDO_CURRENT_TARGET" environment)
	  (file-path-namestring target))
    
    (iolib.os:with-current-directory directory
	(let* ((outfilename (format nil "redo.tmp.outfile.~a" (file-path-file target)))
	       (outfilename (merge-file-paths outfilename
					      (make-file-path :components (file-path-directory target)
					      :defaults target)))
	       (stdoutname (format nil "redo.tmp.stdout.~a" (file-path-file target)))
	       (stdoutname (merge-file-paths stdoutname
					      (make-file-path :components (file-path-directory target)
					      :defaults target)))
	       (stdout (isys:open (file-path-namestring stdoutname)
				    (logior isys:O-CREAT isys:O-TRUNC isys:O-WRONLY))))
	  (unwind-protect
	       (let* ((shebang (shebang doname))
		      (cmd
		       (append
			(if shebang
			    shebang
			    (list *shell-command*
				  "-x" "-e" "--"))
			(list
			 (file-path-namestring doname)
			 (file-path-namestring
			  (enough-file-path target directory))
			 (file-path-namestring
			  (enough-file-path basename directory))
			 (file-path-namestring
			  (enough-file-path outfilename directory)))))
		      (return-code 
		       (iolib/os:run-program
			cmd
			:stdin iolib/os:+stdin+
			:stdout stdout
			:stderr iolib/os:+stderr+
			:environment environment)))
		 (error-write (format nil "~A~%" cmd))
		 (cond
		   ;WTF Redo, I mean seriously why can we touch $1 if it's
		   ;a directory, that makes *no* sense
		   ((and (not
			  (eql (iolib/os:file-kind target) :directory))
			 (or
			  (and target-nsec
			       (> (get-file-nsec target)
				  target-nsec))
			  (and
			   (null target-nsec)
			   (iolib/os:file-exists-p target))))
		    (error-write
		     (format nil
			     "Script wrote to $1, which is disallowed! (~A ~A)~%" doname target))
		    nil)
		   ((and
		     (> (isys:stat-size (isys:fstat stdout)) 0)
		     (iolib/os:file-exists-p outfilename))
		    (error-write
		     (format nil
			     "Written to both $3 and stdout not expected, failing! (~A ~A)~%" doname target))
		    (isys:unlink (file-path-namestring outfilename))
		    nil)
		   ((/= return-code 0)
		    (error-write
		     (format nil "Non-zero return! (~A ~A)~%" doname target))
		    nil)
		   ((and
		     (= (isys:stat-size (isys:fstat stdout)) 0)
		     (not (iolib/os:file-exists-p outfilename)))
		    (ignore-errors
		      (isys:unlink (file-path-namestring target)))
		    t)
		   ((> (isys:stat-size (isys:fstat stdout)) 0)
		    (ignore-errors
		      (isys:unlink (file-path-namestring target)))
		    (isys:rename (file-path-namestring stdoutname) (file-path-namestring target))
		    t)
		   ((iolib/os:file-exists-p outfilename)
		    (ignore-errors
		      (isys:unlink (file-path-namestring target)))
		    (isys:rename (file-path-namestring outfilename) (file-path-namestring target))
		    t)))
	    (ignore-errors
	      (isys:close stdout)
	      (isys:unlink (file-path-namestring stdoutname))
	      (isys:unlink (file-path-namestring outfilename))))))))

(defun get-nsec ()
  (multiple-value-bind (sec nsec)
      (isys:clock-gettime isys:clock-realtime)
    (+ (* 1000000000 sec) nsec)))

(defmacro maybe-setf (place value)
    `(or ,place (setf ,place ,value)))

(defgeneric open-db (type))

(defun setup-env ()
  (setf *redo-dbpath*
	(file-path 
	 (maybe-setf
	  (iolib/os:environment-variable "REDO_DBPATH")
	  (format nil "~A/~A" (iolib/os:environment-variable "HOME") ".redodb"))))
  (when
   (iolib/os:file-exists-p (format nil "~A/flush-cache" *redo-dbpath*))
    (iolib/os:makunbound-environment-variable "REDO_START_TIME")
    (iolib/syscalls:unlink (format nil "~A/flush-cache" *redo-dbpath*)))
			   
  (setf *redo-start-time*
	(parse-integer
	 (maybe-setf
	  (iolib/os:environment-variable "REDO_START_TIME")
	  (format nil "~D" (get-nsec)))))
  (open-db *redo-db-type*))

(defun find-do-file (target &aux (target (iolib/pathnames:file-path target)))
  (unless (iolib/pathnames:absolute-file-path-p target)
    (error "Expected absolute path, got \"~A\"." target))
  (let* ((directory (iolib/pathnames:file-path-directory target))
	 (fname (iolib/pathnames:file-path-file target)))
    (loop
	 for dir = directory then (butlast dir)
	 while dir
	 when (find-do-file-in-directory
	       (iolib/pathnames:make-file-path :components dir
					       :defaults target)
	       fname)
	 return it)))

(defun find-do-file-in-directory (directory target)
  (or
   (let ((dopath
	  (iolib/os:file-exists-p
	   (iolib/pathnames:merge-file-paths (format nil "~A.do" target)
					     directory))))
     (when dopath (cons dopath "")))
  (loop
     for i = (cdr (split-sequence #\. target)) then (cdr i)
     while i
     when
       (iolib/os:file-exists-p
	(iolib/pathnames:merge-file-paths (format nil "default~{.~A~}.do" i) directory))
     return (cons (iolib/pathnames:merge-file-paths
		   (format nil "default~{.~A~}.do" i)
		   directory) (format nil "~{.~A~}" i))
     finally
     (return
       (when
	   (iolib/os:file-exists-p
	    (iolib/pathnames:merge-file-paths "default.do" directory))
	    (cons
	     (iolib/pathnames:merge-file-paths "default.do" directory)
	     "")
	    )))))

(defgeneric %db-file-info (db-type file))
(defgeneric (setf %db-file-info) (value db-type file))
(defgeneric %makunbound-db-file-info (db-type file))

(defun makunbound-db-file-info (file)
  (%makunbound-db-file-info *redo-db-type* file))

(defun db-file-info (file &aux (file (file-path file)))
  (%db-file-info *redo-db-type* file))

(defun (setf db-file-info) (value file &aux (file (file-path file)))
  (setf (%db-file-info *redo-db-type* file) value))

(defstruct dbinfo
  (last-run 0 :type integer)
  (stamp nil :type (or null string))
  (success nil :type boolean)
  (null-output nil :type boolean)
  (deps nil :type list))

(defun get-file-nsec (target)
  (* 1000000000
     (isys:stat-mtime (isys:stat (file-path-namestring target)))))

(defun ensure-file-path-directory-exists (path mode &aux (path (file-path path)))
  "Ensures that the directory containing path exists"
  (let ((components (file-path-directory path)))
    (loop
	 with sofar-path = nil
       for i in components
       collect i into sofar
       do (setf sofar-path  (make-file-path :components sofar :defaults path))
       unless (iolib/os:directory-exists-p sofar-path)
	 do (isys:mkdir (file-path-namestring sofar-path) mode))))
    
(defun run-target (target)
  "Finds and builds target; returns dbinfo for target
   Does not build dependencies"
  (setf *out-of-date-cache* (make-hash-table :test #'equal))
  (error-fmt "Run-target: ~A~%" target)
  (ensure-file-path-directory-exists target #o777)
  (let* ((dof (find-do-file target))
	 (dbinfo (or
		  (db-file-info target)
		  (setf (db-file-info target) (make-dbinfo)))))
    (when dof
      (setf (dbinfo-last-run dbinfo) (get-nsec)
	    (dbinfo-deps dbinfo) (list (cons
					(file-path-namestring (car dof))
					(cons :time (get-file-nsec (car dof)))))
	    (db-file-info target) dbinfo))
    (error-fmt "Found Do-File: ~A~%" dof)
    (let*
	((success
	  (and dof
	       (execute-do-file
		(car dof)
		(make-file-path :components (file-path-directory target)
				:defaults target)
		target
		(subseq (file-path-namestring target) 0
			(- (length (file-path-namestring target))
			   (length (cdr dof)))))))
	 (dbinfo
	  (or
	   (db-file-info target)
	   (setf (db-file-info target) (make-dbinfo)))))
      (when dof ;Don't update target at all if we don't find dofile
	(setf
	 (dbinfo-last-run dbinfo) (get-nsec)
	 (dbinfo-success dbinfo) success
	 (dbinfo-null-output dbinfo) (and success (not (iolib/os:file-exists-p target)))
	 (db-file-info target) dbinfo))
      (when (not success) (throw 'exit 1))
      dbinfo)))

(defun run-if-outdated (target &aux (target (if (consp target) target (file-path target))))
  (error-fmt "run-if-outdated: ~A~%" target)
  (cond
    ((and (consp target) (eql (car target) :create))
     (if (iolib/os:file-exists-p (cdr target))
	 (run-if-outdated (cdr target))
	 nil))
    ((string= (file-path-namestring target) "////ALWAYS////")
     (cons :always :always))
    (t
     (let ((dbinfo (db-file-info target)))
       (if (and (not dbinfo)
		(iolib/os:file-exists-p target))
	   (cons :time (get-file-nsec target))
	   (cond
	     ((find-do-file target)
	      (when (out-of-date-p target)
		(setf dbinfo (run-target target)))
	      (if (dbinfo-stamp dbinfo)
		  (cons :hash (dbinfo-stamp dbinfo))
		  (cons :time (dbinfo-last-run dbinfo))))
	     ((iolib/os:file-exists-p target)
	      (makunbound-db-file-info target)
	      (cons :time (get-file-nsec target)))
	   (t (throw 'exit 2))))))))
	   

(defun out-of-date-p (target)
  (multiple-value-bind
	(cache-value cache-hit)
      (gethash (file-path-namestring (file-path target)) *out-of-date-cache*)
    (if cache-hit
	cache-value
	(progn
	  (error-write (format nil "out-of-date-p: ~A~%" target))
	  (let ((r (out-of-date-pi target)))
	    (error-write (format nil "out-of-date-p: ~A => ~A~%" target r))
	    (setf (gethash (file-path-namestring (file-path target)) *out-of-date-cache*)
		  r)
	    r)))))


(defun get-stamp (target &optional (recurse nil))
  (cond
    ((and (consp target) (eql (car target) :create))
     (if (iolib/os:file-exists-p (cdr target))
	 (cons :created :created)
	 (cons :create :create)))
    ((string= (file-path-namestring target) "////ALWAYS////")
     (cons :always :always))
    (t
     (let ((dbinfo (db-file-info target)))
       (cond
	 ((not dbinfo)
	  (if (iolib/os:file-exists-p target)
	      (cons :time (get-file-nsec target))
	      (cons t t)))
	 ((dbinfo-stamp dbinfo)
	  (if recurse
	      (cons :hash (dbinfo-stamp dbinfo))
	      (progn
		(run-if-outdated target) ; Need to try rebuild to get redo-stamp
		(get-stamp target t))))   ; No guarantee it will still be a hash target
	 ((out-of-date-p target) (cons t t)) ; target is out-of-date and not stamped
	 (t (cons :time (dbinfo-last-run dbinfo))))))))

(defun out-of-date-pi (target)
  (let* ((dbinfo
	  (and
	   (not (consp target))
	   (db-file-info target))))
  (error-write (format nil "~%out-of-date-p ~a ~%" target))
  (cond
    ((and (consp target) (eql (car target) :create))
     (error-fmt "ifcreate target; will check if it exists~%")
     (iolib/os:file-exists-p (cdr target)))
    ((not dbinfo)
     (error-fmt "No DBINFO will rebuild if it dosn't exist~%")
     (not (iolib/os:file-exists-p target)))
    ((and
      (iolib/os:file-exists-p target)
      (or
       (> (get-file-nsec target)
	  (dbinfo-last-run dbinfo))
       (not (dbinfo-success dbinfo))))
     (error-write "File modified since last run, will not overwrite")
     nil)
    ;((> (dbinfo-last-run dbinfo) *redo-start-time*)
     ;(error-write "Have already started this, won't run"  )
     ;nil)
    ((and (not (iolib/os:file-exists-p target))
	  (not (dbinfo-null-output dbinfo)))
     (error-write "File doesn't exist, must be out of date")
     t)
    ((not (dbinfo-success dbinfo))
     (error-write "Never succeeded")
     t) ;Never succeeded?
    
    (t
     (error-write "Checking deps")
     (loop for (target old-type . old-stamp) in (dbinfo-deps dbinfo)
	for (new-type . new-stamp) in
	  (mapcar (compose #'get-stamp #'car)  (dbinfo-deps dbinfo))
	when
	  (or
	   (not (eql old-type new-type))
	   (eql old-type :always)
	   (and (eql old-type :time)
		(< old-stamp new-stamp))
	   (and (eql old-type :hash)
		(string/= old-stamp new-stamp)))
	return t
	finally (return nil))))))

(defun main (args)
  (error-fmt "ARGS: ~A~%~%" args)
  (let*
      ((exec-name (car args))
       (exec-file (file-path exec-name))
       (command-name (file-path-file exec-file))
       (command (make-keyword (string-upcase command-name))))
    (case command
      (:redo (redo-main args))
      (:redo-ifchange (redo-ifchange-main args))
      (:redo-stamp (redo-stamp-main args))
      (:redo-always (redo-always-main args))
      (:redo-ifcreate (redo-ifcreate-main args)))))

(defun redo-main (args)
  (isys:exit 
  (catch 'exit
    ;Nested invocations of redo establish a new environment for building
    ;Or do they?
    ;(iolib/os::makunbound-environment-variable "REDO_START_TIME")
    (let* ((targets
	    (if (and
		 (< (length args) 2)
		 (not (iolib/os::environment-variable "REDO_START_TIME")))
		(list "all")
		(cdr args)))
	   (targets
	    (mapcar (rcurry #'merge-file-paths (iolib/os:current-directory)) targets)))
      (setup-env)
      (error-write (format nil "Redo:~{ ~A~}~%" targets))
      ;(mapc #'out-of-date-p targets) ; will build deps
      (mapc #'run-target targets))
    0)))

(defun redo-stamp-main (args)
  (declare (ignore args))
  (setup-env)
  (isys:exit
   (catch 'exit
     (multiple-value-bind
	   (code out err)
	 (iolib/os:run-program '("md5sum") :stdin iolib/os:+stdin+
			       :stderr iolib/os:+stderr+)
       (declare (ignore err))
       (if (or (/= code 0)
	       (= (length out) 0))
	   (throw 'exit 1)
	   (let* ((redo-current-target (iolib/os:environment-variable "REDO_CURRENT_TARGET")))
	     (when (iolib/os:environment-variable "REDO_CURRENT_TARGET")
	       (let*
		   ((target-dbinfo (db-file-info redo-current-target)))
		    (setf (dbinfo-stamp target-dbinfo) out
			  (db-file-info redo-current-target) target-dbinfo))))))
     0)))

(defun redo-always-main (args)
  (isys:exit 
  (catch 'exit
    (setup-env)
    (unless (= (length args) 1)
      (throw 'exit 3))
    (let* ((redo-current-target (iolib/os:environment-variable "REDO_CURRENT_TARGET")))
      (when (iolib/os:environment-variable "REDO_CURRENT_TARGET")
	(let*
	    ((target-dbinfo (db-file-info redo-current-target)))
	  (pushnew '("////ALWAYS////" :always . :always)
			 (dbinfo-deps target-dbinfo) :test #'equal)

	  (setf (db-file-info redo-current-target) target-dbinfo))))
    0)))

(defun redo-ifcreate-main (args)
  (isys:exit 
  (catch 'exit
    (setup-env)
    (let* ((targets (cdr args))
	   (redo-current-target (iolib/os:environment-variable "REDO_CURRENT_TARGET"))
	   (targets
	    (mapcar (rcurry #'merge-file-paths (iolib/os:current-directory)) targets)))
      (when (some #'iolib/os:file-exists-p targets)
	(throw 'exit 1))
      (when (iolib/os:environment-variable "REDO_CURRENT_TARGET")
	(let*
	    ((target-dbinfo (db-file-info redo-current-target))
	     (old-deps (dbinfo-deps target-dbinfo)))
	  (loop
	     for target in targets
	     for depcons =
	       (assoc (file-path-namestring target) old-deps
		      :test #'string=
		      :key (lambda (x) (if (consp x) (cdr x) x)))
	     when depcons do (setf (cdr depcons) '(:create . :create))
	     else do (push (cons (cons :create (file-path-namestring target)) '(:create . :create)) old-deps))
	  (setf (dbinfo-deps target-dbinfo) old-deps
		(db-file-info redo-current-target) target-dbinfo))))
    0)))
  

(defun redo-ifchange-main (args)
  (isys:exit 
  (catch 'exit
    (setup-env)
    (let* ((targets (cdr args))
	   (targets
	    (mapcar (rcurry #'merge-file-paths (iolib/os:current-directory)) targets)))
      (error-write (format nil "~%ifchange-main: ~a~%" args))
      ;update parent dependencies
      (let* ((redo-current-target (iolib/os:environment-variable "REDO_CURRENT_TARGET"))
	     (new-deps (mapcar #'run-if-outdated targets)))
	(error-fmt "new-deps: ~A~%" new-deps)
	(when (iolib/os:environment-variable "REDO_CURRENT_TARGET")
	  (let*
	      ((target-dbinfo (db-file-info redo-current-target))
	       (old-deps (dbinfo-deps target-dbinfo)))
	    (loop for dep in new-deps
	       for target in targets
	       for depcons =
		 (assoc (file-path-namestring target) old-deps
			:test #'equal)
	       when depcons do (setf (cdr depcons) dep)
	       else do (push (cons (file-path-namestring target) dep) old-deps))
	    (setf (dbinfo-deps target-dbinfo) old-deps
		  (db-file-info redo-current-target) target-dbinfo)))))
    0)))

(defun error-fmt (str &rest r)
  (error-write (apply #'format nil str r)))

;(defun error-write (str)
  ;(cffi:with-foreign-pointer-as-string (s (1+ (length str)))
  ;(isys:write 2 (cffi:lisp-string-to-foreign str s (1+ (length str))) (length str))))

(defun error-write (str) (declare (ignore str)) nil)
;(trace db-file-info)
