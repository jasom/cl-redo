;;;; redo.lisp

;; Things supported by apenwar's redo not supported yet:
;; All flags
;; Make Jobserver (obvious since we don't support -j anyway)

(in-package #:redo)
(declaim (optimize (space 3)))

(defvar *out-of-date-cache* (make-hash-table :test #'equal))

(defvar *redo-current-target* nil)
(defvar *shell-command* "/bin/sh")
(defvar *redo-dbpath*)
(defvar *redo-start-time*)
(defparameter *redo-db-type* :fs)
(defvar *path-to-redo-dir*)

(declaim (ftype (function (t) string) file-path-namestring))

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


;;; "redo" goes here. Hacks and glory await!


(defun shebang (doname)
  (let*
      ((fd (isys:open (file-path-namestring doname) isys:O-RDONLY))
       (str
	(unwind-protect
	     (cffi:with-foreign-pointer-as-string (p 1024)
	       (let ((nbytes (isys:read fd p 1023)))
		 (assert (>= nbytes 0))
		 (setf (cffi:mem-ref p :char nbytes) 0)))
	  (isys:close fd)))
       (newline
	(position #\Newline str))
       (line
	(and newline
	     (subseq str 0 newline))))

    (declare (type string str)
	     (type (or null string) line))
       (when
	   (and line
	   (string= "#!" (subseq line 0 2))) ;
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
		 (declare (type fixnum return-code))
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
		     (>
		      (the integer (isys:stat-size (isys:fstat stdout))) 0)
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

(defun fix-file-path (file)
  "Finds canonical path for file;

The file's directory must at least exist"
  (let
      ((directory
	(iolib/os:directory-exists-p 
	 (make-file-path
	  :components (file-path-directory file)
	  :defaults file))))
    (when directory
      (merge-file-paths
       (file-path-file file)
       directory))))
       

(defgeneric %db-file-info (db-type file))
(defgeneric (setf %db-file-info) (value db-type file))
(defgeneric %makunbound-db-file-info (db-type file))

(defun makunbound-db-file-info (file)
  (%makunbound-db-file-info *redo-db-type* file))

(defun db-file-info (file &aux (file (file-path file)))
  (%db-file-info *redo-db-type* (file-path file)))

(defun (setf db-file-info) (value file &aux (file (file-path file)))
  (setf (%db-file-info *redo-db-type* file) value))

(defstruct dbinfo
  (last-run 0 :type integer)
  (stamp nil :type (or null string))
  (success nil :type boolean)
  (null-output nil :type boolean)
  (deps nil :type list))

(defun get-file-nsec (target)
  (coerce
   (* 1000000000
      (isys:stat-mtime (isys:stat (file-path-namestring target))))
   'integer))

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
    (declare (type (or null (cons file-path string)) dof))
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
			   (length (the string (cdr dof))))))))
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
     (loop
	for (target old-type . old-stamp) in (dbinfo-deps dbinfo)
	  do
	  (cond
	    ((and (consp target)
		  (eql (car target) :create))
	     (when
		 (iolib/os:file-exists-p (cdr target))
	       (return t)))
	    ((eql old-type :always)
	       (return t))
	    ((and (out-of-date-p target) (not (eql old-type :hash)))
	     (return t))
	    ((and (eql old-type :hash)
		  (not (equal (get-stamp target)
			      (cons old-type old-stamp))))
	     (return t))
	    ((and (eql old-type :time)
		  (and (iolib/os:file-exists-p target)
		      (< old-stamp (get-file-nsec target))))
	     (return t))
	    (t nil)))
	  
     #|
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
	finally (return nil))
     |#
     ))))

(defun main (args)
  (error-fmt "ARGS: ~A~%~%" args)
  		 
  (let*
      ((exec-name (car args))
       (exec-file (file-path exec-name))
       (command-name (file-path-file exec-file))
       (command (make-keyword (string-upcase command-name))))
    (setf *path-to-redo-dir*
	(iolib/os:directory-exists-p
	 (make-file-path :components (file-path-directory exec-name)
			 :defaults exec-name)))
    (isys:exit
    (case command
      (:redo (redo-main args))
      (:redo-ifchange (redo-ifchange-main args))
      (:redo-stamp (redo-stamp-main args))
      (:redo-stamp2 (redo-stamp2-main args))
      (:redo-always (redo-always-main args))
      (:redo-ifcreate (redo-ifcreate-main args))))))

(defun read-nul-string (stream)
  (with-output-to-string (string)
    (loop
       for char = (read-char stream)
       while (char/= #\Nul char)
       do (write-char char string))))

(defun read-nul-string-list (stream)
  (loop
       for string = (read-nul-string stream)
       while (string/= "" string)
       collect string))

(defun split-env (string)
  (declare (type string string))
  (let
      ((pos (position #\= string)))
    (cons (subseq string 0 pos)
	  (subseq string (1+ pos)))))

(cffi:defcallback child-handler :void ((v :int))
  (declare (ignore v)))

(defun nochldwait ()
  (cffi:with-foreign-object (action 'isys:sigaction)
    (setf
     (cffi:foreign-slot-value action 'isys:sigaction 'isys:handler)
     (cffi:get-callback 'child-handler)
     (cffi:foreign-slot-value action 'isys:sigaction 'isys:sigaction)
     (cffi:null-pointer)
     (cffi:foreign-slot-value action 'isys:sigaction 'isys::mask) 0
     (cffi:foreign-slot-value action 'isys:sigaction 'isys::flags)
     isys:SA-NOCLDWAIT)
    (iolib/syscalls:sigaction isys:SIGCHLD action (cffi:null-pointer))))

(defun yeschldwait ()
  (cffi:with-foreign-object (action 'isys:sigaction)
    (setf
     (cffi:foreign-slot-value action 'isys:sigaction 'isys:handler)
     (cffi:get-callback 'child-handler)
     (cffi:foreign-slot-value action 'isys:sigaction 'isys:sigaction)
     (cffi:null-pointer)
     (cffi:foreign-slot-value action 'isys:sigaction 'isys::mask) 0
     (cffi:foreign-slot-value action 'isys:sigaction 'isys::flags)
     0)
    (iolib/syscalls:sigaction isys:SIGCHLD action (cffi:null-pointer))))
(defun server-main (args)
  (setup-env)
  
  (multiple-value-bind (server-control client-control)
      (iolib/sockets:make-socket-pair)
    (setf (iolib/os:environment-variable "REDO_SERVER_SOCKFD")
	  (format nil "~D"
		  (iolib/sockets:socket-os-fd client-control))
	  (iolib/os:environment-variable "PATH")
	  (format nil "~A:~A"
		  *path-to-redo-dir*
		  (iolib/os:environment-variable "PATH")))
    (let ((pid (iolib/syscalls:fork)))
      (when (= pid 0)
	(nochldwait)
	(loop
	   (let* ((args (read-nul-string-list server-control))
		  (pid (parse-integer (read-nul-string server-control)))
		  (cwd (read-nul-string server-control))
		  (env-vars (read-nul-string-list server-control)))
	     (when (= (length args) 0) (isys:exit 0))
	     (iolib/os:clear-environment)
	     (loop for
		  (var . val) in (mapcar #'split-env env-vars)
		do (setf (iolib/os:environment-variable var) val))
	     (when (= 0 (isys:fork))
	       (yeschldwait)
		 (iolib/os:with-current-directory cwd
		   (let ((rv (main args)))
		     (error-fmt "Client main done: ~D~%" rv)
		     (isys:kill pid
				(if (= 0 rv) isys:SIGUSR1 isys:SIGUSR2)))
		   (isys:exit 0))))))
      (let ((rv (main args)))
	(error-fmt "Finished main, exiting~%")
	(isys:kill pid isys:SIGKILL)
	;(write-sequence (make-string 40 :initial-element #\Nul) client-control)
	;(finish-output server-control)
	;(isys:waitpid pid 0)
	(isys:exit rv)))))

(defun redo-main (args)
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
    0))

(defun redo-stamp2-main (args)
  (setup-env)
  (catch 'exit
    (let ((stamp (second args)))
      (let* ((redo-current-target (iolib/os:environment-variable "REDO_CURRENT_TARGET")))
	(when (iolib/os:environment-variable "REDO_CURRENT_TARGET")
	  (let*
	      ((target-dbinfo (db-file-info redo-current-target)))
	    (setf (dbinfo-stamp target-dbinfo) stamp
		  (db-file-info redo-current-target) target-dbinfo)))))
    0))

(defun redo-stamp-main (args)
  (declare (ignore args))
  (setup-env)
   (catch 'exit
     (multiple-value-bind
	   (code out err)
	 (iolib/os:run-program '("md5sum") :stdin iolib/os:+stdin+
			       :stderr iolib/os:+stderr+)
       (declare (ignore err)
		(type fixnum code)
		(type string out))
       (if (or (/= code 0)
	       (= (length out) 0))
	   (throw 'exit 1)
	   (let* ((redo-current-target (iolib/os:environment-variable "REDO_CURRENT_TARGET")))
	     (when (iolib/os:environment-variable "REDO_CURRENT_TARGET")
	       (let*
		   ((target-dbinfo (db-file-info redo-current-target)))
		    (setf (dbinfo-stamp target-dbinfo) out
			  (db-file-info redo-current-target) target-dbinfo))))))
     0))

(defun redo-always-main (args)
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
    0))

(defun redo-ifcreate-main (args)
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
    0))
  

(defun redo-ifchange-main (args)
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
    0))

(defun error-fmt (str &rest r)
  (error-write (apply #'format nil str r)))

;(defun error-write (str)
;  (let ((buf (babel:string-to-octets str)))
;  ;(cffi:with-foreign-pointer-as-string (s (1+ (length str)))
;    (cffi:with-pointer-to-vector-data (ptr buf)
;  (isys:write 2 ptr (length buf)))))

(defun error-write (str) (declare (ignore str)) nil)
