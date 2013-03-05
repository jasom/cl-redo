(in-package :redo)
(defvar *file-info-btree*)

(defmethod open-db ((type (eql :elephant)))
  (unless (iolib/os:directory-exists-p *redo-dbpath*)
    (isys:mkdir *redo-dbpath* #o700))
  (elephant:open-store (list :bdb *redo-dbpath*))
  (setf *file-info-btree* (
  )

(defmethod %db-file-info ((type (eql :elephant)) file)
  (el
