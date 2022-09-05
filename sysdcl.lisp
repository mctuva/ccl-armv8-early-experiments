;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: CL-USER; Base: 10 -*-

(in-package :CL-USER)

;; We should use ASDF

(defun make-system (system-name &optional subdir)
  (let ((files (get system-name :defsystem-files)))
    (loop for file in files do
      (progn
        (when (consp file) (setq file (car file))) ;ignore dependencies for now
        (setq file (merge-pathnames file (make-pathname :type "lisp"
                                                        :directory (if subdir
                                                                       `(:absolute "Users" "tim" "ccl-dev" ,subdir)
                                                                       '(:absolute "Users" "tim" "ccl-dev"))
                                                        #|
                                                        :directory (if subdir
                                                                       `(:relative "ccl-dev" ,subdir)
                                                                       '(:relative "ccl-dev"))
                                                        :defaults ccl::*user-homedir-pathname*
                                                        |#
                                                        )))
        (print file)
        (load (compile-file file))))))             ;just compile and load files in order

(defmacro defsystem (system-name &rest files)
  `(setf (get ',system-name :defsystem-files) ',files))

(defsystem simulator
  "pkgdcl"
  "simulator-common"
  ("simulator-x86"   :depends-on "simulator-common")
  ("simulator-arm32" :depends-on "simulator-common")
  ("simulator-arm64" :depends-on "simulator-common")
  ("lap-to-as-translator" :depends-on "simulator-common")
  ("lap-tests-x86"   :depends-on "simulator-x86")
  ("lap-tests-arm32" :depends-on "simulator-arm32")
  ("lap-tests-arm64" :depends-on "simulator-arm64")
  )
