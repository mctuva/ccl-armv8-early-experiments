;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SCOM; Base: 10 -*-

(in-package :SCOM)

;;;;; Version history:
;;; w0718 - Ayla - Expt and cons are working, we will have to fix up the architecture specific parts of def-simulator-code when we get arm underway 
;;; w0722 - Tim - fix macroexpand-lap-tree to understand Lisp LET
;;; w0815 - Ayla - changed def-asm-lisp-fun and def-asm-fun to also translate to assembly


;;; memory and register special variables

(defparameter *simulated-memory-size* #x20000)
(defvar *simulated-memory* (make-array *simulated-memory-size*))  ;needed for the stack, and for symbols, and for function objects, T, NIL
(defparameter *simulated-memory-pointer* 0)

(defvar *registers* (make-hash-table))
(defvar *register-names*) ;; bound on a per-architecture basis by the macro-expander

(defvar flags 0) ;flags register

(defvar *this-function*)

;;; for debugging

(defun show-registers (&optional label)
  "Prints the values in each register"
  (when label (print label))
  (let ((*print-length* 3) (*print-level* 2) (*print-pretty* nil))
    (loop for k being the hash-keys in *registers* using (hash-value v)
          do (if (integerp v)
                 (format t "~a~10t=> 0x~x~%" k v)   ;now in hex
                 (format t "~a~10t=> ~a~%" k v)))))

(defun show-memory (&optional w2b-p)
  "Prints the initialized slots of simulated memory. Optional parameter to set memory slot size to word rather than quad."
  (let ((*print-length* 3) (*print-level* 2) (*print-pretty* nil))
    (loop for i from 0 and x across *simulated-memory*
          do (unless (equal x :uninit)
               (if w2b-p
                   (format t "0x~x: ~a~%" (%w2b i) x)       ;[new] address in hex now
                   (format t "0x~x: ~a~%" (%q2b i) x))))))

#+AYLAS
(defun show-memory (&optional w2b-p)
  "Prints the initialized slots of simulated memory. Optional parameter to set memory slot size to word rather than quad."
  (let ((*print-length* 3) (*print-level* 2) (*print-pretty* nil))
    (loop for i from 0 and x across *simulated-memory*
          do (unless (equal x :uninit)
               (if w2b-p
                   (format t "~a: ~a~%" (%w2b i) x)
                   (format t "~a: ~a~%" (%q2b i) x))))))


;;; memory and register helpers

(defun %q2b (address)
  "Converts quadword address to byte address"
  (* 8 address))

(defun %b2q (address)
  "Converts byte addresses to our node/quadword-addressable memory"
  (if (zerop (rem address 8)) ;optimize?
      (floor address 8) ;never returns a float, but it wouldn't anyway because we know remainder=0
      (error "Non-qaligned address to memory-ref: #x~x" address)))

(defun %q2tagged-b (pointer tag)
  "Converts quadword addresses to a tagged byte address"
  (+ (%q2b pointer) tag))

(defun %tagged-b2q (pointer tag)
  "Converts tagged byte addresses to our node/quadword-addressable memory"
  (%b2q (- pointer tag)))

(defun %w2b (address)
  "Converts word address to byte address (for 32-bit ARM)"
  (* 4 address))

(defun %b2w (address)
  "Converts byte addresses to our word-addressable memory (for 32-bit ARM)"
  (if (zerop (rem address 4)) ;optimize?
      (floor address 4) ;never returns a float, but it wouldn't anyway because we know remainder=0
      (error "Non-qaligned address to memory-ref: #x~x" address)))

(defun %w2tagged-b (pointer tag)
   "Converts word addresses to a tagged byte address"
  (+ (%w2b pointer) tag))

(defun %tagged-b2w (pointer tag)
  "Converts tagged byte addresses to word-addressable memory"
  (%b2w (- pointer tag)))

(defun bool2bit (bool)
  "Convert T/NIL to 1/0"
  (if bool 1 0))

(defun alias-register (reg-name &optional reg-pos->size)
  "Returns the true name of a register from its alias and an optional data width"
  (loop for aliases in *register-names*
        for position = (position reg-name aliases)
        when position
          return (if reg-pos->size
		     (values (first aliases)
			     (funcall reg-pos->size position)) ; data-width
		     (first aliases))))

(defun mask-at-data-width (value data-width)
  "Returns the appropriate bytes of a value given a data width"
  (case data-width
    (64 value) ;truncate at 64?  What about sign?  sign-extend?
    (32 (ldb (byte 32 0) value))
    (8  (ldb (byte 8 0) value))))

#+OLD
(defmacro reg-value (register-name &optional data-width)
  "Accesses the value of a register"
  (declare (ignore data-width))
  `(gethash ,register-name *registers* ,register-name))

(defun reg-value (register-name &optional data-width)
  (declare (ignore data-width))
  (values (gethash register-name *registers* register-name))) ;strip 2nd value

(defun reg-set (register-name new-value &optional data-width)
  (declare (ignore data-width))
  (when (and (string= (symbol-name register-name) "FNAME") ;O(ugh) DBG
             (not (numberp new-value)))
    (break "tried to put closure into fname"))
  (setf (gethash register-name *registers*) new-value))

(defsetf reg-value reg-set)

(defmacro def-asm-alias (from to)
  "Defines an alias of an existing assembly instruction"
  `(defmacro ,from (&rest args)
     (list* ',to args)))

(defun strip-label (label) ;this was added because macroexpansion stopped at lambda
  "Removes ^ before a label if present"
  (if (listp label)
      (cadr label)
      label))


;;; core simulator functions

(defmacro %splice (&body body)
  "Adds a PROGN when there are multiple LAP instructions"
  (if (= 1 (length body))
      (first body)
      `(progn ,@body)))

(defun macroexpand-lap-tree (lap-tree)
  "Macroexpands the LAP tree"
  (cond ((or (atom lap-tree) ;label or symbol or constant
             (and (listp lap-tree) (eq 'QUOTE (first lap-tree))))
         lap-tree)   
        ((eq (car lap-tree) 'PROGN)
         (if (= 2 (length lap-tree))
             (second lap-tree)       ;optimize (progn x) => x
             `(PROGN
               ,@(mapcar #'macroexpand-lap-tree (cdr lap-tree)))))
        ((eq (car lap-tree) 'LET)
         `(let ,(second lap-tree) ;don't bother macroexpanding the bindings
            ,@(mapcar #'macroexpand-lap-tree (cddr lap-tree)))) ;is the list of Lisp forms after the bindings
        ((member (car lap-tree) '(%SPLICE))  ;special-case, so even if it were a macro, we'd be okay
         `(%EXPANDED-SPLICE
           ,@(mapcar #'macroexpand-lap-tree (cdr lap-tree))))
        ((eq (car lap-tree) '%EXPANDED-SPLICE)
         lap-tree)          ;already expanded: no recursion
        ((and (macro-function (car lap-tree))
	      (not (member (car lap-tree) '(SETF DECF INCF LAMBDA)))) ; lambda was causing infinite recursion
         ;;(print (car lap-tree))  ;DBG
         (let ((expansion (macroexpand lap-tree)))
           (if (atom expansion)
               expansion
               (macroexpand-lap-tree expansion))))
        ((listp lap-tree)   ;normal expression 
	 (let ((function (first lap-tree))
	       (arguments (rest lap-tree)))
	   `(,function ,@(mapcar #'macroexpand-lap-tree arguments))))))

(defun flatten-lap-tree (lap-tree)
  "Removes PROGN, %SPLICE, and %EXPANDED-SPLICE from the lap-tree"
  (if (and (listp lap-tree)
           (or (eq (car lap-tree) 'PROGN)
               (eq (car lap-tree) '%SPLICE)
               (eq (car lap-tree) '%EXPANDED-SPLICE)))      ;if looks like this (%expanded-splice x y z) loop on (x y z)
      (flatten-lap-tree-loop (rest lap-tree))
      ;;else
      (list lap-tree)))

(defun flatten-simple? (branch)
  "Checks if branch is doesn't need flattening (is non-null, doesn't begin with progn or %splice)"
  (and (not (null branch))               ;remove NILs
       (or (symbolp branch)              ;like a tag
           (and (listp branch)
                (not (or (eq (car branch) 'PROGN)
                         (eq (car branch) '%SPLICE)
                         (eq (car branch) '%EXPANDED-SPLICE)))))))

(defun flatten-recurse? (branch)
  "Checks if a branch needs flattening"
  (and (consp branch)
       (or (eq (car branch) 'PROGN)
           (eq (car branch) '%SPLICE)
           (eq (car branch) '%EXPANDED-SPLICE))))

(defun flatten-lap-tree-loop (forest)
  "Flattens a forest of LAP code into a simple tree"
  (loop for branch in forest
        when (flatten-simple? branch)
          collect branch
        when (flatten-recurse? branch)
          append (loop for sub-branch in (cdr branch)
                       append (flatten-lap-tree sub-branch))))

(defun sim-lookup-symbol (symbol)
  "Returns the value of a named constant"
  (symbol-value symbol))

(defun eval-constant (expression)
  "Evaluates a integers to themselves and symbols to their values"
  (cond ((integerp expression)
	 expression)
	((symbolp expression)
	 (sim-lookup-symbol expression)) ;like from the constants file
	((and (listp expression)
	      (eq (car expression) 'quote))
	 (error "quote found in constant expression: ~s" expression)) ;+++ constants NYI here
	((and (listp expression)
	      (eq (car expression) :APPLY))
	 (destructuring-bind (-apply- operator . arguments)
	     expression
           (declare (ignore -apply-))
	   (apply operator (mapcar #'eval-constant arguments))))))

#+ORIGINAL
(defmacro def-asm-lisp-fun (name options body)
  "Defines a lisp function whose body is LAP code for a given architecture"
  (let* ((architecture (destructuring-bind (&key architecture) options architecture)))
    (setf *register-names* (get architecture :register-names))
    (setf *this-function* name)
    `(eval-when (compile load eval)
       (defparameter ,name
         '(%SPLICE
           ,name  ; a label that can be called (and does n-arg checking, unlike internal call)
           ,(macroexpand-lap-tree body))))))

(defmacro def-asm-lisp-fun (name options body)
  "Defines a lisp function whose body is LAP code for a given architecture"
  (let* ((architecture (destructuring-bind (&key architecture) options architecture)))
    (setf *register-names* (get architecture :register-names))
    (setf *this-function* name) ;This should probably be bound using (LET ... (declare (special ...)) ...)
    (let ((as-translator (get architecture :as-translator)))
      (if as-translator
	  (funcall as-translator body name)))
    `(eval-when (compile load eval)
       (defparameter ,name
         '(%SPLICE
           ,name  ; a label that can be called (and does n-arg checking, unlike internal call)
           ,(macroexpand-lap-tree body))))
    ))

(defmacro def-asm-fun (name options body)
  "Defines a subprimitive whose body is LAP code for a given architecture"
  ;;;The only difference between def-asm-fun and def-asm-lisp-fun is binding *this-function*
  ;;; Lisp funs have a real function object, asm funs do not
  (let* ((architecture (destructuring-bind (&key architecture) options architecture))
         (*register-names* (get architecture :register-names)))
    (let ((as-translator (get architecture :as-translator)))
      (if as-translator
	  (funcall as-translator body name)))
    `(eval-when (compile load eval)
       (defparameter ,name
         '(%SPLICE
           ,name                          ;label
           ,(macroexpand-lap-tree body))))))

;;;

(defmacro $ (constant)
  constant)

(defmacro ^ (label)
  label)

;;; flags
  
(defmacro carry-flag-bit (f)
  `(ldb (byte 1 0) ,f))

(defmacro zero-flag-bit (f)
  `(ldb (byte 1 6) ,f))

(defmacro sign-flag-bit (f)
  `(ldb (byte 1 7) ,f))

(defmacro overflow-flag-bit (f)
  `(ldb (byte 1 11) ,f))


(defun zero-flag-p (flags)
  "Returns T if zero flag is set"
  (= 1 (zero-flag-bit flags)))

(defun overflow-flag-p (flags)
  "Returns T if overflow flag is set"
  (= 1 (overflow-flag-bit flags)))

(defun carry-flag-p (flags)
  "Returns T if carry flag is set"
  (= 1 (carry-flag-bit flags)))

;;(setf (%128 imm1 imm0) value)

(defun set-flags! (result size)
  "Sets zero, sign, carry, and overflow flags according to a value and data width"
  (setf (zero-flag-bit flags) (bool2bit (zerop result)))
  (setf (sign-flag-bit flags) (bool2bit (minusp result)))
  (let ((carry-bit (bool2bit (> (abs result) (expt 2 size))))) ;probably wrong, will cons if size > 59
    (setf (carry-flag-bit flags) carry-bit)
    (setf (overflow-flag-bit flags) carry-bit))) ;probably doubly wrong


;; %__ functions could be moved here and used across simulators
