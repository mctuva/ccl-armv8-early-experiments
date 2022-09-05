;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SARM; Base: 10 -*-

(in-package :SARM)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Revision history:
;;; w0713 - Ayla - register aliases and a handful of instructions
;;; w0719 - Ayla - added allocators, def-simulator-code, init-simulator
;;; w0720 - Ayla - need to replace b2q with b2w, sploadlr and blx still unimplemented
;;; w0721 - Ayla - function calls seem to be working - list-top-level doesn't throw errors but returns NIL. Still todo: sploadlr and blx,
;;; w0721 - Tim  - Fixed reference to quoted integers, wrote %integer2fixnum
;;;                Changed LDR so it doesn't special-case
;;;                ++A++ (diff this to cleanly incorporate into your version, maybe without false-start, and comments)
;;; w0722 - Ayla - added function objects, re-added pc ldr and ldmia special case
;;;                once we fix blx, we should be able to call list2
;;; w0722a - Tim - Try to fix BLX
;;; w0722b -Ayla - added ARM-AND and fixed stmbd and ldmia (reversed the wrong one)
;;;                list example works!! (with sploadlr commented out)
;;; w0723 - Tim  - New instructions used in builtin_times, makes64 and makes32; TO-DO list
;;; w0807 - Tim  - added missing backquote in asr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant tag-fixnum 0)
(defconstant tag-list 1)
(defconstant tag-misc 2)
(defconstant tag-imm 3)

(defconstant fulltag-even-fixnum 0)
(defconstant fulltag-nil 1) ;  NIL and nothing but.  (Note that there's still a hidden NILSYM.)
(defconstant fulltag-nodeheader 2) ; Header of heap_allocated object that contains lisp_object pointers
(defconstant fulltag-imm 3) ; a "real" immediate object.  Shares TAG with fulltag-immheader
(defconstant fulltag-odd-fixnum 4)
(defconstant fulltag-cons 5) ; a real (non-null) cons.  Shares TAG with fulltag-nil.
(defconstant fulltag-misc 6) ; Pointer "real" tag-misc object.  Shares TAG with fulltag-nodeheader.
(defconstant fulltag-immheader 7) ; Header of heap-allocated object that contains unboxed data.

(defconstant nil-value (+ #x13000 fulltag-nil));;(+ #x04000000 fulltag-nil)) ;this is outside of memory
(defconstant misc-bias fulltag-misc)
(defconstant cons-bias fulltag-cons)

(defconstant nbits-in-word 32)
(defconstant nbits-in-byte 8)
(defconstant ntagbits 3)     ; But only 2 are significant to lisp
(defconstant nlisptagbits 2)
(defconstant nfixnumtagbits 2)
(defconstant num-subtag-bits 8)
(defconstant fixnumshift 2)
(defconstant fixnum-shift 2)
(defconstant fulltagmask 7)
(defconstant tagmask 3)
(defconstant fixnummask 3)
(defconstant subtag-mask #xff)        
(defconstant ncharcodebits 24)
(defconstant charcode-shift (- nbits-in-word ncharcodebits))
(defconstant word-shift 2)
(defconstant node-size 4)
(defconstant dnode-size 8)
(defconstant dnode-align-bits 3)
(defconstant dnode-shift dnode-align-bits)
(defconstant bitmap-shift 5)

(defconstant t-offset (+ (- dnode-size fulltag-nil) fulltag-misc))
(defconstant t-value (+ nil-value t-offset))

(defvar *symbol-table* (make-hash-table))

(defparameter *tcr-address* #x10000)

(defparameter *stack-base* *simulated-memory-size*)

(defparameter *value-stack-base* #x2000)

(defvar *allocbase* #x5000)

(defconstant *word-size* 4)

;;;

(EVAL-WHEN (compile load eval)

(setf (get :ARM32 :register-names)
  '((imm0)            ;r0 
    (imm1)            ;r1
    (imm2 nargs)      ;r2
    (rcontext)        ;r3
    (arg-z)           ;r4
    (arg-y)           ;r5
    (arg-x)           ;r6
    (temp0 allocbase) ;r7
    (temp1 fname next-method-context) ;r8
    (temp2 nfn)       ;r9
    (vsp)             ;r10
    (fn)              ;r11
    (allocptr)        ;r12
    (sp)              ;r13
    (lr)              ;r14
    (pc)))            ;r15
      
) ;eval-when

(defmacro def-simulator-code (name &key lispfuns builtins)
  "Top level simulator"
  (let* ((procedures (append lispfuns builtins))
         (main-name (first procedures))
         (expansions (mapcar #'symbol-value procedures))
         (call-main (macroexpand-lap-tree `(blx (^ ,main-name))))
         (flattened (flatten-lap-tree `(%SPLICE
					 (mov (% nargs) ($ 0))					 (ldr (% temp1) (+ *word-size* (get ',main-name 'sim-symbol-address))) ;; offset from header
					 (ldr (% nfn) (@ (% temp1) ($ 6)))
                                         ,call-main
                                         (return-from ,name (convert-result (% arg-z)))
                                         ,@expansions))))
    `(defun ,name ()
       (init-simulator ',lispfuns) ;init registers and fill simulated memory with constant vectors and "trampoline" symbols
       (tagbody
	  ,@(loop for fun-name in lispfuns
                  collect (initialize-go-thunk-to-lisp-fun fun-name)) ;just like mapcar now
          ,@flattened))))

(defun init-simulator (lisp-fun-names)
  "Clears simulated memory and the register hashtable, allocates symbols and the TCR in memory, and sets values of prefilled registers."
  (dotimes (i *simulated-memory-size*)
    (setf (aref *simulated-memory* i) :uninit))
  (setf *registers* (if *registers* (clrhash *registers*) (make-hash-table)))
  (setq *simulated-memory-pointer* 0)
  ;; initialize memory
  (initialize-nil)
  (initialize-t)
  (mapc #'allocate-symbol lisp-fun-names)
  (mapc #'local-constants-table lisp-fun-names)
  (mapc #'allocate-constants-table lisp-fun-names)
  (mapc #'allocate-function-object lisp-fun-names)
  (initialize-tcr)
  ;; initialize register
  (setf (reg-value 'sp) *stack-base*)
  (setf (reg-value 'vsp) *value-stack-base*)
  (setf (reg-value 'rcontext) *tcr-address*)
  (setf (reg-value 'allocbase) *allocbase*)
  (setf (reg-value 'allocptr) *allocbase*))

(defun initialize-go-thunk-to-lisp-fun (fun-name)
  "Returns a list of 'instructions' that creates a go-thunk and puts it in the function cell of the symbol in simulated memory"
  `(setf (aref *simulated-memory* (function-code-vector (get ',fun-name 'sim-fn-address)))
         (lambda () (go ,fun-name))))


;;; memory allocation

(defun memory-ref (address)
  "References the value stored at a word-aligned byte address in simulated memory"
  (aref *simulated-memory* (%b2w address)))

(defun memory-set (address new)
  "Sets the addressed cell of simulated memory to a new value"
  (setf (aref *simulated-memory* (%b2w address)) new))

(defsetf memory-ref memory-set)

(defun new-aligned-object (length)
  "Incrememnts the *simulated-memory-pointer* to the next double word-aligned slot after the new object"
  (prog1 
      (if (oddp *simulated-memory-pointer*)
	  (incf *simulated-memory-pointer*)
	  *simulated-memory-pointer*)
    (incf *simulated-memory-pointer* length)))

(defun initialize-tcr ()
  "Initializes the Thread Context Record (TCR), well, really just the slots we care about to make CONSing work"
  (setf (gethash 'rcontext *registers*) *tcr-address*)
  (setf (aref *simulated-memory* (%b2w *tcr-address*)) "this is the tcr") ;what actually goes here and do we care?
  (setf (aref *simulated-memory* (%b2w (+ #x68 *tcr-address*))) *allocbase*))

(defun allocate-symbol (sym)
  "Allocates a new symbol in simulated memory"
  (let* ((structure-size 7)
	 (original-pointer (new-aligned-object (1+ structure-size)))
	 (tagged-address (%w2tagged-b original-pointer tag-misc))) ;??? is this the right tag? or should it be misc?
    (setf (gethash sym *symbol-table*) original-pointer)
    (setf (get sym 'sim-symbol-address) tagged-address)
    (setf (aref *simulated-memory* original-pointer) structure-size) ; 1st element is the header which gives the length ++A++ need accurate header
    (setf (aref *simulated-memory* (symbol-pname tagged-address))              (symbol-name sym)) ; implement strings
    (setf (aref *simulated-memory* (symbol-value-cell tagged-address))          nil-value)
    (setf (aref *simulated-memory* (symbol-function-cell tagged-address))       nil-value)
    (setf (aref *simulated-memory* (symbol-package-predicate tagged-address))   nil-value)
    (setf (aref *simulated-memory* (symbol-flags tagged-address))               nil-value)
    (setf (aref *simulated-memory* (symbol-plist-cell tagged-address))          nil-value)
    (setf (aref *simulated-memory* (symbol-binding-index tagged-address))       nil-value)))

;; nil has its own tag and is both a symbol and a list so it has to be treated differently
(defun initialize-nil ()
  "Allocates the symbol structure of NIL in simulated memory"
  (let ((nil-ptr (%tagged-b2w nil-value fulltag-nil)))
    (setf (aref *simulated-memory* nil-ptr) "nil-header-placeholder")
    (setf (aref *simulated-memory* (+ nil-ptr 1)) "NIL") ; pname
    (setf (aref *simulated-memory* (+ nil-ptr 2)) nil-value) ; vcell
    (setf (aref *simulated-memory* (+ nil-ptr 3)) nil-value) ; fcell
    (setf (aref *simulated-memory* (+ nil-ptr 4)) nil-value) ; pkg pred
    (setf (aref *simulated-memory* (+ nil-ptr 5)) nil-value) ; flags
    (setf (aref *simulated-memory* (+ nil-ptr 6)) nil-value) ; plist
    (setf (aref *simulated-memory* (+ nil-ptr 7)) nil-value))) ; binding index

(defun initialize-t ()
  "Allocates the symbol structure of T in simulated memory"
  (setf (aref *simulated-memory* (%tagged-b2w t-value fulltag-nodeheader)) "t-header-placeholder") ;++A++
  (setf (aref *simulated-memory* (symbol-pname t-value)) "T") ; implement strings
  (setf (aref *simulated-memory* (symbol-value-cell t-value)) t-value)
  (setf (aref *simulated-memory* (symbol-function-cell t-value)) nil-value)
  (setf (aref *simulated-memory* (symbol-package-predicate t-value)) nil-value)
  (setf (aref *simulated-memory* (symbol-flags t-value)) nil-value)
  (setf (aref *simulated-memory* (symbol-plist-cell t-value)) nil-value)
  (setf (aref *simulated-memory* (symbol-binding-index t-value)) nil-value))

;; symbol cell accessors

(defun symbol-pname (symbol-address)
  "Returns the simulated memory quad address of a symbol's name"
  (+ (%tagged-b2w symbol-address tag-misc) 1))

(defun symbol-value-cell (symbol-address)
  "Returns the simulated memory quad address of a symbol's value cell"
  (+ (%tagged-b2w symbol-address tag-misc) 2))

(defun symbol-function-cell (symbol-address)
  "Returns the simulated memory quad address of a symbol's function cell"
  (+ (%tagged-b2w symbol-address tag-misc) 3))

(defun symbol-package-predicate (symbol-address)
  "Returns the simulated memory quad address of a symbol's package predicate"
  (+ (%tagged-b2w symbol-address tag-misc) 4))

(defun symbol-flags (symbol-address)
  "Returns the simulated memory quad address of a symbol's flags"
  (+ (%tagged-b2w symbol-address tag-misc) 5))

(defun symbol-plist-cell (symbol-address)
  "Returns the simulated memory quad address of a symbol's property list"
  (+ (%tagged-b2w symbol-address tag-misc) 6))

(defun symbol-binding-index (symbol-address)
  "Returns the simulated memory quad address of a symbol's binding index"
  (+ (%tagged-b2w symbol-address tag-misc) 7))


;;; function

(defun allocate-function-object (function-name)
  "Allocates a function object"
  (let* ((structure-size 2)
	 (original-pointer (new-aligned-object (1+ structure-size)))
	 (tagged-address (%w2tagged-b (1+ original-pointer) tag-misc))) ;offset from header
    (setf (get function-name 'sim-fn-address) tagged-address)
    (setf (aref *simulated-memory* original-pointer) function-name) ;fake header
    (setf (aref *simulated-memory* (function-code-vector tagged-address)) "code")
    (setf (aref *simulated-memory* (function-constants-vector tagged-address))
	  (get function-name 'constants-vector-address))
    (setf (aref *simulated-memory* (symbol-function-cell (get function-name 'sim-symbol-address)))
	  tagged-address)))

(defun function-code-vector (function-address)
  "Code vector accessor"
  (+ (%tagged-b2w function-address tag-misc) 0))

(defun function-constants-vector (function-address)
  "Constants vector accessor"
  (+ (%tagged-b2w function-address tag-misc) 1))

(defun allocate-constants-table (function-name)
  "Allocates a function's constants-vector in simulated memory"
  (let* ((table (get function-name 'constants-table))
	 (length (hash-table-count table))
	 (original-pointer (new-aligned-object (1+ length))))
    (setf (aref *simulated-memory* original-pointer) length) ; 1st element is the header which gives the length ++A++ need accurate header
    (loop for constant being the hash-key using (hash-value offset) of table
	  do (let ((index (+ original-pointer (%b2w offset)))) ; preserve constants order
	       (typecase constant
		 (symbol
		  (setf (aref *simulated-memory* index) (+ *word-size* (get constant 'sim-symbol-address)))) ;points to first cell, not header
		 (otherwise
		  (error "NYI")))))
    (setf (get function-name 'constants-vector-address) (%w2b original-pointer))))
          ;;(%w2tagged-b original-pointer fulltag-misc)))) ;should point to the header (eventually)

(defun local-constants-table (function-name)
  "Allocates a hash table for each function that maps from values to constants-vector offsets"
  (let ((table (get function-name 'constants-table)))
    (unless table
      (setq table (make-hash-table :test 'equal))             ;that maps constants to offsets
      (setf (get function-name 'constants-table) table)      ;(note: hash tables, by default are "EQ hash tables")
      (setf (get function-name 'constants-table-index) 0)) ;the first index of the vector is for the header, not constants
    table))

(defun lookup-local-constant (constant)
  (declare (special *this-function*))
  (let ((table (local-constants-table *this-function*)))
    (gethash constant table)))

(defun allocate-new-constant (constant)
  "Adds a constant to *this-function*'s constants table, with an associated offset"
  (declare (special *this-function*))
  (let* ((table (local-constants-table *this-function*))
	 (offset (+ *word-size* (get *this-function* 'constants-table-index)))) ;pre-scaled offset into the constants vector
    (setf (gethash constant table) offset)
    (setf (get *this-function* 'constants-table-index) offset)    ;so you can never have a constant in index=0 (Tim said this was okay)
    offset))

(defun local-constant-offset (constant)
  "Returns the offset of a constant from the beginning of *this-function*'s constants table"
  (declare (special *this-function*))
  (let ((table (local-constants-table *this-function*)))
    (multiple-value-bind (offset found?)
        (gethash constant table)
      (if found?
          offset
          (allocate-new-constant constant)))))

;;;

(defun convert-result (x)
  "Converts simulator result to reader-friendly lisp result"
  (cond
    ((%fixnum-p x) (%unbox-fixnum x))
    ((%is-t x) t)
    ((%null x) nil)
    (t (error "nyi"))))

(defun %fixnum-p (x)
  "Returns T if x has fixnum tagbits"
  (= (mod x 4) 0)) ; low 2 bits = 00

(defun %is-t (x)
  "Returns T if t-value"
  (= x t-value))

(defun %null (x)
  "Returns T if nil-value"
  (= x nil-value))

(defun %unbox-fixnum (fixnum)
  "Arithmetic shift right by 3"
  (ash fixnum (- fixnumshift)))

(defun %integer2fixnum (integer)
  "Boxes an integer"
  (ash integer fixnumshift))


(defmacro % (register-name)
  "The register value"
  (let ((reg (alias-register register-name)))
    `(reg-value ',reg)))

(defmacro @ (&rest mem-ref)
  "The value in simulated memory addressed by the register value + the offset"
  (declare (special *this-function*))
  (case (length mem-ref) ;mem-ref doesn't include the @
    (1 
      ;; absolute: (@ symbol)
      (cond ((atom (first mem-ref))  ;+++ should this be SYMBOLP again?
	     (sim-lookup-symbol (first mem-ref)))))
    (2
      ;; register with displacement 
     (let ((constant (cond ((cl:and (listp (second mem-ref)) ; (@ (% register) ($ offset))
                                    (eq '$ (first (second mem-ref))))
			    (second (second mem-ref)))
			   ((or (atom (second mem-ref)) ; (@ (% register) constant)
                               (not (eq 'quote (first (second mem-ref)))))
                            (eval-constant (second mem-ref)))  ;+++ should really integrate constant vector references
                           (t
                            (second mem-ref))))
	    (register-name (second (first mem-ref))))
        ;;(when (and (listp constant)
        ;;           (eq :apply (first constant)))
        ;;  (setq constant (eval-constant constant)))
	(cond ((integerp constant) ;assume not a bignum
	       `(memory-ref (+ (% ,register-name) ,constant)))
	      ((eq register-name 'fn)		      ;+++ package?
	       (let ((offset (local-constant-offset (second constant)))) ; ''my-expt => 'my-expt  ;how do we set this?
                 `(memory-ref (+ ,offset (get ',*this-function* 'constants-vector-address))))) ;;(% ,register-name)))))
              ;;(t
	      ;; `(memory-ref (+ ,constant (% ,register-name))))
	      ;; Constant can be any Lisp object if the register is FN
	      ;; it this case, it is essentially an assembler directive:
	      ;; a) allocate a slot in the function's constants vector
	      ;; b) insert the constant, and c) return the integer offset
	      )))))



(defmacro def-arm-instruction (name arglist &body body)
  "Defines an arm instruction"
  `(defmacro ,name ,arglist
     ,@body))

(defun parse-simple-mem-arg (reg-ref)
  "Helper for incrementing the memory address when using ldmia and stmdb"
  (if (eq (car reg-ref) '!)
      (values (second reg-ref) t)
      (values reg-ref nil)))

(def-arm-instruction LDMIA (stack-pointer register-list) ; pop
  "Load multiple, increment after"
  (multiple-value-bind (stack-reg index-p)
      (parse-simple-mem-arg stack-pointer)
    (if index-p
        (let ((clauses (loop for register in register-list
                             collect `(%splice
                                        (setf ,register (memory-ref ,stack-reg))
					(incf ,stack-reg *word-size*))))
	      (pc-call (if (member '(% pc) register-list :test 'equal) ;; loading pc is a function call
			   `(funcall (% pc)))))
	  (if pc-call
              `(%splice ,@clauses ,pc-call)
	      `(%splice ,@clauses)))
        "this case doesn't appear"
	)))

(def-arm-instruction STMDB (stack-pointer register-list) ; push 
  "Store multiple, decrement before"
  (multiple-value-bind (stack-reg index-p)
      (parse-simple-mem-arg stack-pointer)
    (if index-p
        (let ((clauses (loop for register in (reverse register-list)
                             collect `(%splice (decf ,stack-reg *word-size*)
                                               (setf (memory-ref ,stack-reg) ,register)))))
          `(%splice ,@clauses))
        "this case doesn't appear"
	)))


(def-arm-instruction STR (reg-src mem-dest)
  "Store register"
  `(setf ,mem-dest ,reg-src))

(def-arm-instruction LDR (reg-dest mem-src)
  "Load register"
  (if (equal reg-dest '(% pc))     ;; we still need to handle the pc when it's loaded
      `(%splice
	 (setf ,reg-dest ,mem-src)
	 (funcall ,mem-src))
      `(setf ,reg-dest ,mem-src)))

(defun unquote (expression)
  "Converts 'symbol -> symbol"
  (second expression))

(defun quoted-integer-p (expression)
  "Returns T on quoted integers like '2"
  (cl:and (eq 'quote (first expression))
          (integerp (second expression))))

(def-arm-instruction MOV (dest src)
  "Move"
  (if (quoted-integer-p src)
      `(setf ,dest ,(%integer2fixnum (unquote src)))
      `(setf ,dest ,src)))

(def-arm-instruction B (label)
  "Branch (relative)"
  `(go ,(strip-label label)))

(def-arm-instruction BEQ (label)
  "Branch if equal"
  `(when (zero-flag-p flags)
     (go ,(strip-label label))))

(def-arm-instruction BNE (label)
  "Branch if not equal"
  `(when (not (zero-flag-p flags))
     (go ,(strip-label label))))

(def-arm-instruction BHI (label)
  "Branch if unsigned higher"
  `(when (cl:and (not (zero-flag-p flags))
                 (carry-flag-p flags))
     (go ,(strip-label label))))

;;; New

(def-arm-instruction ASR (dest src shift)
  "Arithmetic shift right"
  `(setf ,dest (ash ,src (- ,shift))))

(def-arm-instruction SMULL (dlo dhi src1 src2)
  "Signed multiply low"
  `(multiple-value-bind (high low)
       (%imul ,src1 , src2)  ;this is not faithful or arch specific: lo=product, hi=0
     (setf ,dhi high)
     (setf ,dlo low)))

(def-arm-instruction CMP (rn op2 &optional shift) ; op2 is a register or literal
  "Compare"
  (cond ((null shift)
         `(setf flags (%cmpq ,rn ,op2)))
        (t
         (let ((amount (second shift)))
           `(setf flags (%cmpq ,rn (ash ,op2 (- ,amount))))))))

(def-arm-instruction BLX (thing)
  "Branch with link and exchange instruction set"
  (let ((return-label (gensym "return-label-"))
        (caller (case (first thing)
                  (% '(funcall branch-target))
                  (^ `(go ,(second thing))))))
    `(%splice
       (let ((branch-target ,thing)) ;+++ unnecessary in the ^ case, but we're just going to leave this here for now
         (setf (% lr) (lambda () (go ,return-label)))
         ,caller)
       ,return-label)))

;; loads a subprim from the jump table at the end of the TCR into LR
(def-arm-instruction sploadlr (subprim) ;;unimplemented
  (declare (ignore subprim))
  )

(def-arm-instruction AND (rd rn op2)
  "And"
  `(setf ,rd (%andl ,rn ,op2)))

(def-arm-instruction ADD (rd rn op2)
  "Add"
  `(setf ,rd (%add ,rn ,op2)))

(def-arm-instruction ADDEQ (rd rn op2)
  "Add if equal"
  `(when (zero-flag-p flags)
     (setf ,rd (%add ,rn ,op2))))

(def-arm-instruction SUB (rd rn op2)
  "Subtract"
  `(setf ,rd (%sub ,rn ,op2)))

(def-arm-instruction BIC (rd rn op2)
  "Bit clear"
  `(setf ,rd (%bic ,rn ,op2)))

(def-arm-instruction VPUSH1 (rd)
  "Push to value stack"
  `(%splice
     (decf (% vsp) *word-size*) ;grows down
     (setf (aref *simulated-memory* (%b2w (% vsp))) ,rd)))

(def-arm-instruction uuo-error-reg-not-lisptag (&rest error-info)
  (declare (ignore error-info))
  ;; nop
  )

(def-arm-instruction uuo-error-wrong-nargs (&rest error-info)
  (declare (ignore error-info))
  ;; nop
  )

(def-arm-instruction uuo-alloc-trap ()
  ;; nop
  )

(defun %andl (arg src-dest)
  (logand arg src-dest)
  ;;set flags
  )

(defun %add (rn op2) ; doesn't set flags
  (+ rn op2))

(defun %sub (rn op2) ; doesn't set flags
  (- rn op2))

(defun %bic (rn op2)
  (logand rn (lognot op2)))

(defun %cmpq (arg1 arg2)
  (let ((temp (- arg1 arg2)))
    (set-flags! temp 64)
    flags))
