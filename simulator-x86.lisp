;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SX86; Base: 10 -*-

(in-package :SX86)

;;;;; Version history:
;;; w0718 - Ayla - Expt and cons are now working in this package system
;;; w0719 - Ayla - Moved def-simulator-code into this file. Weird issue where % refrences sarm::% rather than sx86::%


(defvar *data-width* 0) ;privately used by instruction macros

(defparameter *stack-base* *simulated-memory-size*) ;grows down, double word aligned

(defparameter *tcr-address* #x10000)

(defvar *symbol-table* (make-hash-table))


(eval-when (compile load eval)

(defconstant fixnummask 7)
(defconstant fixnumshift 3)

(defconstant tag-size 3)
(defconstant ntagbits 4)

(defconstant fulltag-nodeheader-0 5) ; strings
(defconstant fulltag-immheader-1 9)
(defconstant fulltag-nil 11)
(defconstant fulltag-misc 13)
(defconstant fulltag-sym 14)
(defconstant fulltag-function 15)

(defconstant num-subtag-bits 8)

(defconstant subtag-bignum             (logior fulltag-immheader-1 (ash 1 ntagbits)))
(defconstant two-digit-bignum-header   (logior (ash 2 num-subtag-bits) subtag-bignum))
(defconstant three-digit-bignum-header (logior (ash 3 num-subtag-bits) subtag-bignum))
(defconstant four-digit-bignum-header  (logior (ash 4 num-subtag-bits) subtag-bignum))

(defconstant node-size 8)

(defconstant misc-header-offset (- fulltag-misc))
(defconstant misc-data-offset (+ misc-header-offset node-size)) ;/* first word of data */

(defconstant nil-value (+ #x13000 fulltag-nil))
(defconstant t-value (+ #x13020 fulltag-sym))
(defconstant t-offset (- t-value nil-value))


(defmacro def-simulator-code (name &key lispfuns builtins)
  "Top level simulator"
  (let* ((procedures (append lispfuns builtins))
         (main-name (first procedures))
         (expansions (mapcar #'symbol-value procedures))
         (call-main (macroexpand-lap-tree `(call (^ ,main-name))))
         (flattened (flatten-lap-tree `(%SPLICE
					 (movl ($ 0) (% nargs))
                                         ,call-main
                                         (return-from ,name (convert-result (% arg-z)))
                                         ,@expansions))))
    `(defun ,name ()
       (init-simulator ',lispfuns) ;init stack pointer and fill simulated memory with constant vectors and "trampoline" symbols
       (tagbody
	  ,@(loop for fun-name in lispfuns
                  collect (initialize-go-thunk-to-lisp-fun fun-name)) ;just like mapcar now
          ,@flattened))))

;;init-simulator is language dependent

(defun init-simulator (lisp-fun-names)
  "Clears simulated memory and the register hashtable, allocates symbols and the TCR in memory, and sets values of prefilled registers."
  (dotimes (i *simulated-memory-size*)
    (setf (aref *simulated-memory* i) :uninit))
  (setq *simulated-memory-pointer* 0)
  ;; initialize memory
  (initialize-nil)
  (initialize-t)
  (mapc #'allocate-symbol lisp-fun-names)
  (mapc #'local-constants-table lisp-fun-names)
  (mapc #'allocate-constants-table lisp-fun-names)
  (initialize-tcr)
  ;; initialize stack pointer and rcontext
  (setf (gethash 'rsp *registers*) *stack-base*)
  (setf (gethash 'rcontext *registers*) *tcr-address*))

(defvar save-allocptr #x5000)
(defconstant base-allocptr #x5000)


;;; memory allocation

(defun new-aligned-object (length)
  "Incrememnts the *simulated-memory-pointer* to the next doublenode-aligned slot after the new object"
  (prog1 
      (if (oddp *simulated-memory-pointer*)
	  (incf *simulated-memory-pointer*)
	  *simulated-memory-pointer*)
    (incf *simulated-memory-pointer* length)))

(defun initialize-tcr ()
  "Initializes the Thread Context Record (TCR), well, really just the slots we care about to make CONSing work"
  (setf (gethash 'rcontext *registers*) *tcr-address*)
  (setf (aref *simulated-memory* (%b2q *tcr-address*)) "this is the tcr") ;what actually goes here and do we care?
  (setf (aref *simulated-memory* (%b2q (+ #xD8 *tcr-address*))) save-allocptr)
  (setf (aref *simulated-memory* (%b2q (+ #xE0 *tcr-address*))) base-allocptr))

(defun allocate-symbol (sym)
  "Allocates a new symbol in simulated memory"
  (let* ((structure-size 7)
	 (original-pointer (new-aligned-object (1+ structure-size)))
	 (tagged-address (%q2tagged-b original-pointer fulltag-sym)))
    (setf (gethash sym *symbol-table*) original-pointer)
    (setf (get sym 'sim-symbol-address) tagged-address)
    (setf (aref *simulated-memory* original-pointer) structure-size) ; 1st element is the header which gives the length ++A++ need accurate header
    (setf (aref *simulated-memory* (symbol-pname tagged-address))              (allocate-string (symbol-name sym))) ; implement strings
    (setf (aref *simulated-memory* (symbol-value-cell tagged-address))          nil-value)
    (setf (aref *simulated-memory* (symbol-function-cell tagged-address))       nil-value)
    (setf (aref *simulated-memory* (symbol-package-predicate tagged-address))   nil-value)
    (setf (aref *simulated-memory* (symbol-flags tagged-address))               nil-value)
    (setf (aref *simulated-memory* (symbol-plist-cell tagged-address))          nil-value)
    (setf (aref *simulated-memory* (symbol-binding-index tagged-address))       nil-value)))

;; nil has its own tag and is both a symbol and a list so it has to be treated differently
(defun initialize-nil ()
  "Allocates the symbol structure of NIL in simulated memory"
  (let ((nil-ptr (%tagged-b2q nil-value fulltag-nil)))
    (setf (aref *simulated-memory* nil-ptr) "nil-header-placeholder")
    (setf (aref *simulated-memory* (+ nil-ptr 1)) (allocate-string "NIL")) ; pname
    (setf (aref *simulated-memory* (+ nil-ptr 2)) nil-value) ; vcell
    (setf (aref *simulated-memory* (+ nil-ptr 3)) nil-value) ; fcell
    (setf (aref *simulated-memory* (+ nil-ptr 4)) nil-value) ; pkg pred
    (setf (aref *simulated-memory* (+ nil-ptr 5)) nil-value) ; flags
    (setf (aref *simulated-memory* (+ nil-ptr 6)) nil-value) ; plist
    (setf (aref *simulated-memory* (+ nil-ptr 7)) nil-value))) ; binding index

(defun initialize-t ()
  "Allocates the symbol structure of T in simulated memory"
  (setf (aref *simulated-memory* (%tagged-b2q t-value fulltag-sym)) "t-header-placeholder")
  (setf (aref *simulated-memory* (symbol-pname t-value)) (allocate-string "T")) ; implement strings
  (setf (aref *simulated-memory* (symbol-value-cell t-value)) t-value)
  (setf (aref *simulated-memory* (symbol-function-cell t-value)) nil-value)
  (setf (aref *simulated-memory* (symbol-package-predicate t-value)) nil-value)
  (setf (aref *simulated-memory* (symbol-flags t-value)) nil-value)
  (setf (aref *simulated-memory* (symbol-plist-cell t-value)) nil-value)
  (setf (aref *simulated-memory* (symbol-binding-index t-value)) nil-value))

;; symbol cell accessors

(defun symbol-pname (symbol-address)
  "Returns the simulated memory quad address of a symbol's name"
  (+ (%tagged-b2q symbol-address fulltag-sym) 1))

(defun symbol-value-cell (symbol-address)
  "Returns the simulated memory quad address of a symbol's value cell"
  (+ (%tagged-b2q symbol-address fulltag-sym) 2))

(defun symbol-function-cell (symbol-address)
  "Returns the simulated memory quad address of a symbol's function cell"
  (+ (%tagged-b2q symbol-address fulltag-sym) 3))

(defun symbol-package-predicate (symbol-address)
  "Returns the simulated memory quad address of a symbol's package predicate"
  (+ (%tagged-b2q symbol-address fulltag-sym) 4))

(defun symbol-flags (symbol-address)
  "Returns the simulated memory quad address of a symbol's flags"
  (+ (%tagged-b2q symbol-address fulltag-sym) 5))

(defun symbol-plist-cell (symbol-address)
  "Returns the simulated memory quad address of a symbol's property list"
  (+ (%tagged-b2q symbol-address fulltag-sym) 6))

(defun symbol-binding-index (symbol-address)
  "Returns the simulated memory quad address of a symbol's binding index"
  (+ (%tagged-b2q symbol-address fulltag-sym) 7))

(defun allocate-string (str)
  "Allocates a string in memory"
  (let ((original-pointer (new-aligned-object (+ 1 (length str)))))
    (setf (aref *simulated-memory* original-pointer) "string header") ; length and tag?
    (loop for ptr from (+ 1 original-pointer) and char across str
	  do (setf (aref *simulated-memory* ptr) char))
    (%q2tagged-b original-pointer fulltag-nodeheader-0)))

;;; function constants table

(defun allocate-constants-table (function-name)
  "Allocates a function's constants-vector in simulated memory"
  (let* ((table (get function-name 'constants-table))
	 (length (hash-table-count table))
	 (original-pointer (new-aligned-object (1+ length))))
    (setf (aref *simulated-memory* original-pointer) "symbol header") ; 1st element is the header ++A++ need accurate header
    (loop for constant being the hash-key using (hash-value offset) of table
	  do (let ((index (+ original-pointer (%b2q offset)))) ; preserve constants order
	       (typecase constant
		 (symbol
		  (setf (aref *simulated-memory* index) (get constant 'sim-symbol-address)))
		 (otherwise
		  (error "NYI")))))
    (setf (get function-name 'constants-vector-address)
          (%q2tagged-b original-pointer fulltag-function)))) ;should point to the header (eventually)

(defun local-constants-table (function-name)
  "Allocates a hash table for each function that maps from values to constants-vector offsets"
  (let ((table (get function-name 'constants-table)))
    (unless table
      (setq table (make-hash-table :test 'equal))          ; maps constants to offsets
      (setf (get function-name 'constants-table) table)
      (setf (get function-name 'constants-table-index) 0)) ;the first index of the vector is for the header, not constants
    table))

#+UNUSED?
(defun lookup-local-constant (constant)
  (declare (special *this-function*))
  (let ((table (local-constants-table *this-function*)))
    (gethash constant table)))

(defun allocate-new-constant (constant)
  "Add a constant to *this-function*'s constants table, with an associated offset"
  (declare (special *this-function*))
  (let* ((table (local-constants-table *this-function*))
	 (offset (+ 8 (get *this-function* 'constants-table-index)))) ;pre-scaled offset into the constants vector
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

) ;eval-when
;;;


(EVAL-WHEN (compile load eval)
  
(setf (get :x8664 :register-names)   ;Was defparameter *register-names*
      '((arg-x arg-x.q arg-x.l arg-x.b) ;nodes
        (arg-y arg-y.q arg-y.l arg-y.b)
        (arg-z arg-z.q arg-z.l arg-z.b)
        (mm0)                           ;mmx temps
        (mm1)
        (imm0 imm0.q imm0.l imm0.b)     ;raw
        (imm1 imm1.q imm1.l imm1.b) 
        (nargs)                         ;(imm2 imm2.q imm2.l imm2.b) 
        (fn) ;current function pointer, needed to access its constants
        (rip)                    ;PC   (program counter, not used yet)
        (rbp)                    ;FP   (frame pointer)
        (rsp)                    ;SP   (stack pointer)
        (temp0 temp0.q temp0.l temp0.b)
        (temp1 temp1.q temp1.l temp1.b)
        (temp2 temp2.q temp2.l temp2.b)
        (temp3 temp3.q temp3.l temp3.b)
        (temp4 temp4.q temp4.l temp4.b)
        (temp5 temp5.q temp5.l temp5.b)
        (temp6 temp6.q temp6.l temp6.b)
        (rcontext))) ; GS (general purpose segment register, only 16 bits - should make it the correct width)

(defun reg-pos->size (pos)
  "Returns the register's width given its position in register-names"
  (case pos
    ((0 1) 64)
    (2     32)
    (3      8)))

) ;</eval-when>


(defun initialize-go-thunk-to-lisp-fun (fun-name)
  "Returns a list of 'instructions' that creates a go-thunk and puts it in the function cell of the symbol in simulated memory"
  `(setf (aref *simulated-memory* (symbol-function-cell (get ',fun-name 'sim-symbol-address)))
         (lambda () (go ,fun-name))))

(defun convert-result (x)
  "Converts simulator result to reader-friendly lisp result"
  (cond
    ((%fixnum-p x) (%unbox-fixnum x))
    ((%is-t x) t)
    ((%null x) nil)
    (t (error "nyi"))))

(defun %fixnum-p (x)
  "Returns T if x has fixnum tagbits"
  (= (mod x 8) 0)) ; low 3 bits = 000

(defun %is-t (x)
  "Returns T if t-value"
  (= x t-value))

(defun %null (x)
  "Returns T if nil-value"
  (= x nil-value))

(defun %unbox-fixnum (fixnum)
  "Arithmetic shift right by 3"
  (ash fixnum (- fixnumshift)))

;; accessing constants, registers, memory

(defun memory-ref (address)         ;a macro so SETF works
  "References the value stored at a quad-aligned byte address in simulated memory"
  (aref *simulated-memory* (%b2q address)))

(defun memory-set (address new)
  "Sets the addressed cell of simulated memory to a new value"
  (setf (aref *simulated-memory* (%b2q address)) new))

(defsetf memory-ref memory-set)

(defmacro % (register-name)
  "The register value"
  (multiple-value-bind (aliased-reg-name data-width)
      (alias-register register-name 'reg-pos->size)
    `(reg-value ',aliased-reg-name ,data-width)))

(defmacro @ (&rest mem-ref)
  "The value in simulated memory addressed by the register value + the offset"
  (declare (special *this-function*))
  (case (length mem-ref) ;mem-ref doesn't include the @
    (1 
      ;; absolute: (@ symbol)
      (cond ((atom (first mem-ref))
	     (sim-lookup-symbol (first mem-ref)))))
    (2
      ;; register with displacement (@ constant (% register))
     (let ((constant (cond ((or (atom (first mem-ref))
                                (not (eq 'quote (first (first mem-ref)))))
                            (eval-constant (first mem-ref)))  ;+++ should really integrate constant vector references
                           (t
                            (first mem-ref))))
	    (register-name (second (second mem-ref))))
	(cond ((integerp constant) ;assume not a bignum
	       `(memory-ref (+ ,constant (% ,register-name))))
	      ((eq register-name 'fn)
	       (let ((offset (local-constant-offset (second constant)))) ; ''my-expt => 'my-expt  ;how do we set this?
                 `(memory-ref (+ ,offset (% ,register-name)))))
	      ;; Constant can be any Lisp object if the register is FN
	      ;; it this case, it is essentially an assembler directive:
	      ;; a) allocate a slot in the function's constants vector
	      ;; b) insert the constant, and c) return the integer offset
	      )))))


;;; asm instructions

(defmacro def-asm-instruction (name data-width arglist &body body)
  "Defines an x86 instruction"
  (declare (ignore data-width))
  `(defmacro ,name ,arglist
     ,@body))

(def-asm-instruction UNBOX-FIXNUM 64 (src dest)
  "Unboxes the fixnum"
  `(%SPLICE
     (mov ,src ,dest)
     (sar ($ fixnumshift) ,dest)))

(def-asm-instruction recover-fn-from-rip 64 ()
  "Our (cheat) version puts the current function's constants vector address in fn"
  ;; It is important to derive function -> %FN.  That's where it finds the function's constants vector
  `(movq ($ (- (get ',*this-function* 'constants-vector-address) fulltag-function)) (% fn))  ;Cheat until every label is associated with an address, LEAQ works; seems the function address put in %fn is untagged
  ;;;;;;;
  #+LATER  
  (let ((label (gentemp "LABEL-")))
    `(%SPLICE
      (leaq (@ (:apply - (^ ,label)) (% rip)) (% fn))
      ,label))                 ;this is the only asm-instruction macro that generates a label
  )

(def-asm-instruction :align 0 (bytes)
  (declare (ignore bytes))
  ;; NOP
  )

(def-asm-instruction :talign 0 (bytes)
  (declare (ignore bytes))
  ;; NOP
  )

(def-asm-instruction ADDQ 64 (src1 src2-dest)
  "Add"
  `(setf ,src2-dest (%addq ,src1 ,src2-dest)))

(def-asm-instruction SUBQ 64 (src1 src2-dest)
  "Subtract"
  `(setf ,src2-dest (%subq ,src1 ,src2-dest)))

(def-asm-instruction CMPB 8 (arg1 arg2)
  "Compare"
  `(setf flags (%cmpb ,arg1 ,arg2)))

(def-asm-instruction CMPL 32 (arg1 arg2)
  "Compare"
  `(setf flags (%cmpl ,arg1 ,arg2)))

(def-asm-instruction CMPQ 64 (arg1 arg2)
  "Compare"
  `(setf flags (%cmpq ,arg1 ,arg2)))

(def-asm-instruction RCMPQ 64 (arg1 arg2)
  "Reverse compare"
  `(setf flags (%cmpq ,arg2 ,arg1)))
 

(def-asm-instruction IMUL 64 (src)   ;really 64->128
  "Signed multiply"
  `(multiple-value-bind (highq lowq)
       (%imul ,src (% imm0))
     (setf (% imm1) highq)
     (setf (% imm0) lowq)))
     ;;Wishful: (setf (%128 imm1 imm0) (%imul ,src (% imm0)))

;;; Branches

(def-asm-instruction JE :address (label)
  "Jump if equal"
  `(when (zero-flag-p flags)
     (go ,(strip-label label)))) ;+++ doesn't deal with (^ label) syntax

;;(def-asm-alias JMPQ jmp)  ;these are not the same instruction (or so I think)

(def-asm-instruction JMPQ :address (address)
  "Jump (absolute)"
  (let ((offset (second address))   ;(@ integer (% register))
        (base (third address)))     ;register contains a tagged-pointer to a symbol, offset points to function cell
    `(funcall (memory-ref (+ ,offset ,base))))) ; which contains go-thunk
  
(def-asm-instruction JMP :address (address-or-label)
  "Jump"
  (let ((type (car address-or-label)))
    (case type
      (^ 
       `(go ,(cadr address-or-label)))      ;(cadr x) = (second x) e.g.  (^ label) then cadr of that is label
      (@
       (case (length address-or-label)
	 (2
	  `(go ,(cadr address-or-label)))
	 (3
	  `(jmpq ,address-or-label)))))))

(def-asm-alias jz jne)

(def-asm-instruction JNE :address (label)
  "Jump if not equal"
  `(unless (zero-flag-p flags)
     (go ,(strip-label label))))

(def-asm-instruction JNO :address (label)
  "Jump if no overflow"
  `(unless (overflow-flag-p flags)
     (go ,(strip-label label))))

(def-asm-instruction JO :address (label)
  "Jump if overflow"
  `(when (overflow-flag-p flags)
     (go ,(strip-label label))))

(def-asm-instruction JA :address (label)
  "Jump above"
  `(when (and (carry-flag-p flags) (zero-flag-p flags))
    (go ,(strip-label label))))

(def-asm-instruction JUMP-BUILTIN :address (builtin arg-count) ; appears in subprims when calling out to the general case
  "Jump to builtin"
  (declare (ignore arg-count))
  `(error "jump-builtin NYI - ~a" ',builtin))

(def-asm-alias LEA leaq)

(def-asm-instruction LEAQ :address (addr dest)
  "Load effective address"
  `(setf ,dest (%leaq ,addr))) ;this will not work with labels

(def-asm-instruction LISP-CALL :address (address-or-label)
  "Call lisp function of subprim"
  `(call ,address-or-label))

(def-asm-instruction MOV :variable (src dest)         ;note no width +++ reconcile these
  "Move"
  `(setf ,dest ,src))           ;careful about width of dest

(def-asm-instruction MOVB 8 (src dest)
  "Move"
  `(setf ,dest ,src))           ;careful about width of dest

(def-asm-alias MOVD MOVL)

(def-asm-instruction MOVL 32 (src dest)
  "Move"
  `(setf ,dest ,src))           ;careful about width of dest

(def-asm-instruction MOVQ 64 (src dest)
  "Move"
  `(setf ,dest ,src))

(def-asm-instruction ANDB 8 (src1 src-dest)
  "And"
  `(setf ,src-dest (%andb ,src1 ,src-dest)))

(def-asm-instruction ANDL 32 (src1 src-dest)
  "And"
  `(setf ,src-dest (%andl ,src1 ,src-dest)))

(def-asm-instruction ORB 8 (src1 src-dest)
  "Or"
  `(setf ,src-dest (%xorb ,src1 ,src-dest))) ;careful about width of dest

(def-asm-instruction PUSHQ 64 (src)
  "Push"
  `(%splice
     (decf (% rsp) 8) ;grows down
     (setf (aref *simulated-memory* (%b2q (% rsp))) ,src)))          ;push

(def-asm-instruction POPQ 64 (dest)
  "Pop"
  `(%splice
     (setf ,dest (aref *simulated-memory* (%b2q (% rsp))))           ;pop
     (incf (% rsp) 8)))   ; grows down

(def-asm-instruction CALL 64 (address-or-label)
  "Call (branch and save return address)"
  (let ((return-label (gentemp "RETURN-")))
    ;;(pushq (% rip))
    `(%SPLICE
       (pushq (lambda () (go ,return-label)))  ;;MUDDY
       (jmp ,address-or-label)
       ,return-label))) ; the return labels are getting lost - we need them to be tags

#|
/*  On AMD hardware (at least), a one-byte RET instruction should be */
/*  prefixed with a REP prefix if it (a) is the target of a  */
/*  branch or (b) immediately follows a conditional branch not taken. */
define(`repret',`
        --(.byte 0xf3)
        --(ret)
')
|#

(def-asm-alias REPRET RET)

(def-asm-alias RETQ RET)

(def-asm-instruction RET :address ()
  "Return"
  ;;(popq (% temp)) ;internal, hidden register
  ;;(jmp (% temp)) ;won't work until we simulate program memory, wishing for call-cc;
  ;;Call pushed (lambda () (go return-label)) onto stack
  `(let (temp)
     (popq temp)      ;can this be a local var?  ;+++ MUDDY +++
     (funcall temp)))
  
(def-asm-instruction SAR :variable (shift src-dest)
  "Shift arithmetic right"
  `(setf ,src-dest (%sar ,shift ,src-dest)))

(def-asm-instruction SARQ 64 (shift src-dest)
  "Shift arithmetic right"
  `(setf ,src-dest (%sar ,shift ,src-dest)))

(def-asm-instruction SHLQ 64 (shift src-dest)
  "Shift logical left"
  `(setf ,src-dest (%shl ,shift ,src-dest)))

(def-asm-instruction TESTB 8 (mask src)
  "Test"
  `(setf flags (%testb ,mask ,src)))

(def-asm-instruction TESTL 32 (mask src)
  "Test"
  `(setf flags (%testl ,mask ,src)))

(def-asm-instruction TESTQ 64 (mask src)
  "Test"
  `(setf flags (%testq ,mask ,src)))

(def-asm-instruction CMOVEL 32 (src dest)
  "Conditional move"
  `(when (zero-flag-p flags)
     (movl ,src ,dest)))

(def-asm-instruction XORL 32 (arg src-dest)
  "Exclusive or"
  `(setf ,src-dest (%xorl ,arg ,src-dest)))

;;(def-asm-instruction unbox-fixnum 64 (src dest)
;;  `(setf ,dest (ash ,src (- tag-size))))

(def-asm-instruction uuo-error-wrong-number-of-args :variable ()
  ;; nop
  )

(def-asm-instruction uuo-alloc :variable ()
  ;; nop
  )

(def-asm-instruction uuo-error-reg-not-list :variable (src)
  (declare (ignore src))
  ;; nop
  )

(def-asm-instruction Misc-Alloc-Fixed 64 (dest size)
  (declare (ignore dest size))
  ;; NYI
  )

(defun %testq (mask src)
  (let ((temp (logand mask src)))
    (setf (zero-flag-bit flags) (bool2bit (zerop temp)))
    flags))

(defun %testl (mask src)
  (let ((temp (logand mask src)))                       ;+++ mask low half-word
    (setf (zero-flag-bit flags) (bool2bit (zerop temp)))
    flags))

(defun %testb (mask src)
  (let ((temp (logand (logand mask #xff) src)))         ;+++ mask low order byte
    (setf (zero-flag-bit flags) (bool2bit (zerop temp)))
    flags))

(defun %cmpq (arg1 arg2)
  (let ((temp (- arg1 arg2)))
    (set-flags! temp 64)
    flags))

(defun %cmpl (arg1 arg2)
  (let ((temp (- (logand arg1 #xffffffff) (logand arg2 #xffffffff)))) ;+++ make this sign-extend
    (set-flags! temp 32)
    flags))

(defun %cmpb (arg1 arg2)
  (let ((temp (- (logand arg1 #xff) (logand arg2 #xff)))) ;+++ make this sign-extend
    (set-flags! temp 8)
    flags))

(defun %xorl (arg src-dest)
  (logxor arg src-dest)
  ;;set flags
  )

(defun %sar (shift src-dest)
  (ash src-dest (- shift)) ;shift right
  ;;set flags
  )

(defun %shl (shift src-dest)
  (ash src-dest shift) ;shift left
  ;;set flags
  )

(defun %andb (arg src-dest) ; doesn't clear the high bits
  (logand (logior #xFFFFFFFFFFFFFF00 arg) src-dest) 
  ;;set flags
  )

(defun %andl (arg src-dest)
  (logand arg src-dest)
  ;;set flags
  )

(defun %xorb (arg src-dest)
  (logxor arg src-dest)
  ;;set flags
  )

(defun %leaq (addr)
  (declare (ignore addr))
  ;; this is gonna be tough for labels until we have explicit memory model
  )

(defun %imul (src implicit)  ;implicit arg: (% imm0), implicit dest(s): (% imm0) (% imm1)
  (let ((product (* src implicit)))
    ;;testing for overflow will cons if we're not very careful
    (values 0 product))) ;+++ wrong in > 64-bit result case (what's exact def of "overflow"?)


(defun %addq (src1 src2-dest)
  (let ((sum (+ src1 src2-dest)))
    (set-flags! sum 64)
    sum))

(defun %subq (src1 src2-dest)
  (let ((diff (- src2-dest src1)))
    (set-flags! diff 64)
    diff))
