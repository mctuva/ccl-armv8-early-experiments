;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SARM; Base: 10 -*-

(in-package :SAV8)

;; Revision History:
;; w0803 - Ayla - everything compiles, working through alignment bugs in list2
;; w0804 - Ayla - added instructions for the expt example
;; w0805 - Tim  - changed push2/pop2 and stp/ldp to ensure memory/order consistency
;; w0808 - Tim  - beginnings of documentation, no changes to the code
;; w0816 - Ayla - restored documentation that had been lost to inconsistent version history
;;                added function objects
;; w0821 - Tim  - More work to create 'as' file
;; w0822 - Tim  - label everything
;; w0823 - Tim  - make it possible to print label+tag, point to header of fn object not +1
;;                fixed 1+ offset for initializing function object at runtime
;;                added tag .equ's, added tag to T and NIL references
;; w0829 - Tim  - init_and_call_expt for demo
;; w0904 - Tim  - .ORG ..._base (backwards) labels, init TCR allocbase correctly
;;                generalize emit-main-to-as to emit several callable init/call functions
;; w0904 - Tim  - Allocated a NIL register and T register, translate constant to reg. ref.
;; w0905 - Tim  - Stubbed out sav8::jump-builtin to avoid compiler warnings in 'as' output

(EVAL-WHEN (compile load eval)

(defconstant fixnummask 7)
(defconstant fixnumshift 3)

(defconstant tag-size 3)
(defconstant ntagbits 4)
(defconstant tagmask #xf)

(defun %addr-strip-tag (addr)
  (values (ash (ash addr (- ntagbits)) ntagbits)
          (logand addr tagmask)))

(defvar *tag-names* (make-array 16))

(eval-when (compile load eval)
(defun underscorize-symbol (sym)
  "Replace dashes for underscores to support 'as' syntax"
  (substitute #\_ #\- (string-downcase (string sym))))  ;not making a new symbol
) ;</eval-when>

(defmacro deftag (tag-name tag-val)
  `(progn
     (defconstant ,tag-name ,tag-val)
     (setf (aref *tag-names* ,tag-val) ,(underscorize-symbol tag-name)))) ;implicit eval-when (load eval)

;; copying x86 tags
(deftag fulltag-even-fixnum 0)
(deftag fulltag-imm-0 1)
(deftag fulltag-imm-1 2)
(deftag fulltag-cons 3)
(deftag fulltag-tra-0 4)         ; tagged return address
(deftag fulltag-nodeheader-0 5)  ; random uvectors (eg. strings) + [new] function objects
(deftag fulltag-nodeheader-1 6)
(deftag fulltag-immheader-0 7)
(deftag fulltag-odd-fixnum 8)
(deftag fulltag-immheader-1 9)
(deftag fulltag-immheader-2 10)
(deftag fulltag-nil 11)
(deftag fulltag-tra-1 12)
(deftag fulltag-misc 13)
(deftag fulltag-symbol 14)
(deftag fulltag-function 15)

(defun %tag2tag-name (tag)
  (aref *tag-names* tag))

(defconstant node-size 8)

(defconstant fn-tag-offset (+ node-size (- fulltag-nodeheader-0))) ;[new] tagged pointer to header -> untagged addr to first slot

#+OLD
(defconstant fn-tag-offset (- fulltag-nodeheader-0))   ;+++ change this once function pointer points to header not 1st slot

;; +++ figure out why NIL and T symbol structures are overlapping
(defconstant nil-value (+ #x13000 fulltag-nil))
(defconstant t-value (+ #x13020 fulltag-symbol))
(defconstant t-offset (- t-value nil-value))

(defparameter *symbol-table* (make-hash-table))
(defparameter *address-table* (make-hash-table))  ;maps from addresses to objects (e.g. Lisp symbols for symbol table)

(defparameter *tcr-address* #x10000)
(defparameter *runtime-objects-base* #x11000) ;+++ move
(defparameter *stack-base* *simulated-memory-size*)  ;This was supposed to be the top of memory, but it's at #x20000 byte-addr
(defparameter *value-stack-base* #x5000) ;+++ too low, must be > #x10000
(defvar *allocbase* #x6000)              ;+++ too low, must be > #x10000


;; register names

#|
register conventions:

R0        return value
R0-R7	  arguments in function calls 
R8        indirect result location register
R9-R15	  temporary registers (trashed across calls)
R16, R17  aka IP0, IP1; intra-procedure-call temporary registers - the linker may use these in PLT code
R18	  platform register
R19-R28   callee saved registers - preserved across calls
R29       aka FP, copy of SP before function stack allocation
R30       aka LR, link register; BL and BLR instructions save return address in it
SP        stack pointer
ZR        zero register

|#

(setf (get :arm64 :register-names)
      '((arg-z arg-z.w)  ; r0 (return value)
	(arg-y arg-y.w)  ; r1
	(arg-x arg-x.w)  ; r2
	
	(imm0 imm0.w)    ; r9
	(imm1 imm1.w)    ; r10
	(nargs imm2 imm2.w)  ; r11 (assume nargs is w11)
	(temp0 temp0.w)  ; r12
	(fname temp1 temp1.w)  ; r13  fname
	(nfn temp2 temp2.w)  ; r14  nfn
	(temp3 temp3.w)  ; r15
	;; temp4...temp6
	
	(fn)             ; r19 (callee saved)
	(vsp)            ; r20
        (tsp)            ; r21 
	(rcontext)
	(allocptr)
	(allocbase)
	
	(fp)             ; r29
	(lr)             ; r30
	(sp)
	(zr)
	))

) ;eval-when

;; registers in arm that are not in x86:
;; nfn, fname, allocbase, allocptr, vsp

;; registers with .w suffix are 32 bit (but we don't actually differentiate between 64 and 32

(defparameter *dumped-memory-already* nil)

(defmacro def-simulator-code (name &key lispfuns builtins)
  "Top level simulator"
  (let* ((procedures (append lispfuns builtins))
         (main-name (first procedures))
         (expansions (mapcar #'symbol-value procedures))
         (call-main (macroexpand-lap-tree `(bl ,main-name)))
         (flattened (flatten-lap-tree `(%SPLICE
					 (mov (% nargs) ($ 0))
					 (ldr (% fname) (get ',main-name 'sim-symbol-address)) ;; offset from header (may be an issue in other parts too)
					 (ldr (% nfn) (@ (% fname) ($ 10)))
                                         ,call-main
                                         (return-from ,name (convert-result (% arg-z))) ; imm0?
                                         ,@expansions))))
    ;;(unless *dumped-memory-already*
    ;;  (init-simulator lispfuns)         ;mexp-time ;just run the examples, does fuller init anyway
    ;;  (setq *dumped-memory-already* t))
    ;;(dump-memory-to-as)   ;mexp-time ;+++ we don't pass a STREAM here yet
    `(defun ,name ()
       (init-simulator ',lispfuns) ;init stack pointer and fill simulated memory with constant vectors and "trampoline" symbols
       (tagbody
	  ,@(loop for fun-name in lispfuns
                  collect (initialize-go-thunk-to-lisp-fun fun-name)) ;just like mapcar now
          ,@flattened))))

;;;

(defun convert-result (x)
  "Converts simulator result to reader-friendly lisp result"
  (cond
    ((%fixnum-p x) (%unbox-fixnum x))
    ((%is-t x) t)
    ((%null x) nil)
    (t (error "nyi"))))

(defun %fixnum-p (x) ; fixnum tag may be different in arm32
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

;;;

(defvar *region-number* 0)
(defvar *region-size*   1024) ;maybe later 4096 and aligned
(defvar *glued-to-label* nil)
(defvar *last-label* nil)

(defvar *emit-org* nil) ;nil = no absolute addresses

(defvar *quiet-as* nil)

(defun dump-memory-to-as (&optional (stream t) &aux (silent t) label emitted-data last-byte-addr-emitted) ;starts off needing an .org
  (when *quiet-as* (return-from dump-memory-to-as))
  "Dump simulated memory in a way that can be use to create 'as' statically initialized (and labeled) objects"
  ;;+++ emit definitions for t_value and nil_value -> .s
  (setq *region-number* 0) ;reset
  (let ((*print-case* :downcase))
    (dotimes (tagnum 16)
      (format stream "~&~8t.equ ~A, ~D" (aref *tag-names* tagnum) tagnum))
    (format stream "~&~%~8t.data")
    (dotimes (word-address *simulated-memory-size*)  ;i is counting in quadwords
      (setq emitted-data nil)
      (let ((contents (aref *simulated-memory* word-address))
            (byte-address (* 8 word-address)))
        (cond ((functionp contents)
               (setq contents (gethash contents *address-table*)))  ;deals with lexical closure -> it's name
              ((setq label (gethash contents *address-table*))
               (setq contents (label-encoding-to-label label))))
        (cond ((eq contents :uninit) ;when we run into :UNINIT, we still need to keep track of where we are
               (setq silent t)
               
               (when (setq label (gethash byte-address *address-table*))
                 ;; "Floating" label
                 (when *emit-org*
                   (format stream "~&~%~8t.org 0x~x" byte-address))
                 ;;(print *last-label*)
                 (unless (eq *last-label* :TCR)
                   (setq *glued-to-label* nil)) ;TCR object has "holes" in it
                 (emit-label-maybe-preceeded-by-region stream byte-address label))
               )
              (t
               (when silent
                 ;;(print last-byte-addr-emitted)
                 (cond (*emit-org*
                        (format stream "~&~%~8t.org 0x~x" byte-address))
                       ((eq *last-label* :TCR)
                        (format stream "~&~8t.zero ~D"
                                (- byte-address last-byte-addr-emitted node-size)))) ;off by "one"
                 ;;(print *last-label*)
                 (unless (eq *last-label* :TCR)
                   (setq *glued-to-label* nil)) ;TCR object has "holes" in it
                 (setq silent nil))
               (emit-label-maybe-preceeded-by-region stream byte-address)
               (setq emitted-data t)
               (setq last-byte-addr-emitted byte-address)
               ;;(print *glued-to-label*)
               (when *glued-to-label* ;don't emit data if it isn't "reachable" from below (all alloc regions labeled from above)
                 (cond ((stringp contents)
                        (let* ((len (length contents))
                               (truncated (subseq contents 0 (min 8 len))))
                          (format stream "~&~8t.ascii ~S" truncated)
                          (when (< len 8)
                            (format stream "~&~8t.zero ~D" (- 8 len))))) ;make sure we align the next quadword
                       (t
                        (cond ((eq contents t-value)
                               (format stream "~&~8t.xword ~A + fulltag_symbol" :t_value))
                              ((eq contents nil-value)
                               (format stream "~&~8t.xword ~A + fulltag_nil" :nil_value))
                              ((symbolp contents)
                               (format stream "~&~8t.xword ~A" (underscorize-symbol contents))) ;e.g. refs to code label
                              ((listp contents)
                               (format stream "~&~8t.xword ~A" (label-encoding-to-label contents)))
                              (t
                               (multiple-value-bind (address-wo-tag tag) (%addr-strip-tag contents)
                                 (let ((label (gethash address-wo-tag *address-table*)))
                                   (if label
                                       (format stream "~&~8t.xword ~A + ~A"
                                               (label-encoding-to-label label)
                                               (%tag2tag-name tag))
                                       (format stream "~&~8t.xword 0x~x" contents))))))))))))
      (setq *glued-to-label* (cl:and *glued-to-label*
                                     (or (eq *last-label* :TCR) ;allowed to have "holes" in it
                                         emitted-data)))))
  ;; Finishing touch
  (emit-main-to-as stream))

(defun emit-label-maybe-preceeded-by-region (stream byte-address &optional label &aux switched)
  "Before emitting a label with 'base' in its name, allocate a 'region' object in the .bss section"
  (when (or label
            (setq label (gethash byte-address *address-table*)))
    (let ((ulabel (underscorize-symbol (label-encoding-to-label label)))
          (limit-label (gethash (- byte-address *region-size*) *address-table*)))
      (when limit-label
        (setq limit-label (underscorize-symbol limit-label)))
      (when (setq switched (cl:and (stringp ulabel)
                                   (or (search "base" ulabel)
                                       (search "BASE" ulabel))))
        (let ((region-name (or limit-label
                               (format nil "alloc_region_~d" (incf *region-number*)))))
          (format stream "~&~%~8t.bss~
                            ~&~8t.align  4~
                            ~&~A:~
                            ~&~8t.type   ~A, %object~
                            ~&~8t.size   ~A, ~D~
                            ~&~8t.org ~A + ~D"
                  region-name ;label
                  region-name ;type
                  region-name *region-size* ;size
                  region-name *region-size*))) ;org
      ;;"create" a static region for a stack or alloc region
      (setq *last-label* label)
      (unless (cl:and (stringp ulabel)
                      (or (search "limit" ulabel)
                          (search "LIMIT" ulabel))) ;special label
        (format stream "~&~A:" ulabel))
      (when switched
        ;;switch back
        (format stream "~&~8t.data~%~8t.align 4")))  ;(w0904) Force 16-byte alignment
    (setq *glued-to-label* t)))

(defun label-encoding-to-label (label)
  "Converts (symbol <sym>) and (function <sym>) to unique labels"
  (if (symbolp label)
      label
      (underscorize-symbol  ;+++ could be more efficient (like don't bother interning)
       (intern (concatenate 'string (string (cadr label))
                            (case (car label)
                              (symbol "_symbol")
                              (function "_function")))
               :keyword))))
  
(defparameter *init-statics-already* nil)

(defun init-simulator (lisp-fun-names)
  "Clears simulated memory and the register hashtable, allocates symbols and the TCR in memory, and sets values of prefilled registers."
  (unless *init-statics-already*
    (dotimes (i *simulated-memory-size*)
      (setf (aref *simulated-memory* i) :uninit)))
  (setf *registers* (if *registers* (clrhash *registers*) (make-hash-table)))
  (when (zerop *simulated-memory-pointer*) ;don't init twice to the same address
    (setq *simulated-memory-pointer* *runtime-objects-base*)) ;;(%b2q #x4000)) ; was 0  ;this is wrong, can't write to page zero

  ;; initialize memory
  (unless *init-statics-already*
    (initialize-tcr) ;this implicitly initializes *simulated-memory-pointer* to 0x10000
    (initialize-nil)
    (initialize-t)
    (setq *init-statics-already* t))
  (mapc #'allocate-symbol lisp-fun-names)
  (mapc #'local-constants-table lisp-fun-names)
  (mapc #'allocate-function-object lisp-fun-names)
  ;;(mapc #'allocate-constants-table lisp-fun-names)
  ;; initialize stack pointer
  (setf (gethash *allocbase* *address-table*) 'alloc-base)
  (setf (gethash (- *allocbase* *region-size*) *address-table*) 'alloc-limit) ;hack for alloc_limit
  (setf (gethash *runtime-objects-base* *address-table*) 'runtime-objects-base)
  (setf (gethash *stack-base* *address-table*) 'stack-base)
  (setf (gethash *value-stack-base* *address-table*) 'value-stack-base)
  (setf (reg-value 'allocbase) *allocbase*)   ;the arm64, arm32, and PPC all put these two into ;+++ why not open-coded in A64?
  (setf (reg-value 'allocptr) *allocbase*)   ; registers.  The x86-64 does not.  It keeps referencing the TCR
  (setf (reg-value 'rcontext) *tcr-address*)
  (setf (reg-value 'sp) *stack-base*)
  (setf (reg-value 'vsp) *value-stack-base*)
  )

#+REFERENCE
(def-asm-fun .init-simulator (:architecture :ARM64)
  (%splice
    ;; +++ Save fp and lr
    ;;No: (mov (% allocbase) ($ *allocbase*))   ; is this right? never used in open code
    (mov (% allocptr)  ($ *allocbase*))   ; copy from tcr slot?
    ;; mov x27, alloc_base
    (mov (% rcontext)  ($ *tcr-address*))
    ;; mov x25, tcr
    (mov (% sp)        ($ *stack-base*))
    ;; Hmm, maybe we don't initialize SP
    (mov (% vsp)       ($ *value-stack-base*))
    ;; mov x28, value_stack_base
    ;; +++ Save fp and lr ;no need
    (bl expt_caller_2_3) ;no! tail call
    ;; b expt_caller_2_3
    ;; the following is all unnecessary plus we don't have a fn object ourselves
#|
    (ldr (% fname) (@ (% fn) 'EXPT-CALLER-2-3))         ;k symbol
    (ldr (% nfn) (@ (% fname) ($ 10)))          ;symbol-function
    (ldr (% temp0) (@ (% nfn) ($ fn-tag-offset))) ;code-vector
    (blr (% temp0))             ; tail-call                   

;; Save restore FP, LR
;;       stp x29, x30, [sp, -16]!
;;       ldp x29, x30, [sp], 16

|#
    ))

;; (w0827) Note: Had to use LDR instruction to initialize x11 (nfn), MOV silently truncates
(defun emit-main-to-as (stream)
  "emit various init/call functions callable from 'main'"
  (emit-caller (underscorize-symbol 'init-and-call-expt)
               (underscorize-symbol 'my-expt)
               2
               stream)
  (emit-caller (underscorize-symbol 'init-and-call-list2)
               (underscorize-symbol 'call-list2)
               0
               stream)
  (format t "~&~%/* main() is commented-out by default to allow linking with expt.c or list2.c~%")
  (emit-caller (underscorize-symbol 'main)
               (underscorize-symbol 'expt-caller-2-3)
               0
               stream)
  (format t "~&EOF */~%")
  )

(defun emit-caller (name callee nargs stream)
  "initialize registers allocptr, rcontext, vsp, and nfn, then tail call my_expt with 2 args from caller"
  (format stream "

        .text
	.align	2
	.global	~A
	.type	~A, %function

~A:
	.cfi_startproc
        adrp x27, alloc_base
	add x27, x27, :lo12:alloc_base

        adrp x25, tcr
	add x25, x25, :lo12:tcr

        adrp x28, value_stack_base
	add  x28, x28, :lo12:value_stack_base

        adrp x11, ~A_function
	add  x11, x11, :lo12:~A_function
	add  x11, x11, fulltag_nodeheader_0

        adrp x23, nil_value
        add  x23, x23, :lo12:nil_value
        add  x23, x23, fulltag_nil

        adrp x24, t_value
        add  x24, x24, :lo12:t_value
        add  x24, x24, fulltag_symbol

        mov x5, ~D
        b ~A
	.cfi_endproc

	.size	~A, .-~A~%"
          name name name callee callee (* nargs node-size) callee name name))

#+ORIGINAL--OBSOLETE
(defun emit-main-to-as (stream)
  "initialize registers allocptr, rcontext, vsp, and nfn, then tail call"
  (format stream "

        .data
ec23fn: .xword (expt_caller_2_3_function + fulltag_nodeheader_0)

        .text
	.align	2
	.global	main
	.type	main, %function
main:
	.cfi_startproc
        adrp x27, alloc_base
	add x27, x27, :lo12:alloc_base

        adrp x25, tcr
	add x25, x25, :lo12:tcr

        adrp x28, value_stack_base
	add x28, x28, :lo12:value_stack_base

        adrp x11, expt_caller_2_3_function
	add x11, x11, :lo12:expt_caller_2_3_function
	add x11, x11, fulltag_nodeheader_0

        mov x5, 0
        b expt_caller_2_3
	.cfi_endproc

	.size	main, .-main
"))

#+PRE-FUNCTION-OBJECT
(defun initialize-go-thunk-to-lisp-fun (fun-name)
  ;; return a list of "instructions" that creates a go-thunk and puts it in the function cell of the "fake symbol" in simulated memory
  `(setf (aref *simulated-memory* (symbol-function-cell (get ',fun-name 'sim-symbol-address)))
         (lambda () (go ,fun-name))))

(defun initialize-go-thunk-to-lisp-fun (fun-name)
  "Returns a list of 'instructions' that creates a go-thunk and puts it in the function cell of the symbol in simulated memory"
  `(%splice
     (setf (aref *simulated-memory* (1+ (%tagged-b2q (get ',fun-name 'code-vector-address) fulltag-nodeheader-0))) ;%tagged-b2q strips tag
           (posit-and-return-closure (lambda () (go ,fun-name)) ',fun-name))                                       ;first slot!
     (setf (aref *simulated-memory* (symbol-function-cell (get ',fun-name 'sim-symbol-address)))
           (get ',fun-name 'code-vector-address)))) ;this is tagged pointer

#+OLD
(defun initialize-go-thunk-to-lisp-fun (fun-name)
  "Returns a list of 'instructions' that creates a go-thunk and puts it in the function cell of the symbol in simulated memory"
  `(%splice
     (setf (aref *simulated-memory* (%tagged-b2q (get ',fun-name 'code-vector-address) fulltag-nodeheader-0)) ;%tagged-b2q strips tag
           (posit-and-return-closure (lambda () (go ,fun-name)) ',fun-name))
     (setf (aref *simulated-memory* (symbol-function-cell (get ',fun-name 'sim-symbol-address)))
           (get ',fun-name 'code-vector-address)))) ;this is tagged pointer

(defun posit-and-return-closure (thunk fun-name)
  "Puts the closure in the address table so we can substitute the fun-name label when we dump memory to 'as'"
  (setf (gethash thunk *address-table*) fun-name) ;was `(function ,fun-name))
  thunk) ;must return thunk

;;; memory allocation

(defun memory-ref (address)
  "References the value stored at a quad-aligned byte address in simulated memory"
  (aref *simulated-memory* (%b2q address)))

(defun memory-set (address new)
  "Sets the addressed cell of simulated memory to a new value"
  (setf (aref *simulated-memory* (%b2q address)) new))


(defsetf memory-ref memory-set)

(defun new-aligned-object (length)
  "Incrememnts the *simulated-memory-pointer* to the next doublenode-aligned slot after the new object"
  (prog1 
      (if (oddp *simulated-memory-pointer*)
	  (incf *simulated-memory-pointer*)
	  *simulated-memory-pointer*)
    (incf *simulated-memory-pointer* length)))

(defun initialize-tcr ()
  "Initializes the Thread Context Record (TCR), well, really just the slots we care about to make CONSing work"
  (setf (gethash 'rcontext *registers*) *tcr-address*)          ;+++ move to code
  (setf (gethash *tcr-address* *address-table*) :tcr)
  
  (setf (aref *simulated-memory* (%b2q *tcr-address*)) "The TCR")
  (setf (aref *simulated-memory* (%b2q (+ #xD8 *tcr-address*))) *allocbase*)   ;alloc pointer (supposed to be *allocptr*?)
  (setf (aref *simulated-memory* (%b2q (+ #xE0 *tcr-address*))) (- *allocbase* *region-size*)))  ;alloc base  (w0904) is the end when grown down
  ;; +++ set *simulated-memory-pointer*, +++ how big is the TCR?
(defun allocate-symbol (sym)
  "Allocates a new symbol in simulated memory"
  (let* ((structure-size 7)
	 (original-pointer (new-aligned-object (1+ structure-size)))
	 (tagged-address (%q2tagged-b original-pointer fulltag-symbol)))
    (setf (gethash sym *symbol-table*) original-pointer)
    (setf (gethash (%q2b original-pointer) *address-table*) `(symbol ,sym))
    (setf (get sym 'sim-symbol-address) tagged-address)
    (q-memory-set original-pointer "sym-header")
    (q-memory-set (symbol-pname tagged-address)              (symbol-name sym)) ; implement strings
    (q-memory-set (symbol-value-cell tagged-address)          nil-value)
    (q-memory-set (symbol-function-cell tagged-address)       nil-value)
    (q-memory-set (symbol-package-predicate tagged-address)   nil-value)
    (q-memory-set (symbol-flags tagged-address)               nil-value)
    (q-memory-set (symbol-plist-cell tagged-address)          nil-value)
    (q-memory-set (symbol-binding-index tagged-address)       nil-value)))

;; nil has its own tag and is both a symbol and a list so it has to be treated differently
(defun initialize-nil ()
  "Allocates the symbol structure of NIL in simulated memory"
  (let ((nil-ptr (%tagged-b2q nil-value fulltag-nil)))
    (setf (gethash (%q2b nil-ptr) *address-table*) :nil_value)
    (q-memory-set nil-ptr "NIL-header")
    (q-memory-set (+ nil-ptr 1) "NIL") ; pname
    (q-memory-set (+ nil-ptr 2) nil-value) ; vcell
    (q-memory-set (+ nil-ptr 3) nil-value) ; fcell
    (q-memory-set (+ nil-ptr 4) nil-value) ; pkg pred          ;everything including this slot and below get overwritten by T
    (q-memory-set (+ nil-ptr 5) nil-value) ; flags
    (q-memory-set (+ nil-ptr 6) nil-value) ; plist
    (q-memory-set (+ nil-ptr 7) nil-value))) ; binding index

(defun initialize-t ()
  "Allocates the symbol structure of T in simulated memory"
  (let ((t-ptr (%tagged-b2q t-value fulltag-symbol)))
    (setf (gethash (%q2b t-ptr) *address-table*) :t_value)
    (q-memory-set t-ptr "T-header")
    (q-memory-set (symbol-pname t-value) "T")
    (q-memory-set (symbol-value-cell t-value) t-value)
    (q-memory-set (symbol-function-cell t-value) nil-value)
    (q-memory-set (symbol-package-predicate t-value) nil-value)
    (q-memory-set (symbol-flags t-value) nil-value)
    (q-memory-set (symbol-plist-cell t-value) nil-value)
    (q-memory-set (symbol-binding-index t-value) nil-value)))
  
(defun q-memory-set (quad-address value)
  "Sets the cell of simulated memory at a quad address to a new value"
  (setf (aref *simulated-memory* quad-address) value))

;;; Symbol slot accessors into simulated memory

(defun symbol-pname (symbol-address)
  "Returns the simulated memory quad address of a symbol's name"
  (+ (%tagged-b2q symbol-address fulltag-symbol) 1))

(defun symbol-value-cell (symbol-address)
  "Returns the simulated memory quad address of a symbol's value cell"
  (+ (%tagged-b2q symbol-address fulltag-symbol) 2))

(defun symbol-function-cell (symbol-address)
  "Returns the simulated memory quad address of a symbol's function cell"
  (+ (%tagged-b2q symbol-address fulltag-symbol) 3))

(defun symbol-package-predicate (symbol-address)
   "Returns the simulated memory quad address of a symbol's package predicate"
  (+ (%tagged-b2q symbol-address fulltag-symbol) 4))

(defun symbol-flags (symbol-address)
  "Returns the simulated memory quad address of a symbol's flags"
  (+ (%tagged-b2q symbol-address fulltag-symbol) 5))

(defun symbol-plist-cell (symbol-address)
  "Returns the simulated memory quad address of a symbol's property list"
  (+ (%tagged-b2q symbol-address fulltag-symbol) 6))

(defun symbol-binding-index (symbol-address)
  "Returns the simulated memory quad address of a symbol's binding index"
  (+ (%tagged-b2q symbol-address fulltag-symbol) 7))

;;; function

(defun allocate-function-object (function-name)
  (let* ((constants-vector-length (%b2q (get function-name 'constants-table-index)))
	 (length (+ 1 constants-vector-length)) ; pointer to code + constants (+ in reality should also have the code vector)
	 (original-pointer (new-aligned-object (+ 1 length))))
    (setf (gethash (%q2b original-pointer) *address-table*) `(function ,function-name))
    (setf (aref *simulated-memory* original-pointer) "fn-header") ; (fake) header ;was: length (yes, var uvector must have length)
    (setf (aref *simulated-memory* (+ 1 original-pointer)) function-name)
    (fill-constants-vector function-name (+ 1 original-pointer))
    (setf (get function-name 'code-vector-address)
          (+ (%q2b original-pointer)       ; [new] point to header, not first slot
             fulltag-nodeheader-0))        ; Last minute addition: this is now a tagged pointer
    ))

#+OLD
(defun allocate-function-object (function-name)
  (let* ((constants-vector-length (%b2q (get function-name 'constants-table-index)))
	 (length (+ 1 constants-vector-length)) ; pointer to code + constants (+ in reality should also have the code vector)
	 (original-pointer (new-aligned-object (+ 1 length))))
    (setf (gethash (%q2b original-pointer) *address-table*) `(function ,function-name))
    (setf (aref *simulated-memory* original-pointer) "fn-header") ; (fake) header ;was: length (yes, var uvector must have length)
    (setf (aref *simulated-memory* (+ 1 original-pointer)) function-name)
    (fill-constants-vector function-name (+ 1 original-pointer))
    (setf (get function-name 'code-vector-address)
          (+ (%q2b (+ 1 original-pointer)) ; for now I'm setting this to point after the header but we may want to change this
             fulltag-nodeheader-0))        ;  (and "last minute addition" this is now a tagged pointer) -- this is the only 
;;                                         ;  place where we assume the pointer is to the first slot, not the header 
;;                                         ;  (symbol structures do)
    ))

(defun fill-constants-vector (function-name pointer)
  (let ((table (get function-name 'constants-table)))
    (loop for constant being the hash-key using (hash-value offset) of table
	  do (let ((index (+ pointer (%b2q offset)))) ; preserve constants order
	       (typecase constant
		 (symbol
		  (setf (aref *simulated-memory* index) (get constant 'sim-symbol-address)))
		 (otherwise
		  (error "NYI")))))
    (setf (get function-name 'constants-vector-address) (%q2b pointer))))

;;; function constants table

#+PRE-FUNCTION-OBJECT
(defun allocate-constants-table (function-name)
  "Allocates a function's constants-vector in simulated memory"
  (let* ((table (get function-name 'constants-table))
	 (length (hash-table-count table))
	 (original-pointer (new-aligned-object (1+ length))))
    (setf (aref *simulated-memory* original-pointer) length) ; 1st element is the header which gives the length ++A++ need accurate header
    (loop for constant being the hash-key using (hash-value offset) of table
	  do (let ((index (+ original-pointer (%b2q offset)))) ; preserve constants order
	       (typecase constant
		 (symbol
		  (setf (aref *simulated-memory* index) (get constant 'sim-symbol-address)))
		 (otherwise
		  (error "NYI")))))
    (setf (get function-name 'constants-vector-address)
          (%q2b original-pointer)))) ; no tag for now

(defun local-constants-table (function-name)
  "Allocates a hash table for each function that maps from values to constants-vector offsets"
  (let ((table (get function-name 'constants-table)))
    (unless table
      (setq table (make-hash-table :test 'equal))             ;that maps constants to offsets
      (setf (get function-name 'constants-table) table)      ;(note: hash tables, by default are "EQ hash tables")
      (setf (get function-name 'constants-table-index) 0)) ;the first index of the vector is for the header, not constants
    table))

#+UNUSED?
(defun lookup-local-constant (constant)
  (declare (special *this-function*))
  (let ((table (local-constants-table *this-function*)))
    (gethash constant table)))

(defun allocate-new-constant (constant)
  "Adds a constant to *this-function*'s constants table, with an associated offset"
  (declare (special *this-function*))
  (let* ((table (local-constants-table *this-function*))
	 (offset (+ node-size (get *this-function* 'constants-table-index)))) ;pre-scaled offset into the constants vector
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
          (+ offset node-size)  ;is this the right place to increment this? (now that pointers point to header instead of first slot)
          (allocate-new-constant constant)))))

;;;


(defmacro % (register-name)
  "The register value"
  (let ((reg (alias-register register-name)))
    `(reg-value ',reg)))


;;addressing mode: offset
(defmacro @ (reg offset)
  "The value in simulated memory addressed by the register value + the offset"
  (declare (special *this-function*))
  (cond
    ((eql '$ (first offset)) ; ex. ($ -3)
     `(memory-ref (+ ,reg ,offset)))
    ((eql 'quote (first offset)) ; ex. 'my-expt
     (let ((displacement (local-constant-offset (second offset)))) ; ''my-expt      ;this displacement doesn't have a tag or tag-stripper
       `(memory-ref (+ ,reg ,displacement (- fulltag-nodeheader-0)))       ;this strips, knowing tag type (fingers crossed)
       #+NOPE `(memory-ref (+ ,displacement
                              (get ',*this-function* 'constants-vector-address))))))) ;this is not explicit enough, didn't catch bug in fn contents
    
   
#|
addressing-modes                                         ;<--- Keep this sort of documentation
(ldr (% arg-z) (% arg-y))
(ldr (% arg-z) (@ (% arg-y) ($ 4)))
(ldr (% arg-z) (+! (% arg-y) ($ 4))) ;pre-index
(ldr (% arg-z) (!+ (% arg-y) ($ 4))) ;post-index
|#


(defmacro def-arm-instruction (name arglist &body body)
  "Defines an arm instruction"
  `(defmacro ,name ,arglist
     ,@body))

(def-arm-instruction STR (reg-src mem-dest)
  "store register"
  (case (first mem-dest)
    (+!
     (let ((mem-reg (second mem-dest))
	   (index (third mem-dest)))
       `(%splice
	  (add ,mem-reg ,mem-reg ,index)
	  (memory-set ,mem-reg ,reg-src))))
    (!+
     (let ((mem-reg (second mem-dest))
	   (index (third mem-dest)))
       `(%splice
	  (memory-set ,mem-reg ,reg-src)
	  (add ,mem-reg ,mem-reg ,index))))
    (%
     `(memory-set ,mem-dest ,reg-src))
    (@
     `(setf ,mem-dest ,reg-src))
    ))

(def-arm-instruction LDR (reg-dest mem-src)
  "load register"
  (case (first mem-src)
    (+!
     (let ((mem-reg (second mem-src))
	   (index (third mem-src)))
       `(%splice
	  (add ,mem-reg ,mem-reg ,index)
	  (setf ,reg-dest (memory-ref ,mem-reg)))))
    (!+
     (let ((mem-reg (second mem-src))
	   (index (third mem-src)))
       `(%splice
	  (setf ,reg-dest (memory-ref ,mem-reg))
	  (add ,mem-reg ,mem-reg ,index))))
    (%
     `(setf ,reg-dest (memory-ref ,mem-src)))
    (otherwise
     `(setf ,reg-dest ,mem-src))))

(def-arm-instruction STP (reg-src1 reg-src2 mem-dest)
  "store pair of registers"
  `(%splice
     (str ,reg-src1 ,mem-dest)
     (str ,reg-src2 (@ ,mem-dest ($ 8)))))    ;+++ the offset changes depending on the width of the register

(def-arm-instruction LDP  (reg-dest1 reg-dest2 mem-src)
  "load pair of registers"
  `(%splice                              ;the original didn't have the (@ r 8); it relied on two auto-increments
     (ldr ,reg-dest1 ,mem-src)           ;we are writing it this way to highlight the invariant memory order
     (ldr ,reg-dest2 (@ ,mem-src ($ 8)))))   ;+++ the offset changes depending on the width of the register

(def-arm-instruction PUSH1 (reg-src)
  "push to stack"
  `(str ,reg-src (+! (% sp) ($ -16)))) ;must be 16 byte aligned

(def-arm-instruction POP1 (reg-dest)
  "pop from stack"
  `(ldr ,reg-dest (!+ (% sp) ($ 16))))

(def-arm-instruction PUSH2 (reg-src1 reg-src2)
  "push 2 registers to stack"
  `(%splice (add (% sp) (% sp) ($ -16))          ;in reality this pre-decrement is done in the one instruction
            (stp ,reg-src1 ,reg-src2 (% sp))))

(def-arm-instruction POP2 (reg-dest1 reg-dest2)
  "pop 2 registers from stack"
  `(%splice (ldp ,reg-dest1 ,reg-dest2 (% sp))
            (add (% sp) (% sp) ($ 16))))         ;ditto for post-increment

(def-arm-instruction VPUSH1 (reg-src)
  "push to value stack"
  `(str ,reg-src (+! (% vsp) ($ -8))))

(def-arm-instruction VPUSH2 (reg-src1 reg-src2)
  "push 2 registers to value stack"
  `(%splice (add (% vsp) (% vsp) ($ -16))
            (stp ,reg-src1 ,reg-src2 (% vsp))))

(def-arm-instruction MOV (dest src)
  "move"
  `(setf ,dest, src))



(def-arm-instruction BR (address)
  "unconditional, absolute branch to (contents of) register"
  `(funcall ,address)) ;almost certainly won't work - make a call macro that parses the register

(def-arm-instruction BLR (address)
  "branch and link to register (absolute)"
  (let ((return-label (gensym "return-label-")))
    `(%splice
       (let ((branch-target ,address)) ; in case address is in register lr which gets over written
	 (setf (% lr) (lambda () (go ,return-label)))
	 (funcall branch-target))
       ,return-label)))


(def-arm-instruction B (label)
  "relative branch"
  `(go ,label))

(def-arm-instruction BL (label)
  "branch and link"
  (let ((return-label (gensym "return-label-")))
    `(%splice
       (setf (% lr) (lambda () (go ,return-label)))
       (go ,label)
       ,return-label)))

(def-arm-instruction BEQ (label)
  "branch if equal"
  `(when (zero-flag-p flags)
     (go ,label)))

(def-arm-instruction BNE (label)
  "branch if not equal"
  `(when (not (zero-flag-p flags))
     (go ,label)))

(def-arm-instruction BHI (label)
  "branch if unsigned higher"
  `(when (overflow-flag-p flags)
     (go ,label)))

(def-arm-instruction BVS (label) ; overflow
  "branch if overflow"
  `(when (carry-flag-p flags)
     (go ,label)))

(def-arm-instruction BVC (label) ; no overflow
  "branch if no overflow"
  `(when (not (carry-flag-p flags))
     (go ,label)))

(def-arm-instruction RET (&optional (return-address '(% lr)))
  "return"
  `(funcall ,return-address))



(def-arm-instruction CMP (rn op2) ; op2 is a register or literal
  "compare"
  `(setf flags (%cmpq ,rn ,op2)))

(def-arm-instruction AND (rd rn op2)
  "and"
  `(setf ,rd (%and ,rn ,op2)))

(def-arm-instruction ORR (rd rn op2)
  "or"
  `(setf ,rd (%orr ,rn ,op2)))

(def-arm-instruction TST (rn op2)
  "test"
  `(setf flags (%tst ,rn ,op2)))

(def-arm-instruction BIC (rd rn op2)
  "bit clear"
  `(setf ,rd (%bic ,rn ,op2)))



(def-arm-instruction ADD (rd rn op2)
  "add"
  `(setf ,rd (%addq ,rn ,op2)))

(def-arm-instruction ADDS (rd rn op2)
  "add and set flags"
  `(let ((sum (%addq ,rn ,op2)))
     (set-flags! sum 64)
     (setf ,rd sum)))

(def-arm-instruction SUB (rd rn op2)
  "subtract"
  `(setf ,rd (%subq ,rn ,op2)))

(def-arm-instruction SUBS (rd rn op2)
  "subtract and set flags"
  `(let ((diff (%subq ,rn ,op2)))
     (set-flags! diff 64) ; may be a problem if there are 32 bit instructions
     (setf ,rd diff)))

(def-arm-instruction SMULL (rd rn op2)    ;was accidentally named smulh
  "signed multiply long"
  `(setf ,rd (%smull ,rn ,op2)))


(def-arm-instruction unbox-fixnum (rd rn)
  "unbox fixnum (arithmetic shift right by 3)"
  `(asr ,rd ,rn ($ fixnumshift)))

(def-arm-instruction ASR (rd rn shift)
  "arithmetic shift right"
  `(setf ,rd (%asr ,rn ,shift)))


;; in armv7 sploadlr translates to ldr lr, [rcontext, subprim]
;; where subprim is the address of the of the subprim in the jump table
;; which is at the end of the tcr
;; Q: how does it work in x86 (there's also a jump table I believe...)
(def-arm-instruction SPLOADLR (subprim)
  `(ldr (% lr) ',subprim))

;;; Unimplemented instructions

(def-arm-instruction uuo-alloc-trap ()
  ;nop
  )

(def-arm-instruction uuo-error-wrong-nargs ()
  ;nop
  )

(def-arm-instruction uuo-error-reg-not-list (reg)
  (declare (ignore reg))
  ;nop
  )

(def-arm-instruction jump-builtin (target nargs)
  (declare (ignore target nargs))
  ;nyi
  )
           
;;; Building blocks for (above) instructions
;;; They aren't particularly accurate
;;; (e.g. w.r.t. sign extension and word size/trunation, but it doesn't really matter)

(defun %subq (op1 op2)
  (- op1 op2))

(defun %addq (op1 op2)
  (+ op1 op2))

(defun %smull (op1 op2)   ;was accidentally named %smulh
  (* op1 op2))

(defun %cmpq (arg1 arg2)
  (let ((temp (- arg1 arg2)))
    (set-flags! temp 64)
    flags))

(defun %bic (rn op2)
  (logand rn (lognot op2)))

(defun %tst (rn op2)
  (let ((temp (logand rn op2)))
    (setf (zero-flag-bit flags) (bool2bit (zerop temp)))
    flags))

(defun %and (arg src-dest)
  (logand arg src-dest)
  ;;set flags
  )

(defun %orr (rn op2)
  (logior rn op2))

(defun %asr (rn shift)
  (ash rn (- shift)) ;shift right
  )

;;;;;;;;;;;;;;;;;
;;;
;;; Debugging fun

(defun show-address-table ()
  "Display the *address-table* in a readable format, mapping from addresses and thunks to sim symbols and sim funs"
  (maphash (lambda (k v)
             (let ((*print-case* :downcase))
               (if (functionp k)
                   (format t "~&~s~&~12t~S" k v)
                   (format t "~&0x~x~12t~S" k v))))
           *address-table*))
