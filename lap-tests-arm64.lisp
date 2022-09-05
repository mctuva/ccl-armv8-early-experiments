;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SARM; Base: 10 -*-

(in-package :SAV8)

;;; Revision history
;; w0804 - Ayla - added the expt example - having issues with the recursion
;; w0805 - Ayla - fixed my-expt and spbuiltin-times so recursion works and the multiplication is somemewhat more accurate
;;                added verify-list and test-suite; the simulator is working
;; w0807 - Tim - arm-and -> and
;; w0810 - Ayla - annotated lap code; car and cdr are reversed in memory from x86 (may want to swap them)
;; w0811 - Ayla - swapped car/cdr in list2 and verify-list2
;; w0816 - Ayla - added (ldr (% nfn) (@ (% nfn) ($ 0)) before function calls to support new function objects - deserves more thought about what the pointer actually is and if we need to offset it
;; w0823 - Tim - Commented out reference functions
;; w0824 - Tim - fix nfn usage before call -> temp0
;; w0826 - Tim - de-dot, misc fixes that wouldn't assemble in 'as'
;; w0829 - Tim - smulh -> smull

(defun expt-caller-2-3 ()
  (my-expt 2 3))

(def-asm-lisp-fun EXPT-CALLER-2-3 (:architecture :ARM64)
  (%splice
    (cmp (% nargs) ($ 0))    ; by default let nargs be 32 bits (alias of register W11)
    (beq L49)
    (uuo-error-wrong-nargs)

    L49
    (push1 (% fp))           ; save old FP
    (mov (% fp) (% sp))      ; fp = sp; start of new frame
    (push1 (% vsp))          ; push vsp to new frame
    (push2 (% fn) (% lr))    ; push fn and lr to new frame
    (mov (% fn) (% nfn))   ;new (w0826)

    ;;; (my-expt 2 3)
    (mov (% arg-y.w) ($ 16)) ; arg-y = 2 (boxed fixnum, shifted left 3 bits)
    (mov (% arg-z.w) ($ 24)) ; arg-z = 3
    (mov (% nargs) ($ 16))   ; 2 arguments
    (ldr (% fname) (@ (% fn) 'MY-EXPT)) ;($ 11); load the tagged address of my-expt's symbol structure from expt-caller-2-3's constants vector
    (pop2 (% fn) (% lr))
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))            ; pop function frame (prep for tail-call)
    (ldr (% nfn) (@ (% fname) ($ 10))) ; load my-expt's function cell into nfn
    ;; may need to untag the function address before branching
    (ldr (% temp0) (@ (% nfn) ($ fn-tag-offset))) ; newly added line to make function object work
    (br (% temp0))             ; tail-call
    ))


(defun expt-caller-3-3 ()
  (my-expt 3 3))

#+MANANA
(def-asm-lisp-fun EXPT-CALLER-3-3 (:architecture :ARM64)
  (%splice
    (cmp (% nargs) ($ 0))
    (beq L49)
    (uuo-error-wrong-nargs)

    L49
    (push1 (% fp))
    (mov (% fp) (% sp))
    (push1 (% vsp))
    (push2 (% fn) (% lr))
    (mov (% fn) (% nfn))   ;new (w0826)

    ;;; (my-expt 3 3) -> 27
    (mov (% arg-y.w) ($ 24)) ;3
    (mov (% arg-z.w) ($ 24)) ;3
    (mov (% nargs) ($ 16))
    (ldr (% fname) (@ (% fn) 'MY-EXPT)) ;($ 11)
    (pop2 (% fn) (% lr))
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))
    (ldr (% nfn) (@ (% fname) ($ 10)))
    (ldr (% temp0) (@ (% nfn)  ($ fn-tag-offset))) ; newly added line to make function object work
    (br (% temp0)) ; tail-call
    ))


(defun my-expt (b n)
  (if (= n 0)
      1
      (* b (my-expt b (- n 1)))))

(def-asm-lisp-fun MY-EXPT (:architecture :ARM64)
  (%splice
    L0
    (cmp (% nargs) ($ 16))       ; 2 args?
    (beq L12)                    ; yes
    (uuo-error-wrong-nargs)      ; no

    L12
    (push1 (% fp))               ; set up function frame
    (mov (% fp) (% sp))
    (push1 (% vsp))
    (push2 (% fn) (% lr))
    (mov (% fn) (% nfn))
    (vpush2 (% arg-z) (% arg-y)) ; push args to value stack

    ;;; (= n 0)
    (tst (% arg-z.w) ($ 7))      ; fixnum?
    (bne L48)
    (cmp (% arg-z) ($ 0))        ; arg-z = 0?
    (beq L68)
    (b L76)

    L48                          ; case: n is not a fixnum
    (mov (% arg-y.w) ($ 0))      ; arg-y = 0
    (bl SPBUILTIN-EQ)           ; call out to subprim, set link register so we will return to next instruction (de-dot)
    (cmp (% arg-z) ($ nil-value)); did spbuiltin-eq return nil?
    (beq L76)

    ;;; (if (= n 0) 1 (* b (my-expt b (- n 1))))
    L68                          ; base case: n = 0
    (mov (% arg-z.w) ($ 8))      ; arg-z = 1
    (pop2 (% fn) (% lr))         ; frame cleanup
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))
    (ret)                        ; return to link register

    ;;; (- n 1)
    L76
    (ldr (% arg-y) (@ (% vsp) ($ 0)))  ; arg-y = n (second arg, top of vstack) ;(rc3) (@ (% vsp)) should asm to [vsp] but we don't understand no offset
    (tst (% arg-z.w) ($ 7))      ; fixnum?
    (bne L112)
    (subs (% arg-z) (% arg-z) ($ 8)) ; (- n 1), set flags
    (bvc L120)                   ; branch if no overflow
    (bl SPFIX-OVERFLOW)  ;de-dot
    (b L120)

    L112                         ; case: n is not a fixnum
    (bl SPBUILTIN-MINUS)     ;de-dot

    ;;; (my-expt b (- n 1))
    L120
    (ldr (% arg-y) (@ (% vsp) ($ 8))) ; arg-y = b (first arg, bottom of vstack)
    (mov (% nargs) ($ 16))
    (mov (% nfn) (% fn))         ; nfn = fn (next function = current function)
    (bl L0)                      ; recursive call

    ;;; (* b (my-expt b (- n 1)))
    (ldr (% arg-y) (@ (% vsp) ($ 8))) ; arg-y = b
    (pop2 (% fn) (% lr))              ; cleanup function frame (prep for tail-call)
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))
    (b SPBUILTIN-TIMES)              ; tail-call to multiplication subprim (de-dot)
    ))

#|
ARM32
_spentry(builtin_times)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(unbox_fixnum(imm2,arg_z))
        __(unbox_fixnum(imm0,arg_y))
	__(smull imm0,imm1,imm2,imm0)
        __(b _SPmakes64)


define(`test_two_fixnums',`
        __(orr $3,$1,$2)
        __(test_fixnum($3))
       ')

define(`test_fixnum',`
        __(tst $1,#fixnummask)
        ')

define(`unbox_fixnum',`	
	__(mov $1,$2, asr #fixnumshift)
	')

|#

(def-asm-fun SPBUILTIN-TIMES (:architecture :ARM64)  ;de-dot
  (%splice
    (orr (% imm0.w) (% arg-y.w) (% arg-z.w)) ; test_two_fixnums (if both are fixnums, or-ing the tagbits stays 0)
    (tst (% imm0.w) ($ fixnummask))          ; test_fixnum
    (bne L2-X)
    (unbox-fixnum (% imm0) (% arg-z))        ; imm0 = unboxed arg-z (shift right 3)
    (unbox-fixnum (% imm1) (% arg-y))        ; imm1 = unboxed arg-y
    (smull (% imm0) (% imm1.w) (% imm0.w))   ; signed multiply long (temporary placeholder for proper 64-bit multiply)
    (adds (% imm0) (% imm0) (% imm0))        ; double imm0 (re-boxing), set flags
    (bvs L1-X)                               ; branch if overflow
    (adds (% imm0) (% imm0) (% imm0))        ; re-boxing, set flags
    (bvs L1-X)                               ; branch if overflow
    (adds (% imm0) (% imm0) (% imm0))        ; re-boxing, set flags; imm0 is now a boxed fixnum
    (bvs L1-X)                               ; branch if overflow
    (mov (% arg-z) (% imm0))                 ; arg-z = imm0
    (ret)

    L1-X                                     ; case: overflow
    (unbox-fixnum (% imm0) (% arg-z))
    (unbox-fixnum (% imm1) (% arg-y))
    (smulh (% imm1) (% imm1) (% imm0))
    (b spmakes128) ; we won't reach this

    L2-X
    (jump-builtin builtin-times 2) ; we won't reach this
    ))

;;; subprims referenced but not used

(def-asm-fun SPBUILTIN-EQ (:architecture :ARM64) ;de-dot
  (%splice
    (ret)
    ))

(def-asm-fun SPFIX-OVERFLOW (:architecture :ARM64) ;de-dot
  (%splice
    (ret)
    ))

(def-asm-fun SPBUILTIN-MINUS (:architecture :ARM64) ;de-dot
  (%splice
    (ret)
    ))

(def-asm-fun SPMAKES128 (:architecture :ARM64) ;de-dot
  (%splice
    (ret)
    ))


;;; list example

(defun call-list2 ()
  (let ((x 1)
        (y 2))
    (let ((lst (list2 x y)))             ;Call LIST2, then test that:
      (verify-list2 lst x y))))

(def-asm-lisp-fun call-list2 (:architecture :ARM64)
  (%splice
    (cmp (% nargs) ($ 0))     ; check nargs
    (beq L12b)
    (uuo-error-wrong-nargs)

    L12b
    (push1 (% fp))            ; set up frame
    (mov (% fp) (% sp))
    (push1 (% vsp))
    (push2 (% fn) (% lr))
    (mov (% fn) (% nfn))      ; fn = nfn
                              ; prep for function call
    (mov (% arg-y.w) ($ 8))   ; 1
    (mov (% arg-z.w) ($ 16))  ; 2
    (mov (% nargs.w) ($ 16))  ; 2
    (ldr (% fname) (@ (% fn) 'list2)) ;($ 11); ; fname = list2 symbol structure address (loaded from fn's constants vector)
    (ldr (% nfn) (@ (% fname) ($ 10))) ; nfn = function cell (symbol tag = 14, point to header)
    (ldr (% temp0) (@ (% nfn) ($ fn-tag-offset))) ; newly added line to make function object work +++ This assumes nfn points to 1st slot
    (blr (% temp0))              ; function call (branch to register and set lr)
    (vpush1 (% arg-z))        ; save list2 return value before next function call
    (mov (% arg-x) (% arg-z)) ; arg-x = arg-z (tagged pointer to list (1 2)) ;(can't be .w!)
    (mov (% arg-y.w) ($ 8))   ; arg-y = 1
    (mov (% arg-z.w) ($ 16))  ; arg-z = 2
    (mov (% nargs) ($ 24))    ; nargs = 3
    (ldr (% fname) (@ (% fn) 'verify-list2)) ;($ 19); 16 - fulltag-nodeheader-0
    (pop2 (% fn) (% lr))
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))
    (ldr (% nfn) (@ (% fname) ($ 10))) ; nfn = tagged function address
    (ldr (% temp0) (@ (% nfn) ($ fn-tag-offset))) ; newly added line to make function object work
    (br (% temp0)) ; tail-call
    ))

;; Discussion: On the ARMv7, FN reg points to the beginning of the
;;     function object, which points to code vector,
;;     and the top of the code vector is the first instruction
;;     On the x86-64, the function cell points to the first
;;     instruction of the combined constants and code vector,
;;     So it's really a special locative (points into the middle
;;     of an object not at the first word after the header)
;;     I bet the x86-64 is unique among all the CCL implementations
;;     in this respect.  So now I'm thinking we should go the ARMv7 route

(defun list2 (x y)
  (let ((tail (cons y nil)))
    (cons x tail)))
;; a simpler list (cons x (cons y nil)) gets optimized into (list x y)

(def-asm-lisp-fun list2 (:architecture :ARM64)
  (%splice
    (cmp (% nargs) ($ 16))       ; check nargs
    (beq L137)
    (uuo-error-wrong-nargs)

    L137
    (push1 (% fp))               ; set up function frame
    (mov (% fp) (% sp))
    (push1 (% vsp))
    (push2 (% fn) (% lr))
    (mov (% fn) (% nfn))
    (vpush2 (% arg-z) (% arg-y))
    (ldr (% arg-y) (@ (% vsp) ($ 0)))  ;bogus use of mov, changed to ldr (in rc2b, rc3), why is this here, swap? (in x86, not in arm32) 
    (mov (% arg-z) ($ nil-value))          ; ??? .w was wrong size register for a pointer
    (sub (% allocptr) (% allocptr) ($ 13))   ; allocptr = allocptr - 13 (now allocptr has cons tag 3)
    (ldr (% imm0) (@ (% rcontext) ($ #xE0))) ; tcr field save-allocbase
    (cmp (% allocptr) (% imm0))              ; is allocptr > allocbase?
    (bhi l53)
    (uuo-alloc-trap)

    L53
    (str (% arg-y) (@ (% allocptr) ($ 5)))   ; allocptr[5] = arg-y (2)
    (str (% arg-z) (@ (% allocptr) ($ -3)))  ; allocptr[-3] = arg-z (nil-value)
    (mov (% arg-z) (% allocptr))             ; arg-z = tagged address of cons cell (2 nil)
    (bic (% allocptr) (% allocptr) ($ 15))   ; clear tag bits (note: we haven't updated the allocptr in the tcr, but maybe we should)
    (vpush1 (% arg-z))                       ; vstack top->bottom: cons pointer, 8, 16
    (ldr (% arg-z) (@ (% vsp) ($ 16)))       ; arg-z = 1
    (ldr (% arg-x) (@ (% vsp) ($ 0)))        ; arg-x = cons pointer
    (sub (% allocptr) (% allocptr) ($ 13))
    (ldr (% imm0) (@ (% rcontext) ($ #xE0))) ; allocbase
    (cmp (% allocptr) (% imm0))
    (bhi L107)
    (uuo-alloc-trap)

    L107
    (str (% arg-z) (@ (% allocptr) ($ 5)))  ; allocptr[5] = 1
    (str (% arg-x) (@ (% allocptr) ($ -3))) ; allocptr[-3] = pointer to (2 nil)
    (mov (% arg-z) (% allocptr))            ; arg-z = pointer to cons cell (1 (2 nil))
    (bic (% allocptr) (% allocptr) ($ 15))  ; clear tag bits
    (pop2 (% fn) (% lr))
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))
    (ret)
    ))


(defun verify-list2 (lst x y)  ;<-- this function can be call "reference conses" too
  (cl:and (consp lst)                   ; lst is a cons
       (eql (first lst) x)           ; car = x
       (consp (rest lst))            ; cdr is a cons
       (eql (second lst) y)          ; cadr = y
       (null (rest (rest lst)))))    ; cddr = nil

(def-asm-lisp-fun verify-list2 (:architecture :ARM64)
  (%splice
    (cmp (% nargs) ($ 24))
    (beq L13)
    (uuo-error-wrong-nargs)

    L13
    (push1 (% fp))
    (mov (% fp) (% sp))
    (push1 (% vsp))
    (push2 (% fn) (% lr))
    (mov (% fn) (% nfn))
    (vpush1 (% arg-x))
    (vpush1 (% arg-y))
    (vpush1 (% arg-z))

;;; (consp lst)
    (and (% imm0.w) (% arg-x.w) ($ 15)) ; clear all but tag bits
    (cmp (% imm0.w) ($ 3))              ; cons cell?
    (bne L265)

;;; (first lst)
    (and (% imm0.w) (% arg-x.w) ($ 7))  ; clear all but subtag bits (arg-y was wrong. -> arg-x)
    (cmp (% imm0.w) ($ 3))              ; cons cell?
    (beq L57)
    (uuo-error-reg-not-list (% arg-x))

    L57
    (ldr (% arg-y) (@ (% arg-x) ($ 5))); arg-y = (first lst)
;;; (eql (first lst) x)
    (ldr (% arg-z) (@ (% vsp) ($ 8)))   ; arg-z = x
    (bl SPBUILTIN-EQL)     ;de-dot
    (cmp (% arg-z) ($ nil-value))       ; eql returned nil?
    (beq L257)
;;; (rest lst)
    (ldr (% arg-z) (@ (% vsp) ($ 16)))  ; arg-z = lst
    (and (% imm0.w) (% arg-z.w) ($ 7))
    (cmp (% imm0.w) ($ 3))              ; is lst a cons?
    (beq L101)
    (uuo-error-reg-not-list (% arg-z))

    L101
    (ldr (% arg-z) (@ (% arg-z) ($ -3))) ; arg-z = (rest lst)
;;; (consp (rest lst))
    (and (% imm0.w) (% arg-z.w) ($ 15))
    (cmp (% imm0.w) ($ 3))              ; is (rest lst) a cons?
    (bne L249)
;;; (second lst)
    (ldr (% arg-z) (@ (% vsp) ($ 16)))
    (and (% imm0.w) (% arg-z.w) ($ 7))
    (cmp (% imm0.w) ($ 3))              ; is lst a cons?
    (beq L138)
    (uuo-error-reg-not-list (% arg-z))

    L138
    (ldr (% arg-z) (@ (% arg-z) ($ -3)))
    (and (% imm0.w) (% arg-z.w) ($ 7))
    (cmp (% imm0.w) ($ 3))              ; is (rest lst) a cons?
    (beq L157)
    (uuo-error-reg-not-list (% arg-z))

    L157
    (ldr (% arg-y) (@ (% arg-z) ($ 5)))  ; arg-y = second lst
;;; (eql (second lst) y)
    (ldr (% arg-z) (@ (% vsp) ($ 0)))    ; y
    (bl SPBUILTIN-EQL)    ;de-dot
    (cmp (% arg-z) ($ nil-value))        ; did eql return nil?
    (beq L241)
;;; (rest lst)
    (ldr (% arg-z) (@ (% vsp) ($ 16)))
    (and (% imm0.w) (% arg-z.w) ($ 7))
    (cmp (% imm0.w) ($ 3))
    (beq L201)
    (uuo-error-reg-not-list (% arg-z))

    L201
    (ldr (% arg-z) (@ (% arg-z) ($ -3)))
;;; (rest (rest lst))
    (and (% imm0.w) (% arg-z.w) ($ 7))
    (cmp (% imm0.w) ($ 3))
    (beq L221)
    (uuo-error-reg-not-list (% arg-z))

    L221
    (ldr (% arg-z) (@ (% arg-z) ($ -3))) ; arg-z = (rest (rest lst))
;;; (null (rest (rest lst)))
    (cmp (% arg-z) ($ nil-value))       ; is arg-z nil?
    (mov (% arg-z) ($ nil-value))       ; no: arg-z = nil
    (bne L245)
    (mov (% arg-z) ($ t-value))         ; yes: arg-z = t
    (b L245)
    
;;; (and (consp lst) (eql (first lst) x) (consp (rest lst)) (eql (second lst) y) (null (rest (rest lst))
    L241
    (mov (% arg-z) ($ nil-value))

    L245
    (pop2 (% fn) (% lr))
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))
    (ret)

    L249
    (mov (% arg-z) ($ nil-value))
    (pop2 (% fn) (% lr))
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))
    (ret)

    L257
    (mov (% arg-z) ($ nil-value))
    (pop2 (% fn) (% lr))
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))
    (ret)

    L265
    (mov (% arg-z) ($ nil-value))
    (pop2 (% fn) (% lr))
    (pop1 (% vsp))
    (mov (% sp) (% fp))
    (pop1 (% fp))
    (ret)
    ))


;;; a heavily simplified version of the x86 spbuiltin-eql

(def-asm-fun SPBUILTIN-EQL (:architecture :ARM64)  ;de-dot
  (%splice
    (cmp (% arg-z) (% arg-y))
    (bne NOT-EQUAL)
    (mov (% arg-z) ($ t-value))
    (ret)

    NOT-EQUAL
    (mov (% arg-z) ($ nil-value))
    (ret)
    ))

   

;; there is no push and pop, but we write our own instructions

;; push =  (str (% __) (+! (% sp) ($ -8)))
;; push2 = (stp (% __) (% __) (+! (% sp) ($ -16)))
;; pop =   (ldr (% __) (!+ (% sp) ($ 8)))
;; pop2 =  (ldp (% __) (% __) (!+ (% sp) ($ 16)))

#|
Addressing modes:
- Simple: X1 is not changed
    - LDR W0, [X1]
- Offset: X1 is not changed
    - LDR W0, [X1, #4]
- Pre-indexed: X1 changed before load
    - LDR W0, [X1, #4]!
- Post-indexed: X1 changed after load
    - LDR W0, [X1], #4


(ldr (% arg-z) (% arg-y))            ;simple
(ldr (% arg-z) (@ (% arg-y) ($ 4)))  ;offset
(ldr (% arg-z) (+! (% arg-y) ($ 4))) ;pre-index
(ldr (% arg-z) (!+ (% arg-y) ($ 4))) ;post-index

|#

(def-simulator-code list-top-level
  :LISPFUNS (call-list2
	     list2
	     verify-list2
	     )
  :BUILTINS (spbuiltin-eql)    ;de-dot
  )

(def-simulator-code expt-top-level
  :LISPFUNS (expt-caller-2-3
	     my-expt)
  :BUILTINS (spbuiltin-times   ;de-dot all
             ;; Below: unused for now, but referenced
             spbuiltin-eq
             spfix-overflow
             spbuiltin-minus
             spmakes128
             ;;-spmakes64
	     ))

#+DE-DUP-FOR-AS
(def-simulator-code expt-3-3-top-level
  :LISPFUNS (expt-caller-3-3
	     my-expt)
  :BUILTINS (spbuiltin-times
	     ;; Below: unused for now, but referenced
             spbuiltin-eq
             spfix-overflow
             spbuiltin-minus
             spmakes128
             ;;spmakes64
	     ))

(defun test-example (simulator-code expected)
  "Report results from each test"
  (let* ((actual (funcall simulator-code))
	 (pass-p (equal expected actual)))
    (format t
	    "~a: expected: ~a actual: ~a pass? ~a~%" simulator-code expected actual pass-p)
    pass-p))

(defun simulator-test-suite ()
  "Run the whole test suite"
  (cl:and
   (test-example 'expt-top-level 8)
   ;;(test-example 'expt-3-3-top-level 27) ;de-dup 'as'
   (test-example 'list-top-level T)))

(eval-when (load)
  "Dump memory only once, after the test suite has run"
  (format t "~&/*~%")
  (simulator-test-suite)
  (format t "~&*/~%")
  (dump-memory-to-as t))
