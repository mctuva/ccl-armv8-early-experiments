;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SARM; Base: 10 -*-

(in-package :SARM)

;;;;;; Version history:
;;; w0722 - Ayla - replaced 'nil with ($ nil-value), AND with ARM-AND, commented out sploadlr
;;; w0802 - Tim -  added a bunch of comments, many marked with #, no changes to code
;;; w0803 - Tim - added notes, no code changes
;;; w0807 - Tim - arm-and -> and (now that pkgdcl shadows lisp:and)


(defun call-list2 ()
  (let ((x 1)
        (y 2))
    (let ((lst (list2 x y)))
      (verify-list2 lst x y))))

(def-asm-lisp-fun call-list2 (:architecture :ARM32)
  (%splice
    (cmp (% nargs) ($ 0))
    (beq l12b)   ; changing labels by hand (adding b to the end)
    (uuo-error-wrong-nargs (? ne))

    l12b                   ; imm0=r0, which convention says is return value
    (mov (% imm0) ($ 19))  ; I wonder if "19" is a uvector header? [imm0 doesn't need a tag    ; ** 19 = 10011, 8 + tag=3??]
    (stmdb (! (% sp)) ((% imm0) (% vsp) (% fn) (% lr))) ; ** bl/blx puts retval in LR



    (mov (% fn) (% nfn))                ; nfn same as temp2 (coordinates with caller set-up)
    (mov (% arg-y) '1)           ;we think this is already tagged
    (mov (% arg-z) '2)           ;ditto
    (mov (% nargs) ($ 8))        ;diy fixnum
    (ldr (% temp1) (@ (% fn) 'list2))     ; ** get ptr to symbol
    (ldr (% nfn) (@ (% temp1) ($ 6)))     ; ** grab fun cell !!!
    (ldr (% lr) (@ (% nfn) ($ -2)))       ; ** (strip off tag and) de-reference code-vector (nuke)
    (blx (% lr))                          ; ** call it (why is it using the LR?)
    (vpush1 (% arg-z))        ;one of our macros pushes onto vsp
    (mov (% arg-x) (% arg-z))
    (mov (% arg-y) '1)
    (mov (% arg-z) '2)
    (mov (% nargs) ($ 12))
    (ldr (% temp1) (@ (% fn) 'verify-list2))
    (ldmia (! (% sp)) ((% imm0) (% vsp) (% fn) (% lr))) ;pop anything we pushed prep for tail call


    (ldr (% nfn) (@ (% temp1) ($ 6)))
    (ldr (% pc) (@ (% nfn) ($ -2)))  ;a branch by any other name, smells as sweet
    ))

;; Lisp pointer always points to the beginning of an object
;; A "locative" points *inside* an object  (Lisp Machines have "1st class" locatives, CCL "PC" is)
;; On the the ARMv7 clever encoding allows the header of the code-vector to be found from a PC.

;;; On the ARMv7
;; function object (header)
;;   slot 0: code vector       <--- pointer to function points here
;;   slot 1: constants vector ;maybe its constants in slot 0, we'd have to check

;;; On the x86-64 the function object, code vector, and constants vector are all a single uvector

(defun list2 (x y)
  (let ((tail (cons y nil)))
    (cons x tail)))

(def-asm-lisp-fun list2 (:architecture :ARM32)
  (%splice
    (cmp (% nargs) ($ 8))
    (beq l12)
    (uuo-error-wrong-nargs (? ne))
    l12
    (mov (% imm0) ($ 19))                               ;**
    (stmdb (! (% sp)) ((% imm0) (% vsp) (% fn) (% lr))) ;**
    (mov (% fn) (% temp2))
    (stmdb (! (% vsp)) ((% arg-z) (% arg-y)))
    (mov (% arg-x) ($ nil-value))
    (sub (% allocptr) (% allocptr) ($ 3))
    (ldr (% imm0) (@ (% rcontext) ($ 104)))
    (cmp (% allocptr) (% imm0))
    (bhi l52)
    (uuo-alloc-trap)
    l52
    (str (% arg-x) (@ (% allocptr) ($ -5)))
    (str (% arg-z) (@ (% allocptr) ($ -1)))
    (mov (% arg-x) (% allocptr))
    (bic (% allocptr) (% allocptr) ($ 7))
    (vpush1 (% arg-x))
    (sub (% allocptr) (% allocptr) ($ 3))
    (ldr (% imm0) (@ (% rcontext) ($ 104)))
    (cmp (% allocptr) (% imm0))
    (bhi l92)
    (uuo-alloc-trap)
    l92
    (str (% arg-x) (@ (% allocptr) ($ -5)))
    (str (% arg-y) (@ (% allocptr) ($ -1)))
    (mov (% arg-z) (% allocptr))
    (bic (% allocptr) (% allocptr) ($ 7))
    (ldmia (! (% sp)) ((% imm0) (% vsp) (% fn) (% pc))) ; loading pc from what used to be lr is how it comes back from the call
    (break) ;should not get reached
    )) 


(defun verify-list2 (lst x y)
  (cl:and (consp lst)
          (eql (first lst) x)
          (consp (rest lst))
          (eql (second lst) y)
          (null (rest (rest lst)))))

(def-asm-lisp-fun verify-list2 (:architecture :ARM32)
  (%splice
    (cmp (% nargs) ($ 12))
    (beq l13) ; changing the labels by hand (incrementing by 1)
    (uuo-error-wrong-nargs (? ne))
    l13
    (mov (% imm0) ($ 19))
    (stmdb (! (% sp)) ((% imm0) (% vsp) (% fn) (% lr)))
    (mov (% fn) (% temp2))
    (stmdb (! (% vsp)) ((% arg-z) (% arg-y) (% arg-x)))
;;; (consp lst)
    (and (% imm0) (% arg-x) ($ 7)) ;;this was using lisp and before
    (cmp (% imm0) ($ 5))
    (bne l265)
    
;;; (first lst)
    (and (% imm0) (% arg-x) ($ 3))
    (cmp (% imm0) ($ 1))
    (beq l57)
    (uuo-error-reg-not-lisptag (% arg-x) ($ 1))
    l57
    (ldr (% arg-y) (@ (% arg-x) ($ -1)))
;;; (eql (first lst) x)
    (ldr (% arg-z) (@ (% vsp) ($ 4)))
    ;;(sploadlr .spbuiltin-eql)
    ;;(blx (% lr))
    (cmp (% arg-z) ($ nil-value))
    (beq l257)
      ;;; (rest lst)
    (ldr (% arg-z) (@ (% vsp) ($ 8)))
    (and (% imm0) (% arg-z) ($ 3))
    (cmp (% imm0) ($ 1))
    (beq l101)
    (uuo-error-reg-not-lisptag (% arg-z) ($ 1))
    l101
    (ldr (% arg-z) (@ (% arg-z) ($ -5)))
;;; (consp (rest lst))
    (and (% imm0) (% arg-z) ($ 7))
    (cmp (% imm0) ($ 5))
    (bne l249)
;;; (second lst)
    (ldr (% arg-z) (@ (% vsp) ($ 8)))
    (and (% imm0) (% arg-z) ($ 3))
    (cmp (% imm0) ($ 1))
    (beq l137)
    (uuo-error-reg-not-lisptag (% arg-z) ($ 1))
    l137
    (ldr (% arg-z) (@ (% arg-z) ($ -5)))
    (and (% imm0) (% arg-z) ($ 3))
    (cmp (% imm0) ($ 1))
    (beq l157)
    (uuo-error-reg-not-lisptag (% arg-z) ($ 1))
    l157
    (ldr (% arg-y) (@ (% arg-z) ($ -1)))
;;; (eql (second lst) y)
    (ldr (% arg-z) (@ (% vsp) ($ 0)))
    ;;(sploadlr .spbuiltin-eql)          ;; +++ SPLOADLR still todo
    ;;(blx (% lr))
    (cmp (% arg-z) ($ nil-value))
    (beq l241)
;;; (rest lst)
    (ldr (% arg-z) (@ (% vsp) ($ 8)))
    (and (% imm0) (% arg-z) ($ 3))
    (cmp (% imm0) ($ 1))
    (beq l201)
    (uuo-error-reg-not-lisptag (% arg-z) ($ 1))
    l201
    (ldr (% arg-z) (@ (% arg-z) ($ -5)))
;;; (rest (rest lst))
    (and (% imm0) (% arg-z) ($ 3))
    (cmp (% imm0) ($ 1))
    (beq l221)
    (uuo-error-reg-not-lisptag (% arg-z) ($ 1))
    l221
    (ldr (% arg-z) (@ (% arg-z) ($ -5)))
;;; (null (rest (rest lst)))
    (cmp (% arg-z) ($ nil-value))
    (mov (% arg-z) ($ nil-value))
    (addeq (% arg-z) (% arg-z) ($ 13)) ; this is t-value
    (b l245)
;;; (and (consp lst) (eql (first lst) x) (consp (rest lst)) (eql (second lst) y) (null (rest (rest lst))
    ;;
    l241
    (mov (% arg-z) ($ nil-value))
    l245
    (ldmia (! (% sp)) ((% imm0) (% vsp) (% fn) (% pc)))
    l249
    (mov (% arg-z) ($ nil-value))
    (ldmia (! (% sp)) ((% imm0) (% vsp) (% fn) (% pc)))
    l257
    (mov (% arg-z) ($ nil-value))
    (ldmia (! (% sp)) ((% imm0) (% vsp) (% fn) (% pc)))
    l265
    (mov (% arg-z) ($ nil-value))
    (ldmia (! (% sp)) ((% imm0) (% vsp) (% fn) (% pc)))))



#+NOT-READY-YET
(def-asm-lisp-fun CALL-CONSY (:architecture :X8664)
  (%SPLICE
;;; (defun call-consy (z y) (consy (* z y)))
    (cmp (% nargs) ($ 8))
    (beq L12)
    (uuo-error-wrong-nargs (? ne))
    L12
    (mov (% imm0) ($ 19))
    (stmdb (! sp) ((% imm0) (% vsp) (% fn) (% lr))) ; need to handle multiple values
    (mov (% fn) (% temp2))
    (stmdb (! vsp) ((% arg-z) (% arg-y)))        ;[24]

;;; (* z y)
    (sploadlr .SPbuiltin-times)           ;[28]
    (blx (% lr))

;;; (consy (* z y))
    (mov (% nargs) ($ 4))                    ;[36]
    (ldr (% temp1) (@ fn 'CONSY))
    (ldmia (! sp) ((% imm0) (% vsp) (% fn) (% lr))) ; what is !?
    (ldr (% temp2) (@ (% temp1) ($ 6)))
    (ldr (% pc) (@ (% temp2) ($ -2)))           ;[52]
    ))

#+NOT-READY-YET
(def-asm-lisp-fun CONSY (:architecture :X8664)
  (%SPLICE
;;; (defun consy (x) (cons x '(2 3 4)))
    (cmp (% nargs) ($ 4))
    (beq L12)
    (uuo-error-wrong-nargs (? ne))
    L12
    (mov (% imm0) ($ 19))
    (stmdb (! sp) ((% imm0) (% vsp) (% fn) (% lr)))
    (mov (% fn) (% temp2))
    (vpush1 (% arg-z))                        ;[24]

;;; '(2 3 4)
    (ldr (% arg-y) (@ (% fn) '(2 3 4)))          ;[28]

;;; (cons x '(2 3 4))
    (sub (% allocptr) (% allocptr) ($ 3))        ;[32]
    (ldr (% imm0) (@ (% rcontext) ($ #x68)))
    (cmp (% allocptr) (% imm0))
    (bhi L52)
    (uuo-alloc-trap)                      ;[48]
    L52
    (str (% arg-y) (@ (% allocptr) ($ -5)))
    (str (% arg-z) (@ (% allocptr) ($ -1)))
    (mov (% arg-z) (% allocptr))
    (bic (% allocptr) (% allocptr) ($ 7))        ;[64]
    (ldmia (! sp) ((% imm0) (% vsp) (% fn) (% pc)))
    ))

#+NOT-READY-YET
(def-simulator-code arm-simulator-top-level
  :ARCHITECTURE :X8664
  :LISPFUNS (call-consy
	     consy)
  )


(def-simulator-code list-top-level
  :LISPFUNS (call-list2
	     list2
	     verify-list2
	     ))

(defun test-example (simulator-code expected)
  (let* ((actual (funcall simulator-code))
	 (pass-p (equal expected actual)))
    (format t
	    "~a: expected: ~a actual: ~a pass? ~a~%" simulator-code expected actual pass-p)
    pass-p))

(defun simulator-test-suite ()
  (cl:and
   (test-example 'list-top-level T)))
