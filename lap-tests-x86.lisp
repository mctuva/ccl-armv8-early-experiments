;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SX86; Base: 10 -*-

(in-package :SX86)

;;; Revision history:

;;; w0628 - Tim - Add reference code: Simple printer: my-print, my-princ
;;; w0629 - Ayla - Added list2, call-list2, verify-list2. DOESN'T COMPILE (need to fix jmp)
;;; w0629 - Tim - moved printer and reader to separate file: printer-reader-examples.lisp
;;; w0630 - Ayla - lots of (unnecessary, but somewhat helpful) cons examples added
;;; w0701 - Ayla - added test suite function
;;; w0802 - Tim -  added a bunch of comments, many marked with #, no changes to code
;;; w0803 - Tim - added notes, no code changes
;;; NOTE TO SELF: Remember to M-x replace-string _ to - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Lisp functions

#+REFERENCE
(defun expt-caller-2-3 () (my-expt 2 3))

(def-asm-lisp-fun EXPT-CALLER-2-3 (:architecture :X8664)
    (%SPLICE
      (recover-fn-from-rip)             ;     [0]   find fn header
      (testl (% nargs) (% nargs))       ;     [7]   wants 0 args
      (jne (^ L49))                         ;     [9]   if not, die
      (pushq (% rbp))                   ;    [11]   save old bp
      (movq (% rsp) (% rbp))            ;    [12]   sp -> bp
      (movl ($ 16) (% arg-y.l))         ;    [15]   2 -> arg1
      ;;(movl ($ 24) (% arg-y.l))         ;    [15]   3 -> arg1
      (movl ($ 24) (% arg-z.l))         ;    [20]   3 -> arg2
      (movl ($ 16) (% nargs))           ;    [25]   2 -> nargs
      (movq (@ 'MY-EXPT (% fn)) (% temp0)) ;    [30]   (symbol-function 'my-expt) why fn? +++
      (movq (% rbp) (% rsp))               ;    [37]   bp -> sp
      (popq (% rbp))                       ;    [40]   restore old bp
      (jmpq (@ 10 (% temp0)))    ;    [41]   tail-call to code-vector?                  ;aha! this is a jump indirect
                                        ;           biased pointer (explains offset 10)
      (:align 4)
      L49                               ;   [@64]
      (uuo-error-wrong-number-of-args))) ;    [49]   die

#+REFERENCE
(defun my-expt (b n)
  (if (= n 0)
      1
      (* b (my-expt b (- n 1)))))

(def-asm-lisp-fun MY-EXPT (:architecture :X8664)
    (%SPLICE
      (recover-fn-from-rip) ;     [0] ;(leaq (@ (:apply - L1) (% rip)) (% fn))
      ;;L1

      (cmpl ($ 16) (% nargs))           ;     [7] ;must set FLAGS
      (jne (^ L177))                    ;    [10] ;from cmpl
      L16                               ;   [@31]
      (pushq (% rbp))                   ;    [16]
      (movq (% rsp) (% rbp))            ;    [17]
      (pushq (% arg-y))                 ;    [20]
      (pushq (% arg-z))                 ;    [21]

;;; (= n 0)
      (movq (@ -16 (% rbp)) (% arg-y))  ;    [22] ;arg 2 -> %arg-y
      (testb ($ 7) (% arg-y.b))    ;    [26] ;fixnum? (must set FLAGS)
      (jne (^ L39))                ;    [30] ; no -> general case
      (testq (% arg-y) (% arg-y)) ;    [32] ;is %arg-y zero? (must set FLAGS)
      (je (^ L69))                ;    [35] ;yes (from testq)
      (jmp (^ L79))               ;    [37] ;no                   ;PC relative addressing

      (:align 1)
      L39                          ;   [@54] 
      (xorl (% arg-z.l) (% arg-z.l)) ;    [39] clear low word -> 0 (but only low word!) ... +A+ Is this really happening? does it clear upper?
      (lisp-call  (@ .SPBUILTIN-EQ)) ;    [46] align and call '=-2' subprim
      (recover-fn-from-rip) ;    [53] ;(leaq (@ (:apply - L2) (% rip)) (% fn))
      ;;L2
      (cmpq ($ #x1300B) (% arg-z)) ;    [60] ;compare against NIL, must set FLAGS
      (je (^ L79))                 ;    [67] ;from cmpl

;;; (if (= n 0) 1 (* b (my-expt b (- n 1))))

      (:align 2) ;;this is probably done by inserting NOP instruction(s) of convenient lengths
      L69        ;   [@84]   ;+++ what is the TAG for TRA?
      (movl ($ 8) (% arg-z.l))          ;    [69]   ;return fixnum 1    ;+A+ What if there is "garbage" in the upper 32 bits?
      (movq (% rbp) (% rsp))            ;    [74]   ;cleanup
      (popq (% rbp))                    ;    [77]
      (retq)                            ;    [78]   ;return

;;; (- n 1)

      (:align 1)
      L79                               ;   [@94]
      (movq (@ -16 (% rbp)) (% arg-z))  ;    [79] ;arg 2 -> %arg-z
      (testb ($ 7) (% arg-z.b))    ;    [83] fixnum? ;(must set FLAGS)
      (jne (^ L118))               ;    [87] no ;(from testb)
      (addq ($ -8) (% arg-z))      ;    [89] yes 1- ;(must set FLAGS)
      (jno (^ L140))       ;    [93] ;jump on overflow (from the addq)
      (lisp-call (@ .SPFIX-OVERFLOW))   ;   [102]
      (recover-fn-from-rip) ;   [109] ;(leaq (@ (:apply - L3) (% rip)) (% fn))
      ;;L3
      (jmp (^ L140))                    ;   [116]
      L118                              ;  [@133]
      (movq ($ -8) (% arg-y))           ;   [118] general case
      (lisp-call (@ .SPBUILTIN-PLUS))   ;   [126]
      (recover-fn-from-rip) ;   [133] ;(leaq (@ (:apply - L4) (% rip)) (% fn))
      ;;L4
;;; (my-expt b (- n 1))
      L140                              ;  [@155]
      (movq (@ -8 (% rbp)) (% arg-y))   ;   [140]
      (lisp-call (^ L16)) ;   [144]  ;Aha! This is the recursive call!
      (recover-fn-from-rip) ;   [149] ;(leaq (@ (:apply - L5) (% rip)) (% fn))
      ;;L5
;;; (* b (my-expt b (- n 1)))
      (movq (@ -8 (% rbp)) (% arg-y))   ;   [156]
      (movq (% rbp) (% rsp))            ;   [160]
      (popq (% rbp))                    ;   [163]
      (jmp (@ .SPBUILTIN-TIMES))        ;   [164] ;required built-in  ;+++ Is this an absolute jump?

;;; #<no source text>

      (:align 4)
      L177                              ;  [@192]
      (uuo-error-wrong-number-of-args)  ;   [177]
      ))

#+REFERENCE
(defun lookup-constants-test () (b (c '(1 2 3))))

(def-asm-lisp-fun LOOKUP-CONSTANTS-TEST (:architecture :X8664)
  (%SPLICE
    (recover-fn-from-rip)                   ;     [0]
    (testl (% nargs) (% nargs))             ;     [7]
    (jne (^ L69))                           ;     [9]
    (pushq (% rbp))                         ;    [11]
    (movq (% rsp) (% rbp))                  ;    [12]
    (movq (@ '(1 2 3) (% fn)) (% arg-z))    ;    [15]
    (movl ($ 8) (% nargs))                  ;    [22]
    (movq (@ 'C (% fn)) (% temp0))          ;    [27]
    (lisp-call (@ 10 (% temp0)))            ;    [34]
    (recover-fn-from-rip)                   ;    [37]
    (movl ($ 8) (% nargs))                  ;    [44]
    (movq (@ 'B (% fn)) (% temp0))          ;    [49]
    (movq (% rbp) (% rsp))                  ;    [56]
    (popq (% rbp))                          ;    [59]
    (jmpq (@ 10 (% temp0)))                 ;    [60]

    (:align 2)
L69                                         ;   [@84]
    (uuo-error-wrong-number-of-args)        ;    [69]
    ))

#+REFERENCE
(defun list-caller-1-2 ()
  (list2 1 2))

;; x86: The stack pointer always points to the value stack when its running Lisp code
;; arm: There is a value stack distinct from the control stack

(def-asm-lisp-fun LIST-CALLER-1-2 (:architecture :X8664)
  (%SPLICE
    (recover-fn-from-rip)                   ;     [0]
    (testl (% nargs) (% nargs))             ;     [7]
    (jne L49)                               ;     [9]
    (pushq (% rbp))                         ;    [11] **
    (movq (% rsp) (% rbp))                  ;    [12] **
    (movl ($ 8) (% arg-y.l))                ;    [15]
    (movl ($ 16) (% arg-z.l))               ;    [20]
    (movl ($ 16) (% nargs))                 ;    [25]
    (movq (@ 'LIST2 (% fn)) (% temp0))      ;    [30]
    (movq (% rbp) (% rsp))                  ;    [37]
    (popq (% rbp))                          ;    [40]
    (jmpq (@ 10 (% temp0)))                 ;    [41]

    (:align 4)
    L49                                     ;   [@64]
    (uuo-error-wrong-number-of-args)        ;    [49]
    ))

#+REFERENCE
(defun call-list2 ()
  (let ((x 1)
        (y 2))
    (let ((lst (list2 x y)))             ;Call LIST2, then test that:
      (verify-list2 lst x y))))

(def-asm-lisp-fun CALL-LIST2 (:architecture :X8664)
  (%SPLICE
    (recover-fn-from-rip)                   ;     [0] We will NOT use this trick for the ARMv8
    (testl (% nargs) (% nargs))             ;     [7]
    (jne L93)                               ;     [9]


    (pushq (% rbp))                         ;    [11] ** pushes old FP
    (movq (% rsp) (% rbp))                  ;    [12] ** sets up new FP



    ;; Regular call
    (movl ($ 8) (% arg-y.l))                ;    [15] Sets up a new stack frame
    (movl ($ 16) (% arg-z.l))               ;    [20]
    (movl ($ 16) (% nargs))                 ;    [25]
    (movq (@ 'LIST2 (% fn)) (% temp0))      ;    [30] ** get symbol

    (lisp-call (@ 10 (% temp0)))            ;    [42] ** call @ through fun cell
    (recover-fn-from-rip)                   ;    [45]
    (pushq (% arg-z))                       ;    [52]
    (movq (@ -8 (% rbp)) (% arg-x))         ;    [53]
    ;; Tail call
    (movl ($ 8) (% arg-y.l))                ;    [57] Modifies the existing stk frame
    (movl ($ 16) (% arg-z.l))               ;    [62] "stack frame" includes
    (movl ($ 24) (% nargs))                 ;    [67]    registers (like nargs)
    (movq (@ 'VERIFY-LIST2 (% fn)) (% temp0)) ;    [72]


    (movq (% rbp) (% rsp))                  ;    [79] [# once you do this you've lost the ability to POP]
    (popq (% rbp))                          ;    [82] [# BUT if the CALL instruction "pre-pushed" below the frame, this is okay]
    (jmpq (@ 10 (% temp0)))                 ;    [83] ** This a tail call @ fun cell

    (:align 2)
L93                                         ;  [@108]
    (uuo-error-wrong-number-of-args)        ;    [93]
    ))

#+REFERENCE
(defun verify-list2 (lst x y)  ;<-- this function can be call "reference conses" too
  (and (consp lst)                   ; lst is a cons
       (eql (first lst) x)           ; car = x
       (consp (rest lst))            ; cdr is a cons
       (eql (second lst) y)          ; cadr = y
       (null (rest (rest lst)))))    ; cddr = nil

(def-asm-lisp-fun VERIFY-LIST2 (:architecture :X8664)
    (%SPLICE
      (recover-fn-from-rip)                   ;     [0]
      (cmpl ($ 24) (% nargs))                 ;     [7]
      (jne L313)                              ;    [10]
      (pushq (% rbp))                         ;    [16]
      (movq (% rsp) (% rbp))                  ;    [17]
      (pushq (% arg-x))                       ;    [20]
      (pushq (% arg-y))                       ;    [22]
      (pushq (% arg-z))                       ;    [23]
      (movq (@ -8 (% rbp)) (% arg-z))         ;    [24]
      (movl (% arg-z.l) (% imm0.l))           ;    [28]
      (andl ($ 15) (% imm0.l))                ;    [30]
      (cmpb ($ 3) (% imm0.b))                 ;    [33]
      (jne L296)                              ;    [36]
      (movq (@ -8 (% rbp)) (% arg-y))         ;    [42]
      (movl (% arg-y.l) (% imm0.l))           ;    [46]
      (andl ($ 7) (% imm0.l))                 ;    [48]
      (cmpl ($ 3) (% imm0.l))                 ;    [51]
      (jne L321)                              ;    [54]
      (movq (@ 5 (% arg-y)) (% arg-y))        ;    [60]
      (movq (@ -16 (% rbp)) (% arg-z))        ;    [64]
      (lisp-call  (@ .SPBUILTIN-EQL))         ;    [70]
      (recover-fn-from-rip)                   ;    [77]
      (cmpq ($ #x1300B) (% arg-z))            ;    [84]
      (je L286)                               ;    [91]
      (movq (@ -8 (% rbp)) (% arg-z))         ;    [97]
      (movl (% arg-z.l) (% imm0.l))           ;   [101]
      (andl ($ 7) (% imm0.l))                 ;   [103]
      (cmpl ($ 3) (% imm0.l))                 ;   [106]
      (jne L329)                              ;   [109]
      (movq (@ -3 (% arg-z)) (% arg-z))       ;   [115]
      (movl (% arg-z.l) (% imm0.l))           ;   [119]
      (andl ($ 15) (% imm0.l))                ;   [121]
      (cmpb ($ 3) (% imm0.b))                 ;   [124]
      (jne L276)                              ;   [127]
      (movq (@ -8 (% rbp)) (% arg-z))         ;   [133]
      (movl (% arg-z.l) (% imm0.l))           ;   [137]
      (andl ($ 7) (% imm0.l))                 ;   [139]
      (cmpl ($ 3) (% imm0.l))                 ;   [142]
      (jne L337)                              ;   [145]
      (movq (@ -3 (% arg-z)) (% arg-z))       ;   [151]
      (movl (% arg-z.l) (% imm0.l))           ;   [155]
      (andl ($ 7) (% imm0.l))                 ;   [157]
      (cmpl ($ 3) (% imm0.l))                 ;   [160]
      (jne L345)                              ;   [163]
      (movq (@ 5 (% arg-z)) (% arg-y))        ;   [169]
      (movq (@ -24 (% rbp)) (% arg-z))        ;   [173]
      (lisp-call  (@ .SPBUILTIN-EQL))         ;   [182]
      (recover-fn-from-rip)                   ;   [189]
      (cmpq ($ #x1300B) (% arg-z))            ;   [196]
      (je L266)                               ;   [203]
      (movq (@ -8 (% rbp)) (% arg-z))         ;   [205]
      (movl (% arg-z.l) (% imm0.l))           ;   [209]
      (andl ($ 7) (% imm0.l))                 ;   [211]
      (cmpl ($ 3) (% imm0.l))                 ;   [214]
      (jne L353)                              ;   [217]
      (movq (@ -3 (% arg-z)) (% arg-z))       ;   [223]
      (movl (% arg-z.l) (% imm0.l))           ;   [227]
      (andl ($ 7) (% imm0.l))                 ;   [229]
      (cmpl ($ 3) (% imm0.l))                 ;   [232]
      (jne L361)                              ;   [235]
      (movq (@ -3 (% arg-z)) (% arg-z))       ;   [237]
      (cmpq ($ #x1300B) (% arg-z))            ;   [241]
      (movl ($ #x1302E) (% imm0.l))           ;   [248]
      (movl ($ #x1300B) (% arg-z.l))          ;   [253]
      (cmovel (% imm0.l) (% arg-z.l))         ;   [258]
      (movq (% rbp) (% rsp))                  ;   [261]
      (popq (% rbp))                          ;   [264]
      (retq)                                  ;   [265]
      L266                                        ;  [@281]
      (movl ($ #x1300B) (% arg-z.l))          ;   [266]
      (movq (% rbp) (% rsp))                  ;   [271]
      (popq (% rbp))                          ;   [274]
      (retq)                                  ;   [275]
      L276                                        ;  [@291]
      (movl ($ #x1300B) (% arg-z.l))          ;   [276]
      (movq (% rbp) (% rsp))                  ;   [281]
      (popq (% rbp))                          ;   [284]
      (retq)                                  ;   [285]
      L286                                        ;  [@301]
      (movl ($ #x1300B) (% arg-z.l))          ;   [286]
      (movq (% rbp) (% rsp))                  ;   [291]
      (popq (% rbp))                          ;   [294]
      (retq)                                  ;   [295]
      L296                                        ;  [@311]
      (movl ($ #x1300B) (% arg-z.l))          ;   [296]
      (movq (% rbp) (% rsp))                  ;   [301]
      (popq (% rbp))                          ;   [304]
      (retq)                                  ;   [305]

      (:align 3)
      L313                                        ;  [@328]
      (uuo-error-wrong-number-of-args)        ;   [313]

      (:align 4)
      L321                                        ;  [@336]
      (uuo-error-reg-not-list (% arg-y))      ;   [321]

      (:align 3)
      L329                                        ;  [@344]
      (uuo-error-reg-not-list (% arg-z))      ;   [329]

      (:align 4)
      L337                                        ;  [@352]
      (uuo-error-reg-not-list (% arg-z))      ;   [337]

      (:align 3)
      L345                                        ;  [@360]
      (uuo-error-reg-not-list (% arg-z))      ;   [345]

      (:align 4)
      L353                                        ;  [@368]
      (uuo-error-reg-not-list (% arg-z))      ;   [353]

      (:align 3)
      L361                                        ;  [@376]
      (uuo-error-reg-not-list (% arg-z))      ;   [361]
      ))

#+REFERENCE
(defun list2 (x y)
  (let ((tail (cons y nil)))
    (cons x tail)))
;; a simpler list (cons x (cons y nil)) gets optimized into (list x y)

(def-asm-lisp-fun LIST2 (:architecture :X8664)
  (%SPLICE
    (recover-fn-from-rip)                   ;     [0]
    (cmpl ($ 16) (% nargs))                 ;     [7]
    (jne L137)                              ;    [10]
    (pushq (% rbp))                         ;    [12]
    (movq (% rsp) (% rbp))                  ;    [13]
    (pushq (% arg-y))                       ;    [16]
    (pushq (% arg-z))                       ;    [17]
    (movq (@ -16 (% rbp)) (% arg-y))        ;    [18]
    (movl ($ #x1300B) (% arg-z.l))          ;    [22]
    (subq ($ 13) (@ #xD8 (% rcontext)))     ;    [27] ; decrement the allocptr
    (movq (@ #xD8 (% rcontext)) (% temp0))  ;    [35]
    (cmpq (@ #xE0 (% rcontext)) (% temp0))  ;    [42]
    (ja L53)                                ;    [49]
    (uuo-alloc)                             ;    [51]

    (:align 2)
    L53                                     ;   [@68]
    (andb ($ #xF0) (@ #xD8 (% rcontext)))   ;    [53]
    (movq (% arg-y) (@ 5 (% temp0)))        ;    [61]
    (movq (% arg-z) (@ -3 (% temp0)))       ;    [65]
    (movq (% temp0) (% arg-z))              ;    [69]
    (pushq (% arg-z))                       ;    [72]
    (movq (@ -8 (% rbp)) (% arg-z))         ;    [73]
    (movq (@ -24 (% rbp)) (% arg-x))        ;    [77]
    (subq ($ 13) (@ #xD8 (% rcontext)))     ;    [81]
    (movq (@ #xD8 (% rcontext)) (% temp0))  ;    [89]
    (cmpq (@ #xE0 (% rcontext)) (% temp0))  ;    [96]
    (ja L107)                               ;   [103]
    (uuo-alloc)                             ;   [105]

    (:align 1)
    L107                                    ;  [@122]
    (andb ($ #xF0) (@ #xD8 (% rcontext)))   ;   [107]
    (movq (% arg-z) (@ 5 (% temp0)))        ;   [115]
    (movq (% arg-x) (@ -3 (% temp0)))       ;   [119]
    (movq (% temp0) (% arg-z))              ;   [123]
    (movq (% rbp) (% rsp))                  ;   [126]
    (popq (% rbp))                          ;   [129]
    (retq)                                  ;   [130]

    (:align 3)
    L137                                    ;  [@152]
    (uuo-error-wrong-number-of-args)        ;   [137]
    ))
    
;;; The subprims

(def-asm-fun .SPBUILTIN-TIMES (:architecture :X8664)  ;this will be jumped to (tail-call)
    (%SPLICE
      (movb (% arg-z.b) (% imm0.b))
      (orb (% arg-y.b) (% imm0.b))
      (testb ($ fixnummask) (% imm0.b)) ;both fixnums?
      (jne (^ L2-X))                          ;no
      ;; yes
      (unbox-fixnum (% arg-z) (% imm0))
      ;;    /* 128-bit fixnum result in (%imm1:%imm0). Overflow set if %imm1  */
      ;;    /* is significant   */
      (imul (% arg-y)) ;implicit source (% imm0) implicit dest (% imm1):(% imm0) ;arg-y is still scaled by 8
      (jo (^ L1-X))

      (mov (% imm0) (% arg-z))          ;simple case
      (ret)

      L1-X
      (unbox-fixnum (% arg-z) (% imm0))
      (unbox-fixnum (% arg-y) (% imm1))
      (imul (% imm1))        ;(%imm 1):(% imm0) <= (% imm0) * (% imm1)
      (jmp (^ -spmakes128))  ;later

      L2-X
      (jump-builtin -builtin-times 2)
      ))

(def-asm-fun .SPBUILTIN-EQ (:architecture :X8664)
    (%SPLICE
       (movb (% arg-z.b) (% imm0.b))
       (orb (% arg-y.b) (% imm0.b))
       (testb ($ fixnummask) (% imm0.b))
       (jne (^ L1-EQ))                      ;(tag(arg1) || tag(arg2) zero -> both fixnums; non-zero -> general case
       
       (rcmpq (% arg-z) (% arg-y))         ;reverse compare
       
       (movl ($ t-value) (% imm0.l))                    ;\  move T into %imm0           doesn't affect FLAGS
       (lea (@ (:apply - t-offset) (% imm0)) (% arg-z)) ;|--move NIL into %arg-z        doesn't affect FLAGS
       (cmovel (% imm0.l) (% arg-z.l))                  ;/  conditional move on E (but only long???)
       
       (ret)
     L1-EQ
       (jump-builtin -builtin-eq 2) ; /* a -jump- to the Lisp built-in not a -call-? no new stack frame? */
       ))

(def-asm-fun .SPBUILTIN-PLUS (:architecture :X8664)
    (%SPLICE
      (movb (% arg-z.b) (% imm0.b))
      (orb (% arg-y.b) (% imm0.b))
      (testb ($ fixnummask) (% imm0.b))
      (jne (^ L1+))                          ;general case

      (addq (% arg-y) (% arg-z))
      (jo (^ fix-one-bit-overflow))         ;overflow  ;why C(...) notation? see immediately above for def
      (repret)
      L1+
      (jump-builtin -builtin-plus 2)
      ))

(def-asm-fun .SPFIX-OVERFLOW (:architecture :X8664)
    (%SPLICE
      fix-one-bit-overflow  ;alias label
      (ret))
  ;;;;;;;;;
    #+LATER
    `(%SPLICE
      fix-one-bit-overflow  ;alias label
       (movq ($ two-digit-bignum-header) (% imm0))
       (Misc-Alloc-Fixed nil (aligned-bignum-size 2))
       (unbox-fixnum (% arg-z) (% imm0))
       (movq ($ 0xe000000000000000) (% imm1))
       (mov (% temp0) (% arg-z))
       (xorq (% imm1) (% imm0))
       (movq (% imm0) (@ misc-data-offset (% arg-z)))
       (ret)	
       ))

(def-asm-fun -SPMAKES128 (:architecture :X8664)
    (%splice
      (movd (% imm0) (% mm0))
      (movd (% imm1) (% mm1))
	
      ;;/* If (% imm1) is just a sign extension of (% imm0) make a 64-bit signed integer.   */
	
      (sarq ($ 63) (% imm0)) 
      (cmpq (% imm0) (% imm1))
      (movd (% mm0) (% imm0))
      (je (^ -SPmakes64))
	
      ;;/* Otherwise, if the high 32 bits of (% imm1) are a sign-extension of the  */
      ;;/* low 32 bits of (% imm1) make a 3-digit bignum.  If the upper 32 bits  */
      ;;/* of (% imm1) are significant, make a 4 digit bignum   */
	
      (movq (% imm1) (% imm0))
      (shlq ($ 32) (% imm0))
      (sarq ($ 32) (% imm0))
      (cmpq (% imm0) (% imm1))
      (jz (^ L3-128))
      (mov ($ four-digit-bignum-header) (% imm0))
      ;;(Misc-Alloc-Fixed((% arg-z) aligned-bignum-size(4))) ;+++ punt for now
      (movq (% mm0) (@ misc-data-offset (% arg-z)))
      (movq (% mm1) (@ (:apply + misc-data-offset 8) (% arg-z)))
      (ret)
      L3-128 (mov ($ three-digit-bignum-header) (% imm0))
      ;;(Misc-Alloc-Fixed(%arg-z aligned-bignum-size(3))) ;+++ punt for now
      (movq (% mm0) (@ misc-data-offset (% arg-z)))
      (movq (% mm1) (@ (:apply + misc-data-offset 8) (% arg-z)))
      (ret)
      ))

(def-asm-fun -SPMAKES64 (:architecture :X8664)
    (%splice
      (movq (% imm0) (% imm1))
      (shlq ($ fixnumshift) (% imm1))
      (movq (% imm1) (% arg-z))
      (sarq ($ fixnumshift) (% imm1))
      (cmpq (% imm1) (% imm0))
      (jz L0-64)
      (movd (% imm0) (% mm0))
      (movq ($ two-digit-bignum-header) (% imm0))
      ;;(misc-alloc-fixed (% arg-z) (aligned-bignum-size 2)) ;+++ punt for now
      (movq (% mm0) (@ misc-data-offset (% arg-z)))
     L0-64
      (repret)
      ))

(def-asm-fun .SPBUILTIN-EQL (:architecture :X8664)
  (%SPLICE
    (ret)))

;;;;;;;;;;
;;; Finale - link everything together

(def-simulator-code expt-top-level
  :LISPFUNS (expt-caller-2-3                ;main
	     my-expt
	     )
  :BUILTINS (.spbuiltin-times
             ;; Below: unused for now, but referenced
             .spbuiltin-eq
             .spfix-overflow
             .spbuiltin-plus
             -spmakes128
             -spmakes64)
  )

(def-simulator-code list-top-level
  :LISPFUNS (call-list2
	     verify-list2
	     list2)
  :BUILTINS (.spbuiltin-eql)
  )


(defun simulator-test-suite ()
  (and
   (test-example 'expt-top-level 8)
   (test-example 'list-top-level T)))

(defun test-example (simulator-code expected)
  (let* ((actual (funcall simulator-code))
	 (pass-p (equal expected actual)))
    (format t
	    "~a: expected: ~a actual: ~a pass? ~a~%" simulator-code expected actual pass-p)
    pass-p))


;;; Analyze aref subprim and svref open-coding

#+REFERENCE
(defun my-aref (a x y) (declare (optimize (speed 3) (safety 0))) (aref a x y))

#|
_spentry(aref2)
        __(testb $fixnummask,%arg_y_b)
        __(jne 0f)
        
        __(testb $fixnummask,%arg_z_b)
        __(jne 1f)
        __(extract_typecode(%arg_x,%imm0))
        __(cmpb $subtag_arrayH,%imm0_b)
        __(jne 2f)
        __(cmpq $2<<fixnumshift,arrayH.rank(%arg_x))
        __(jne 2f)
        __(cmpq arrayH.dim0(%arg_x),%arg_y)
        __(jae 3f)
        __(movq arrayH.dim0+node_size(%arg_x),%imm0)
        __(cmpq %imm0,%arg_z)
        __(jae 4f)
        __(unbox_fixnum(%imm0,%imm0))
        __(mulq %arg_y)         /* imm0 <- imm0 * arg_y */
        __(addq %imm0,%arg_z)
        __(movq %arg_x,%arg_y)
6:      __(addq arrayH.displacement(%arg_y),%arg_z)
        __(movq arrayH.data_vector(%arg_y),%arg_y)
        __(extract_subtag(%arg_y,%imm1))
        __(cmpb $subtag_vectorH,%imm1_b)
        __(je 6b)
        __(cmpb $subtag_arrayH,%imm1_b)
        __(je 6b) 
        __(jmp C(misc_ref_common))
0:      __(uuo_error_reg_not_fixnum(Rarg_y))
1:      __(uuo_error_reg_not_fixnum(Rarg_z))
2:      __(uuo_error_reg_not_type(Rarg_x,error_object_not_array_2d))
3:      __(uuo_error_array_bounds(Rarg_y,Rarg_x))
4:      __(uuo_error_array_bounds(Rarg_z,Rarg_x))
        
_endsubp(aref2)
|#

#+REFERENCE
(defun my-svref (a x) (svref a x))

#|
CL-USER> (disassemble 'my-svref)
    (recover-fn-from-rip)                   ;     [0]
    (cmpl ($ 16) (% nargs))                 ;     [7]
    (jne L85)                               ;    [10]
    (pushq (% rbp))                         ;    [12]
    (movq (% rsp) (% rbp))                  ;    [13]
    (pushq (% arg_y))                       ;    [16]
    (pushq (% arg_z))                       ;    [17]
    (movq (@ -8 (% rbp)) (% arg_y))         ;    [18]
    (movq (@ -16 (% rbp)) (% arg_z))        ;    [22]
    (movl (% arg_y.l) (% imm0.l))           ;    [26]
    (andl ($ 7) (% imm0.l))                 ;    [28]
    (cmpl ($ 5) (% imm0.l))                 ;    [31]
    (jne L40)                               ;    [34]
    (movsbl (@ -13 (% arg_y)) (% imm0.l))   ;    [36]
L40                                         ;   [@55]
    (cmpl ($ -74) (% imm0.l))               ;    [40]
    (jne L93)                               ;    [43]
    (testb ($ 7) (% arg_z.b))               ;    [45]
    (jne L101)                              ;    [49]
    (movq (@ -13 (% arg_y)) (% imm0))       ;    [51]
    (shrq ($ 8) (% imm0))                   ;    [55]
    (shlq ($ 3) (% imm0))                   ;    [59]
    (cmpq (% imm0) (% arg_z))               ;    [63]
    (jae L109)                              ;    [66]
    (movq (@ -5 (% arg_y) (% arg_z)) (% arg_z)) ;    [68]
    (movq (% rbp) (% rsp))                  ;    [73]
    (popq (% rbp))                          ;    [76]
    (retq)                                  ;    [77]

    (:align 2)
L85                                         ;  [@100]
    (uuo-error-wrong-number-of-args)        ;    [85]

    (:align 2)
L93                                         ;  [@108]
    (uuo-error-reg-not-tag (% arg_y) ($ #xB6)) ;    [93]

    (:align 2)
L101                                        ;  [@116]
    (uuo-error-reg-not-fixnum (% arg_z))    ;   [101]

    (:align 2)
L109                                        ;  [@124]
    (uuo-error-vector-bounds (% arg_z) (% arg_y)) ;   [109]

|#
