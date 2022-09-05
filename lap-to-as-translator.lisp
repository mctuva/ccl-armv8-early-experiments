;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SAV8; Base: 10 -*-

(in-package :SAV8)

;;; History:
;;;   w0904 - Tim - Translate constants nil-value and t-value to register references

(setf (get :ARM64 :as-translator) 'lap-fun-to-as)

(defun lap-fun-to-as (lap-body fun-name &optional (stream t))
  "Translate LAP to 'as' syntax"
;;  (let ((*this-function* fun-name))      ;Ayla was worried about this
;;    (declare (special *this-function*))
  (when *quiet-as* (return-from lap-fun-to-as))
  (let ((*print-case* :DOWNCASE)
        (ufun-name (underscorize-symbol fun-name)))
    (lap-define-symbols stream)
    (format stream "~&~8t.text~
	            ~&~8t.align	2~
	            ~&~8t.global	~A~
	            ~&~8t.type	~A, %function"
            ufun-name ufun-name)
    ;; print function name label (upper case for visibility)
    (let ((*print-case* :UPCASE))
      (format stream "~&~%~A:~&~8t.cfi_startproc" ufun-name))
    (let ((instructions (if (eql (first lap-body) '%splice)
                            (rest lap-body)
                            lap-body)))
      (mapcar (lambda (instr) (lap-instruction-to-as instr stream))
	      instructions))
    (format stream "~&~8t.cfi_endproc~
                    ~&~8t.size	~A, .-~A~&~%" ufun-name ufun-name)
    ))

(defparameter *uuo-names*
  '(uuo-error-wrong-nargs
    uuo-alloc-trap
    uuo-error-reg-not-list
    uuo-unimplemented-macro))

(defparameter *definitions-already-done* nil)

(defun lap-define-symbols (stream)
  "Emit the .equ directives so we can see UUOs symbolically in the code"
  (unless *definitions-already-done*
    (format stream "~&~8t.arch armv8-a")
    (format stream "~&~8t.file \"/home/user/filename.s\"")
    (loop for uuo-number upfrom 1
          and uuo-name in *uuo-names* do
            (format stream "~&~8t.equ ~A, ~D" (underscorize-symbol uuo-name) uuo-number)) ;implicit: ARMv8 inst>>16==0 means UUO
    (setq *definitions-already-done* t)))

(defun lap-instruction-to-as (instruction &optional (stream t))
  "Translate one LAP instruction to 'as' syntax"
  (cond ((atom instruction) ; label
	 (lap-label-to-as instruction stream))
	(t
	 (multiple-value-bind (opcode arguments)
	     (destructure-lap-instruction instruction)
	   (cond ((uuo? opcode)
                  (format stream "~&~8t.word ~A " (underscorize-symbol opcode))) ;ignore the operands until we can AND them into the opcode
                 (t
                  (format stream "~&~8t~A " opcode)
                  (format-arguments arguments stream)))))))

(defun uuo? (opcode)
  "Is this a UUO instruction?"
  (member opcode *uuo-names*))

(defun format-arguments (arguments &optional (stream t))
  "Emit argument list"
  (loop for argument in arguments
	for comma = nil then t do
	  (lap-argument-to-as argument comma stream)))

(defun destructure-lap-instruction (instruction)
  "Tease aport opcode and arguments, after 'macro expansion'"
  (let ((asm-instr (lap-macroexpand instruction)))
    (values (first asm-instr)
	    (rest asm-instr))))

(defun lap-macroexpand (instruction)
  "We use a few macros, turn them into instructions"
  (case (car instruction)
    (push1  (macroexpand-1 instruction))
    (pop1   (macroexpand-1 instruction))
    (vpush1 (macroexpand-1 instruction))
    (push2  `(stp ,(second instruction) ,(third instruction) (+! (% sp) ($ -16))))
    (pop2   `(ldp ,(second instruction) ,(third instruction) (!+ (% sp) ($ 16))))
    (vpush2 `(stp ,(second instruction) ,(third instruction) (+! (% vsp) ($ -16))))
    (unbox-fixnum (macroexpand-1 instruction))
    (jump-builtin '(uuo-unimplemented-macro))
    (t instruction)))
  

(defun lap-label-to-as (label &optional (stream t))
  "Emit label"
  (let ((ulabel (underscorize-symbol label)))
    (if (local-label-p ulabel)
        (format stream "~&.~A:" ulabel)   ; old: preceding . makes the label local rather than global
        (format stream "~&~A:" ulabel)))) ; Commentary: added dot once-upon-a-time, BUT may want to reconsider
                                          ;             look in .s files generated by gcc

(defun local-label-p (label)
  "Identify labels of the form lnn (e.g. l23)"
  (setq label (string label))
  (cl:and (> (length label) 1)
          (or (char= #\l (aref label 0)) (char= #\L (aref label 0)))
          (digit-char-p (aref label 1))))

(defun lap-argument-to-as (argument comma &optional (stream t))
  "Translate individual arguments"
  (when comma
    (format stream ", ")) ;Was: (write-char #\, stream)
  (cond ((atom argument)
         (format stream "~A" (if (symbolp argument)
                                 (let ((label (underscorize-symbol argument)))  ;probably a label ;would need to add dot here if add dot to label
                                   (if (local-label-p argument)
                                       (format nil ".~A" label)
                                       label))
                                 argument)))  ;probably an "errant" constant (e.g. 2)
        (t
         (case (car argument)
           (% (lap-register-to-as (cadr argument) stream)) ;register
           (@ (lap-indexed-address-mode-to-as (cadr argument) (caddr argument) stream))
           (+! (lap-pre-increment-address-mode-to-as (cadr argument) (caddr argument) stream))
           (!+ (lap-post-increment-address-mode-to-as (cadr argument) (caddr argument) stream))
           ($ (lap-constant-to-as (cadr argument) stream)))))) ;constant

(defun lap-constant-to-as (constant &optional (stream t))
  "Emits constants, and translates 'variables', others get emitted symbolically"
  (let ((value (cond ((eq constant 't-value)
                      (map-register-name-to-number 't-reg))    ;(new)
                     ((eq constant 'nil-value)
                      (map-register-name-to-number 'nil-reg))  ;(new)
                     ((symbolp constant)
                      (symbol-value constant))
                     (t
                      constant)))) ; converts nil-value, t-value, etc
    ;;Was: (format stream "#~A" value)
    (format stream "~A" value)          ;doesn't look like the arm64 'as' syntax puts sharp-signs in front literals
    ))

#+OLD
(defun lap-constant-to-as (constant &optional (stream t))
  "Emits constants, and translates 'variables', others get emitted symbolically"
  (let ((value (cond ((eq constant 't-value)
                      :t_value+fulltag_symbol) ;(w0904) was just :t_value (no tag)
                     ((eq constant 'nil-value)
                      :nil_value+fulltag_nil)  ;(w0904) was just :nil_value (no tag)
                     ((symbolp constant)
                      (symbol-value constant))
                     (t
                      constant)))) ; converts nil-value, t-value, etc
    ;;Was: (format stream "#~A" value)
    (format stream "~A" value)          ;doesn't look like the arm64 'as' syntax puts sharp-signs in front literals
    ))


(defparameter *lap-to-as-register-map*
  ;;"'as' doesn't let you alias registers, so we have to be the pre-processor"
  '((allocptr x27)   ;callee saves/special-ish/global
    (arg-x    x2)    ;parameter/tagged
    (arg-x.w  w2)
    (arg-y    x1)    ;parameter/tagged
    (arg-y.w  w1)
    (arg-z    x0)    ;parameter/tagged/return value
    (arg-z.w  w0)
    (fn       x26)   ;callee saves/special-ish
    (fname    x10)   ;temp alias
    (fp       x29)   ;frame pointer (special)
    (imm0     x3)    ;un-tagged scratch
    (imm0.w   w3)
    (imm1     x4)    ;un-tagged scratch
    (imm1.w   w4)
    (imm2     x5)    ;un-tagged scratch (aliased to nargs)
    (imm2.w   w5)
    (lr       x30)   ;link register (special)
    (nargs    x5)    ;argument (aliased to imm2)
    (nargs.w  w5)
    (nfn      x11)   ;temp alias
    (nil-reg  x23)   ;constant NIL (new)
    (rcontext x25)   ;callee saves/special-ish/global
    (t-reg    x24)   ;constant T   (new)
    (temp0    x9)    ;temps: used for branching via function object
    (temp1    x10)   ;       alias to fname
    (temp2    x11)   ;       alias to nfn
    (sp       sp)    ;stack pointer (special)
    (vsp      x28))) ;callee saves/special-ish (val-stack sp)

;;; In numerical order
;; (arg-z    x0)    ;parameter/tagged/return value
;; (arg-z.w     w0)
;; (arg-y    x1)    ;parameter/tagged
;; (arg-y.w     w1)
;; (arg-x    x2)    ;parameter/tagged
;; (arg-x.w     w2)

;; (imm0     x3)    ;un-tagged scratch (subprim arg)
;; (imm0.w      w3)
;; (imm1     x4)    ;un-tagged scratch
;; (imm1.w      w4)
;; (imm2     x5)    ;un-tagged scratch (aliased to nargs)
;; (nargs       x5) ;argument (aliased to imm2)
;; (imm2.w      w5)
;; (nargs.w     w5)
;;           x6-x8  ;UNUSED
;; (temp0    x9)    ;temp for branching via function object
;; (temp1    x10)   ;temp alias to fname
;;    (fname    x10)
;; (temp2    x11)   ;temp alias to nfn
;;    (nfn      x11)
;;          x12-x22 ;UNUSED
;; (nil-reg  x23)   ;constant NIL (new)
;; (t-reg    x24)   ;constant T   (new)
;; (rcontext x25)   ;callee saves/special-ish/global
;; (fn       x26)   ;callee saves/special-ish
;; (allocptr x27)   ;callee saves/special-ish/global
;; (vsp      x28)   ;callee saves/special-ish (val-stack sp)
;; (fp       x29)   ;frame pointer (special)
;; (lr       x30)   ;link register (special)
;; (sp       sp)    ;stack pointer (special)


(defun map-register-name-to-number (reg-name)
  "Turn symbolic register name into x0 or w0, etc."
  (let ((reg-num (cadr (assoc reg-name *lap-to-as-register-map*))))
    (if (null reg-num)
        (error "unknown reg name ~a" reg-name)
        reg-num)))

(defun lap-register-to-as (register-name &optional (stream t))
  "Emit register name as numeric register"
  (format stream "~A" (map-register-name-to-number register-name))) ;was: underscorize-symbol

(defun lap-indexed-address-mode-to-as (base offset &optional (stream t))
  "Emit addressing mode, catch Lisp constants and index into constants vector"
  (cond ((quoted? offset)
         (lap-lisp-constant-reference base offset stream))
        ((null offset)
         (format stream "[~A]"
                 (lap-argument-to-as base nil nil)))
        (t
         (format stream "[~A, ~A]"
                 (lap-argument-to-as base nil nil)
                 (lap-argument-to-as offset nil nil)))))

(defun quoted? (x)
  (cl:and (listp x)
       (eq 'quote (car x))))

(defun lap-lisp-constant-reference (base offset &optional (stream t))
  "Emit index into constants vector"
  (format stream "[~A, ~A]" ;Was: "[~A,#~A]"
	  (lap-argument-to-as base nil nil)
          (- (local-constant-offset (cadr offset)) ;this was wrong by itself, it needs a tag-remover (next line)
             fulltag-nodeheader-0))) ;+++ kinda too bad this doesn't appear symbolically in the code (it could)

(defun lap-pre-increment-address-mode-to-as (base offset &optional (stream t))
  "Emit *(++p)"
  (format stream "[~A, ~A]!"
	  (lap-argument-to-as base nil nil)
	  (lap-argument-to-as offset nil nil)))

(defun lap-post-increment-address-mode-to-as (base offset &optional (stream t))
  "Emit *(p++), horrible syntax"
  (format stream "[~A], ~A"
	  (lap-argument-to-as base nil nil)
	  (lap-argument-to-as offset nil nil)))