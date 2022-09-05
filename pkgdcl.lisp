;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: CL-USER; Base: 10 -*-

(in-package :CL-USER)

(defpackage :scom (:use :cl)
            (:export #:^ #:$
		     #:*simulated-memory-size*
		     #:*simulated-memory*
		     #:*simulated-memory-pointer*
		     #:*registers*
                     #:*register-names*
		     #:flags
                     #:*this-function*
                     #:%splice
                     #:def-asm-alias
                     #:def-asm-fun
                     #:def-asm-lisp-fun
                     ;;#:def-simulator-code
		     #:macroexpand-lap-tree
		     #:flatten-lap-tree
		     ;;#:initialize-go-thunk-to-lisp-fun
                     #:reg-value
                     #:%b2q
                     #:%q2b
                     #:%q2tagged-b
                     #:%tagged-b2q
		     #:%b2w
                     #:%w2b
                     #:%w2tagged-b
                     #:%tagged-b2w
                     #:alias-register
                     #:bool2bit
                     #:carry-flag-p
                     #:eval-constant
                     ;;#:memory-ref
                     ;;#:memory-set
                     #:overflow-flag-p
                     #:set-flags!
                     #:show-memory
                     #:show-registers
                     #:sim-lookup-symbol
                     #:strip-label
                     #:zero-flag-p
		     #:zero-flag-bit))

(defpackage :sx86 (:use :cl :scom))       ;do we want to :use the SCOM package?

(defpackage :sarm (:use :cl :scom)
            (:shadow #:and))

(defpackage :sav8 (:use :cl :scom)
            (:shadow #:and))
