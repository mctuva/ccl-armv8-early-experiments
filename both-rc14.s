        .arch armv8-a
        .file "/home/tim/both-rc14.s"
        .equ uuo_error_wrong_nargs, 1
        .equ uuo_alloc_trap, 2
        .equ uuo_error_reg_not_list, 3
        .equ uuo_unimplemented_macro, 4
        .text
        .align	2
        .global	expt_caller_2_3
        .type	expt_caller_2_3, %function

expt_caller_2_3:
        .cfi_startproc
        cmp x5, 0
        beq .l49
        .word uuo_error_wrong_nargs 
.l49:
        str x29, [sp, -16]!
        mov x29, sp
        str x28, [sp, -16]!
        stp x26, x30, [sp, -16]!
        mov x26, x11
        mov w1, 16
        mov w0, 24
        mov x5, 16
        ldr x10, [x26, 11]
        ldp x26, x30, [sp], 16
        ldr x28, [sp], 16
        mov sp, x29
        ldr x29, [sp], 16
        ldr x11, [x10, 10]
        ldr x9, [x11, 3]
        br x9
        .cfi_endproc
        .size	expt_caller_2_3, .-expt_caller_2_3

        .text
        .align	2
        .global	my_expt
        .type	my_expt, %function

my_expt:
        .cfi_startproc
.l0:
        cmp x5, 16
        beq .l12
        .word uuo_error_wrong_nargs 
.l12:
        str x29, [sp, -16]!
        mov x29, sp
        str x28, [sp, -16]!
        stp x26, x30, [sp, -16]!
        mov x26, x11
        stp x0, x1, [x28, -16]!
        tst w0, 7
        bne .l48
        cmp x0, 0
        beq .l68
        b .l76
.l48:
        mov w1, 0
        bl spbuiltin_eq
        cmp x0, x23
        beq .l76
.l68:
        mov w0, 8
        ldp x26, x30, [sp], 16
        ldr x28, [sp], 16
        mov sp, x29
        ldr x29, [sp], 16
        ret 
.l76:
        ldr x1, [x28, 0]
        tst w0, 7
        bne .l112
        subs x0, x0, 8
        bvc .l120
        bl spfix_overflow
        b .l120
.l112:
        bl spbuiltin_minus
.l120:
        ldr x1, [x28, 8]
        mov x5, 16
        mov x11, x26
        bl .l0
        ldr x1, [x28, 8]
        ldp x26, x30, [sp], 16
        ldr x28, [sp], 16
        mov sp, x29
        ldr x29, [sp], 16
        b spbuiltin_times
        .cfi_endproc
        .size	my_expt, .-my_expt

        .text
        .align	2
        .global	spbuiltin_times
        .type	spbuiltin_times, %function

spbuiltin_times:
        .cfi_startproc
        orr w3, w1, w0
        tst w3, 7
        bne .l2_x
        asr x3, x0, 3
        asr x4, x1, 3
        smull x3, w4, w3
        adds x3, x3, x3
        bvs .l1_x
        adds x3, x3, x3
        bvs .l1_x
        adds x3, x3, x3
        bvs .l1_x
        mov x0, x3
        ret 
.l1_x:
        asr x3, x0, 3
        asr x4, x1, 3
        smulh x4, x4, x3
        b spmakes128
.l2_x:
        .word uuo_unimplemented_macro 
        .cfi_endproc
        .size	spbuiltin_times, .-spbuiltin_times

        .text
        .align	2
        .global	spbuiltin_eq
        .type	spbuiltin_eq, %function

spbuiltin_eq:
        .cfi_startproc
        ret 
        .cfi_endproc
        .size	spbuiltin_eq, .-spbuiltin_eq

        .text
        .align	2
        .global	spfix_overflow
        .type	spfix_overflow, %function

spfix_overflow:
        .cfi_startproc
        ret 
        .cfi_endproc
        .size	spfix_overflow, .-spfix_overflow

        .text
        .align	2
        .global	spbuiltin_minus
        .type	spbuiltin_minus, %function

spbuiltin_minus:
        .cfi_startproc
        ret 
        .cfi_endproc
        .size	spbuiltin_minus, .-spbuiltin_minus

        .text
        .align	2
        .global	spmakes128
        .type	spmakes128, %function

spmakes128:
        .cfi_startproc
        ret 
        .cfi_endproc
        .size	spmakes128, .-spmakes128

        .text
        .align	2
        .global	call_list2
        .type	call_list2, %function

call_list2:
        .cfi_startproc
        cmp x5, 0
        beq .l12b
        .word uuo_error_wrong_nargs 
.l12b:
        str x29, [sp, -16]!
        mov x29, sp
        str x28, [sp, -16]!
        stp x26, x30, [sp, -16]!
        mov x26, x11
        mov w1, 8
        mov w0, 16
        mov w5, 16
        ldr x10, [x26, 11]
        ldr x11, [x10, 10]
        ldr x9, [x11, 3]
        blr x9
        str x0, [x28, -8]!
        mov x2, x0
        mov w1, 8
        mov w0, 16
        mov x5, 24
        ldr x10, [x26, 19]
        ldp x26, x30, [sp], 16
        ldr x28, [sp], 16
        mov sp, x29
        ldr x29, [sp], 16
        ldr x11, [x10, 10]
        ldr x9, [x11, 3]
        br x9
        .cfi_endproc
        .size	call_list2, .-call_list2

        .text
        .align	2
        .global	list2
        .type	list2, %function

list2:
        .cfi_startproc
        cmp x5, 16
        beq .l137
        .word uuo_error_wrong_nargs 
.l137:
        str x29, [sp, -16]!
        mov x29, sp
        str x28, [sp, -16]!
        stp x26, x30, [sp, -16]!
        mov x26, x11
        stp x0, x1, [x28, -16]!
        ldr x1, [x28, 0]
        mov x0, x23
        sub x27, x27, 13
        ldr x3, [x25, 224]
        cmp x27, x3
        bhi .l53
        .word uuo_alloc_trap 
.l53:
        str x1, [x27, 5]
        str x0, [x27, -3]
        mov x0, x27
        bic x27, x27, 15
        str x0, [x28, -8]!
        ldr x0, [x28, 16]
        ldr x2, [x28, 0]
        sub x27, x27, 13
        ldr x3, [x25, 224]
        cmp x27, x3
        bhi .l107
        .word uuo_alloc_trap 
.l107:
        str x0, [x27, 5]
        str x2, [x27, -3]
        mov x0, x27
        bic x27, x27, 15
        ldp x26, x30, [sp], 16
        ldr x28, [sp], 16
        mov sp, x29
        ldr x29, [sp], 16
        ret 
        .cfi_endproc
        .size	list2, .-list2

        .text
        .align	2
        .global	verify_list2
        .type	verify_list2, %function

verify_list2:
        .cfi_startproc
        cmp x5, 24
        beq .l13
        .word uuo_error_wrong_nargs 
.l13:
        str x29, [sp, -16]!
        mov x29, sp
        str x28, [sp, -16]!
        stp x26, x30, [sp, -16]!
        mov x26, x11
        str x2, [x28, -8]!
        str x1, [x28, -8]!
        str x0, [x28, -8]!
        and w3, w2, 15
        cmp w3, 3
        bne .l265
        and w3, w2, 7
        cmp w3, 3
        beq .l57
        .word uuo_error_reg_not_list 
.l57:
        ldr x1, [x2, 5]
        ldr x0, [x28, 8]
        bl spbuiltin_eql
        cmp x0, x23
        beq .l257
        ldr x0, [x28, 16]
        and w3, w0, 7
        cmp w3, 3
        beq .l101
        .word uuo_error_reg_not_list 
.l101:
        ldr x0, [x0, -3]
        and w3, w0, 15
        cmp w3, 3
        bne .l249
        ldr x0, [x28, 16]
        and w3, w0, 7
        cmp w3, 3
        beq .l138
        .word uuo_error_reg_not_list 
.l138:
        ldr x0, [x0, -3]
        and w3, w0, 7
        cmp w3, 3
        beq .l157
        .word uuo_error_reg_not_list 
.l157:
        ldr x1, [x0, 5]
        ldr x0, [x28, 0]
        bl spbuiltin_eql
        cmp x0, x23
        beq .l241
        ldr x0, [x28, 16]
        and w3, w0, 7
        cmp w3, 3
        beq .l201
        .word uuo_error_reg_not_list 
.l201:
        ldr x0, [x0, -3]
        and w3, w0, 7
        cmp w3, 3
        beq .l221
        .word uuo_error_reg_not_list 
.l221:
        ldr x0, [x0, -3]
        cmp x0, x23
        mov x0, x23
        bne .l245
        mov x0, x24
        b .l245
.l241:
        mov x0, x23
.l245:
        ldp x26, x30, [sp], 16
        ldr x28, [sp], 16
        mov sp, x29
        ldr x29, [sp], 16
        ret 
.l249:
        mov x0, x23
        ldp x26, x30, [sp], 16
        ldr x28, [sp], 16
        mov sp, x29
        ldr x29, [sp], 16
        ret 
.l257:
        mov x0, x23
        ldp x26, x30, [sp], 16
        ldr x28, [sp], 16
        mov sp, x29
        ldr x29, [sp], 16
        ret 
.l265:
        mov x0, x23
        ldp x26, x30, [sp], 16
        ldr x28, [sp], 16
        mov sp, x29
        ldr x29, [sp], 16
        ret 
        .cfi_endproc
        .size	verify_list2, .-verify_list2

        .text
        .align	2
        .global	spbuiltin_eql
        .type	spbuiltin_eql, %function

spbuiltin_eql:
        .cfi_startproc
        cmp x0, x1
        bne not_equal
        mov x0, x24
        ret 
not_equal:
        mov x0, x23
        ret 
        .cfi_endproc
        .size	spbuiltin_eql, .-spbuiltin_eql

/*
EXPT-TOP-LEVEL: expected: 8 actual: 8 pass? T
LIST-TOP-LEVEL: expected: T actual: T pass? T
*/
        .equ fulltag_even_fixnum, 0
        .equ fulltag_imm_0, 1
        .equ fulltag_imm_1, 2
        .equ fulltag_cons, 3
        .equ fulltag_tra_0, 4
        .equ fulltag_nodeheader_0, 5
        .equ fulltag_nodeheader_1, 6
        .equ fulltag_immheader_0, 7
        .equ fulltag_odd_fixnum, 8
        .equ fulltag_immheader_1, 9
        .equ fulltag_immheader_2, 10
        .equ fulltag_nil, 11
        .equ fulltag_tra_1, 12
        .equ fulltag_misc, 13
        .equ fulltag_symbol, 14
        .equ fulltag_function, 15

        .data

        .bss
        .align  4
alloc_region_1:
        .type   alloc_region_1, %object
        .size   alloc_region_1, 1024
        .org alloc_region_1 + 1024
value_stack_base:
        .data
        .align 4

        .bss
        .align  4
alloc_limit:
        .type   alloc_limit, %object
        .size   alloc_limit, 1024
        .org alloc_limit + 1024
alloc_base:
        .data
        .align 4
tcr:
        .ascii "The TCR"
        .zero 1
        .zero 208
        .xword alloc_base
        .xword alloc_limit

        .bss
        .align  4
alloc_region_2:
        .type   alloc_region_2, %object
        .size   alloc_region_2, 1024
        .org alloc_region_2 + 1024
runtime_objects_base:
        .data
        .align 4
nil_value:
        .ascii "NIL-head"
        .ascii "NIL"
        .zero 5
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
t_value:
        .ascii "T-header"
        .ascii "T"
        .zero 7
        .xword t_value + fulltag_symbol
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil

        .bss
        .align  4
alloc_region_3:
        .type   alloc_region_3, %object
        .size   alloc_region_3, 1024
        .org alloc_region_3 + 1024
stack_base:
        .data
        .align 4
expt_caller_2_3_symbol:
        .ascii "sym-head"
        .ascii "EXPT-CAL"
        .xword nil_value + fulltag_nil
        .xword expt_caller_2_3_function + fulltag_nodeheader_0
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
my_expt_symbol:
        .ascii "sym-head"
        .ascii "MY-EXPT"
        .zero 1
        .xword nil_value + fulltag_nil
        .xword my_expt_function + fulltag_nodeheader_0
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
expt_caller_2_3_function:
        .ascii "fn-heade"
        .xword expt_caller_2_3
        .xword my_expt_symbol + fulltag_symbol
my_expt_function:
        .ascii "fn-heade"
        .xword my_expt
call_list2_symbol:
        .ascii "sym-head"
        .ascii "CALL-LIS"
        .xword nil_value + fulltag_nil
        .xword call_list2_function + fulltag_nodeheader_0
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
list2_symbol:
        .ascii "sym-head"
        .ascii "LIST2"
        .zero 3
        .xword nil_value + fulltag_nil
        .xword list2_function + fulltag_nodeheader_0
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
verify_list2_symbol:
        .ascii "sym-head"
        .ascii "VERIFY-L"
        .xword nil_value + fulltag_nil
        .xword verify_list2_function + fulltag_nodeheader_0
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
        .xword nil_value + fulltag_nil
call_list2_function:
        .ascii "fn-heade"
        .xword call_list2
        .xword list2_symbol + fulltag_symbol
        .xword verify_list2_symbol + fulltag_symbol
list2_function:
        .ascii "fn-heade"
        .xword list2
verify_list2_function:
        .ascii "fn-heade"
        .xword verify_list2

        .text
	.align	2
	.global	init_and_call_expt
	.type	init_and_call_expt, %function

init_and_call_expt:
	.cfi_startproc
        adrp x27, alloc_base
	add x27, x27, :lo12:alloc_base

        adrp x25, tcr
	add x25, x25, :lo12:tcr

        adrp x28, value_stack_base
	add  x28, x28, :lo12:value_stack_base

        adrp x11, my_expt_function
	add  x11, x11, :lo12:my_expt_function
	add  x11, x11, fulltag_nodeheader_0

        adrp x23, nil_value
        add  x23, x23, :lo12:nil_value
        add  x23, x23, fulltag_nil

        adrp x24, t_value
        add  x24, x24, :lo12:t_value
        add  x24, x24, fulltag_symbol

        mov x5, 16
        b my_expt
	.cfi_endproc

	.size	init_and_call_expt, .-init_and_call_expt


        .text
	.align	2
	.global	init_and_call_list2
	.type	init_and_call_list2, %function

init_and_call_list2:
	.cfi_startproc
        adrp x27, alloc_base
	add x27, x27, :lo12:alloc_base

        adrp x25, tcr
	add x25, x25, :lo12:tcr

        adrp x28, value_stack_base
	add  x28, x28, :lo12:value_stack_base

        adrp x11, call_list2_function
	add  x11, x11, :lo12:call_list2_function
	add  x11, x11, fulltag_nodeheader_0

        adrp x23, nil_value
        add  x23, x23, :lo12:nil_value
        add  x23, x23, fulltag_nil

        adrp x24, t_value
        add  x24, x24, :lo12:t_value
        add  x24, x24, fulltag_symbol

        mov x5, 0
        b call_list2
	.cfi_endproc

	.size	init_and_call_list2, .-init_and_call_list2

/* main() is commented-out by default to allow linking with expt.c or list2.c


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
	add  x28, x28, :lo12:value_stack_base

        adrp x11, expt_caller_2_3_function
	add  x11, x11, :lo12:expt_caller_2_3_function
	add  x11, x11, fulltag_nodeheader_0

        adrp x23, nil_value
        add  x23, x23, :lo12:nil_value
        add  x23, x23, fulltag_nil

        adrp x24, t_value
        add  x24, x24, :lo12:t_value
        add  x24, x24, fulltag_symbol

        mov x5, 0
        b expt_caller_2_3
	.cfi_endproc

	.size	main, .-main
EOF */
