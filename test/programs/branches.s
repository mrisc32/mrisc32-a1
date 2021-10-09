; -*- mode: mr32asm; tab-width: 8; indent-tabs-mode: t; -*-
;--------------------------------------------------------------------------------------------------
; Copyright (c) 2021 Marcus Geelnard
;
; This software is provided 'as-is', without any express or implied warranty. In no event will the
; authors be held liable for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose, including commercial
; applications, and to alter it and redistribute it freely, subject to the following restrictions:
;
;  1. The origin of this software must not be misrepresented; you must not claim that you wrote
;     the original software. If you use this software in a product, an acknowledgment in the
;     product documentation would be appreciated but is not required.
;
;  2. Altered source versions must be plainly marked as such, and must not be misrepresented as
;     being the original software.
;
;  3. This notice may not be removed or altered from any source distribution.
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; This program exerchises various forms of branches.
;--------------------------------------------------------------------------------------------------

	.text
	.globl	_start
_start:
	ldi	sp, #0x10000
	bl	loop_x2
	bl	calls1
	bl	calls2
	b	_start

loop_x2:
	ldi	r1, #3			; Outer loop count
1:	ldi	r2, #5			; Inner loop count
2:	slt	r3, r2, #2
	bs	r3, 3f			; ~50% taken forward branch
	stw	r2, [sp, #-4]		; Data port access (may result in WB stall)
3:	add	r2, r2, #-1
	bnz	r2, 2b			; Inner loop backward branch (likely taken)
	add	r1, r1, #-1
	bnz	r1, 1b			; Outer loop backward branch (likely taken)
	ret				; Return branch (100% taken)

calls1:
	mov	r20, lr
	ldi	r1, #3
1:	bl	subroutine1		; 100% taken subroutine call.
	bl	subroutine2		; 100% taken subroutine call.
	add	r1, r1, #-1
	bnz	r1, 1b
	mov	lr, r20
	ret

calls2:
	mov	r20, lr
	ldi	r1, #3
1:	bl	subroutine1		; 100% taken subroutine call.
	bl	subroutine2		; 100% taken subroutine call.
	bl	subroutine1		; Call again to mess up return BTB entry.
	bl	subroutine2		; Call again to mess up return BTB entry.
	add	r1, r1, #-1
	bnz	r1, 1b
	mov	lr, r20
	b	subroutine2		; Tail call (100% taken)

subroutine1:
	; Return directly after call instruction.
	ret				; Return branch (100% taken)

subroutine2:
	nop
	nop
	nop
	nop
	nop
	ret				; Return branch (100% taken)

