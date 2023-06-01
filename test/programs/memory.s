; -*- mode: mr32asm; tab-width: 8; indent-tabs-mode: t; -*-
;--------------------------------------------------------------------------------------------------
; Copyright (c) 2022 Marcus Geelnard
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
; A program that tests various aspects of memory accesses.
;--------------------------------------------------------------------------------------------------

	.section .text.start, "ax"
	.globl	_start
_start:
	ldi	r1, #0x12345678
	ldi	r2, #0x99887766

outer_loop:

	; A simple store + load loop.
	ldi	sp, #0x10008
	ldi	r14, #3			; r14 = loop counter
loop1:
	add	r3, r1, r2

	stw	r1, [sp, #0]
	stw	r2, [sp, #4]
	stw	r3, [sp, #8]

	ldw	r4, [sp, #0]
	ldw	r5, [sp, #4]
	ldw	r6, [sp, #8]

	seq	r4, r4, r1
	seq	r5, r5, r2
	seq	r6, r6, r3
	and	r5, r5, r6
	and	r4, r4, r5
	bns	r4, fail

	ldh	r7, [sp, #0]
	sth	r7, [sp, #4]
	ldh	r7, [sp, #2]
	sth	r7, [sp, #6]

	add	r1, r1, #0x1234
	add	r2, r2, #0x0123

	add	sp, sp, #12
	add	r14, r14, #-1
	bnz	r14, loop1

	; A vectorized store loop
	ldi	sp, #0x10008
	ldi	fp, #0xc0000000
	ldi	r14, #50		; r14 = element counter
	getsr	vl, #0x10
	ldea	v1, [r1, #1]
loop2:
	min	vl, vl, r14
	sub	r14, r14, vl
	stw	r14, [fp, #12]
	stw	v1, [sp, #4]
	ldw	r6, [fp, #12]
	ldea	sp, [sp, vl*4]
	bnz	r14, loop2

	; A vectorized load loop
	ldi	sp, #0x10008
	ldi	fp, #0xc0000000
	ldi	r14, #50		; r14 = element counter
	getsr	vl, #0x10
loop3:
	min	vl, vl, r14
	sub	r14, r14, vl
	ldw	r6, [fp, #12]
	ldw	v1, [sp, #4]
	ldw	r6, [fp, #12]
	ldea	sp, [sp, vl*4]
	bnz	r14, loop3

	b	outer_loop


;--------------------------------------------------------------------------------------------------
; Test failure.
;--------------------------------------------------------------------------------------------------

fail:
	ldi	r1, #0xbaadbeef
	ldi	sp, #0x10000
1:
	stw	r1, [sp, #0]
	bnz	r1, 1b

