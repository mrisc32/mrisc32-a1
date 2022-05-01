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
; A simple program that is useful for basic debugging.
;--------------------------------------------------------------------------------------------------

	.section .text.start, "ax"
	.globl	_start
_start:
	ldi	sp, #0x10000

	ldi	r1, #1
loop:
	add	r2, r1, #7
	mul	r3, r2, #11
	stw	r3, [sp, #-4]	; Should stall, waiting for the mul result.
	add	r1, r1, #1
	slt	r2, r1, #5
	bs	r2, loop

        div     r3, r2, #9      ; Should stall
	b	_start          ; Should give branch correction during stall

