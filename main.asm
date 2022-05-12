;*******;***********************************************************
;*
;*	Marcus_Wheeler_Lab8_Source
;*
;*	
;*
;*	
;*
;***********************************************************
;*
;*	 Author: Marcus Wheeler
;*	   Date: 11/25/2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		ldi mpr, LOW(RAMEND)
		out SPL, mpr

		ldi mpr, HIGH(RAMEND)
		out SPH, mpr



;***********************************************************
;*	Main Program
;***********************************************************
;***************************************************************************
;* I'm going to use count as my address accumulator, but the LCDDriver names it
;***************************************************************************
MAIN:                ;If I'm in state zero, I only need to poll for my PD0
  ldi mpr, 23
  ldi r17, 32
  sub mpr, r17
  nop
  rjmp MAIN