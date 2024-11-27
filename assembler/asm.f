#!/bin/env jamforth

0 VALUE B0
0 VALUE BLEN

VARIABLE BHERE \ BYTES

DEC

26 CONSTANT X
27 CONSTANT XH
28 CONSTANT Y
29 CONSTANT YH
30 CONSTANT Z
31 CONSTANT ZH

HEX

0 CONSTANT CF
1 CONSTANT ZF
2 CONSTANT NF
3 CONSTANT VF
4 CONSTANT SF
5 CONSTANT HF
6 CONSTANT TF
7 CONSTANT GIF

: Z? ZF 1 ;
: NZ? ZF 0 ;

: SET 1 ;
: CLEAR 0 ;

: REGERR ." REG ERR: " TEXTERR ;
: RANGEERR ." RANGE ERR: " TEXTERR ;

VARIABLE RHERE
: RALLOT ( bytes -- ptr )
	RHERE @ TUCK
	+ RHERE !
;
: RVARIABLE
	1 RALLOT CONSTANT
;
: RSVARIABLE
	2 RALLOT CONSTANT
;

H# 4000 VALUE FLASHLEN

: INIT_BUF ( u )
	DUP TO FLASHLEN
	2*
	DUP MORECORE DROP
	HERE @ SWAP
	DUP HERE +! ALIGN
	TO BLEN
	TO B0
	0 BHERE !
;

: B, ( w -- )
	BHERE @ 1+ -2 AND
	( w bhere_aligned )

	TUCK B0 + S!
	2 + BHERE !
;
: BC, ( c -- )
	B0 BHERE @ + C!
	1 BHERE +!
;
: BRESV ( words -- )
	2* BHERE +!
;

\ aligns by byte, not word
: BALIGN ( log2(align) -- )
	1 SWAP << 1-
	DUP
	BHERE @
	+
	SWAP INVERT AND
	BHERE !
;

: $ BHERE @ 2/ ;

: B> 2* B0 + ;
: >B B0 - 2/ ;

: (2REG) ( rr rd opcode -- ins16 )
	-ROT SWAP
	DUP F AND
	SWAP [ F INVERT ], AND 5 << OR
	SWAP 4 << OR OR
;
: MK2REG ( opcode -- )
	\ opcode must be already shifted into place
	WORD CREATE DOCOL ,
	[COMPILE] LITERAL
	[[ (2REG) B, EXIT ]]
;

: (1REG) ( rd opcode -- ins16 )
	SWAP 4 << OR
;
: MK1REG ( opcode -- )
	WORD CREATE DOCOL ,
	[COMPILE] LITERAL
	[[ (1REG) B, EXIT ]]
;

: (IMM) ( imm rd opcode -- ins16 )
	-ROT
	DUP 10 < IF
		REGERR
	THEN
	F AND 4 <<
	SWAP
	
	( imm )
	DUP [ FF INVERT ], AND
	DUP 0= SWAP [ FF INVERT ], = OR UNLESS
		RANGEERR
	THEN
	DUP F AND SWAP
	F0 AND 4 <<
	OR OR OR
;
: MKIMM ( opcode -- )
	WORD CREATE DOCOL ,
	[COMPILE] LITERAL
	[[ (IMM) B, EXIT ]]
;

: CAN_RBRANCH ( to from -- bool )
	[ DEC ]
	-
	2048 + FLASHLEN UMOD 2048 -
	1- -2047 2048 WITHIN
;

10 FLASHLEN CAN_RBRANCH . CR
10 H# 2000 CAN_RBRANCH . CR

: (BRANCH) ( to from opcode -- ins16 )
	-ROT
	- 1-
	[ DEC ]
	DUP -2047 2048 WITHIN UNLESS
		RANGEERR
	THEN
	[ HEX ]
	FFF AND OR
;
: (RJMP) C000 (BRANCH) ;
: RJMP $ (RJMP) B, ;
: (RCALL) D000 (BRANCH) ;
: RCALL $ (RCALL) B, ;

: (JMP) 940C SWAP ;
: JMP ( to )
	940C B,
	B,
;
: (CALL) 940E SWAP ;
: CALL ( to )
	940E B,
	B,
;

: PATCH_CBRANCH ( to from ins16 -- ins16 )
	F407 AND
	-ROT - 1-
	DUP -3F 40 WITHIN UNLESS
		RANGEERR
	THEN
	3 <<
	3FF AND OR
;

: (CBRANCH) ( to from flag state -- ins16 )
	NOT A << OR F000 OR
	PATCH_CBRANCH
;
: (CBRANCH2) $ SWAP PATCH_CBRANCH B, ;
: MKCBRANCH ( flag state )
	NOT A << OR F000 OR
	WORD CREATE DOCOL ,
	[COMPILE] LITERAL
	[[ (CBRANCH2) EXIT ]]
;


: LDS ( addr rd )
	9000 (1REG)
	B, B,
;
: STS ( rr addr )
	SWAP
	9200 (1REG)
	B, B,
;

: (MEMD) ( q ins16 -- ins16 )
	SWAP DUP 3F > IF
		RANGEERR
	THEN
	DUP 7 AND SWAP
	DUP 18 AND 7 << SWAP
	20 AND 8 <<
	OR OR OR
;

: JAND IMMEDIATE ' AND , ;
: JS" [COMPILE] S" ;

LATEST @

: MOVW ( from to )
	DUP 1 AND IFTHEN REGERR
	4 <<
	SWAP
	DUP 1 AND IFTHEN REGERR
	OR 100 OR B,
;

: CLF ( flag -- )
	4 << 9488 OR B,
;
: SEF
	4 << 9408 OR B,
;

: LOCK GIF CLF ;
: UNLOCK GIF SEF ;

: IN
	B000 (1REG) SWAP
	DUP 3F > IF
		RANGEERR
	THEN
	DUP F AND SWAP 30 AND 5 << OR OR B,
;
: OUT ( r io -- )
	DUP 3F > IF
		RANGEERR
	THEN
	DUP F AND SWAP 30 AND 5 << OR
	B800 OR (1REG) B,
;

: MULS
	DUP 10 < IF
		REGERR
	THEN
	F JAND
	SWAP DUP 10 < IF
		REGERR
	THEN
	F JAND
	SWAP
	0200 (2REG) B,
;

: WBI ( A b s )
	9 << OR
	SWAP DUP 20 >= IF
		RANGEERR
	THEN
	3 << OR
	9800 OR B,
;
: SBI SET WBI ;
: CBI CLEAR WBI ;

: STBI ( A b state )
	.S CR
	9 << OR
	SWAP DUP 20 >= IF
		RANGEERR
	THEN
	3 << OR
	9900 OR B,
;
: STBR ( r b state )
	9 << OR
	SWAP
	4 << OR
	FC00 OR B,
;

: S" ( -- len )
	0
	BEGIN
		KEY DUP [CHAR] " <>
	WHILE
		BC, 1+
	REPEAT
	DROP
;
: Z" S" DROP 0 BC, ;

: ALIGN
	BHERE @ 1+ -2 JAND
	BHERE !
;
: XALIGN ( log2 of align -- )
	1 SWAP << 1- DUP .S CR
	BHERE @ + .S CR SWAP INVERT JAND .S CR
	BHERE !
;

: IF ( flag state )
	NOT
	2DUP \ saves 3 cells of compiled code over 0 -ROT 0 -ROT
	(CBRANCH) \ cbranches from nonsense to nonsense, but that's ok because we only care about the condition
	$ SWAP B, \ compiles a nonsense spacer
;
: THEN
	( from )
	DUP 2* B0 +
	( from *ins )
	TUCK S@
	( *ins from ins )
	$ -ROT PATCH_CBRANCH SWAP S!
;

: ELSE
	$ 0 B, SWAP THEN
;
: ETHEN
	( from )
	DUP $ SWAP (RJMP)
	SWAP 2* B0 + S!
;

: BEGIN $ ;
: AGAIN RJMP ;
: CONTINUE DUP RJMP ;

: BR (CBRANCH) B, ;
: UNTIL ( to flag state )
	NOT
	$ -ROT BR
;

: WHILE IF SWAP ;
: REPEAT ( cond head )
	RJMP THEN
;

DEC
: ADIW ( imm reg -- )
	DUP 24 32 WITHIN ULTHEN REGERR
	24 - 2/ 4 <<
	SWAP
	DUP 0 64 WITHIN ULTHEN RANGEERR
	TUCK 15 AND OR SWAP
	2 << H# C0 AND OR
	H# 9600 OR B,
;
: SBIW ( imm reg -- )
	DUP 24 32 WITHIN ULTHEN REGERR
	24 - 2/ 4 <<
	SWAP
	DUP 0 64 WITHIN ULTHEN RANGEERR
	TUCK 15 AND OR SWAP
	2 << H# C0 AND OR
	H# 9700 OR B,
;


HEX
1C00 MK2REG ADC
0C00 MK2REG ADD
2000 MK2REG AND
7000 MKIMM ANDI
9405 MK1REG ASR

1400 MK2REG CP
0400 MK2REG CPC
3000 MKIMM CPI
0100 MK2REG CPSE

940A MK1REG DEC
2400 MK2REG EOR
2400 MK2REG XOR
: CLR DUP XOR ;

9403 MK1REG INC

: IJMP 9409 B, ;

900C MK1REG LDX
900D MK1REG LDX+
900E MK1REG LDX-

8008 MK1REG LDY
9009 MK1REG LDY+
900A MK1REG LDY-
: LDDY 8008 (1REG) (MEMD) B, ;

8000 MK1REG LDZ
9001 MK1REG LDZ+
9002 MK1REG LDZ-
: LDDZ 8000 (1REG) (MEMD) B, ;

E000 MKIMM LDI

9004 MK1REG LPM
9005 MK1REG LPM+

: LSL DUP 0C00 (2REG) B, ;
9406 MK1REG LSR

2C00 MK2REG MOV
: MOVW 2/ SWAP 2/ SWAP 0100 (2REG) B, ;

9C00 MK2REG MUL

9401 MK1REG NEG
: NOP 0 B, ;

2800 MK2REG OR
5000 MKIMM ORI

900F MK1REG POP
920F MK1REG PUSH

: RET 9508 B, ;
: RETI 9518 B, ;

: ROL DUP 1C00 (2REG) B, ;
9407 MK1REG ROR

0800 MK2REG SBC
8000 MKIMM SBCI

: SLEEP 9588 B, ;

920C MK1REG STX
920D MK1REG STX+
920E MK1REG STX-

8208 MK1REG STY
9209 MK1REG STY+
920A MK1REG STY-
: STDY 8208 (1REG) (MEMD) B, ;

8200 MK1REG STZ
9201 MK1REG STZ+
9202 MK1REG STZ-
: STDZ 8200 (1REG) (MEMD) B, ;

1800 MK2REG SUB
5000 MKIMM SUBI
: ADDI SWAP NEGATE SWAP SUBI ;

9402 MK1REG NSWAP

: TST DUP AND ;

9204 MK1REG XCH

: SENTINEL ;

: { IMMEDIATE
	LITERAL [ HERE @ 14 - ], !
;
: } IMMEDIATE
	[ LATEST @ @ @ ], [ LATEST @ @ ], !
;

JS" RET" FIND >CFA
JS" SENTINEL" FIND

{
: {} IMMEDIATE 
	[COMPILE] }
	WORD FIND >CFA ,
	[COMPILE] {
;

: ACREATE
	LATEST @ -ROT
	CREATE
	LATEST @ SWAP LATEST ! \ remove new word from the dictionary
	LITERAL \ pointer to sentinel
	TUCK @ OVER ! SWAP !
;

: (SUBROUTINE) ( to -- )
	\ compiles either rcall or long call depending on distance
	DUP $ - 1-
	[ DEC ]
	-2047 2048 WITHIN UNLESS
		CALL
	ELSE
		RCALL
	THEN
;
: SUBROUTINE	\ TODO: split into 3
	WORD
	\." label " 2DUP TELL CR
	\.S CR
	ACREATE
	DOCOL ,
	\.S CR
	[COMPILE] LITERAL
	' (SUBROUTINE) ,
	' EXIT ,
;

: (ROUTINE) ( to from -- rjmp 0 | jmp dest )
	2DUP - 1-
	[ DEC ]
	-2047 2048 WITHIN UNLESS
		DROP (JMP)
	ELSE
		(RJMP) 0
	THEN
;
: ROUTINE ( to -- )
	\ compiles either rjmp or long jmp depending on distance
	$ (ROUTINE) ?DUP IF
		SWAP B, B,
	ELSE
		B,
	THEN
;
: MKROUTINE
	\." at " DUP .
	WORD
	\2DUP ." make routine " TELL CR
	ACREATE
	DOCOL ,
	[COMPILE] LITERAL
	' ROUTINE ,
	' EXIT ,
;

: MK:;'
	S" :" ACREATE DOCOL , [[ $ SUBROUTINE EXIT ]]
	S" ;" ACREATE DOCOL , LITERAL , ' EXIT ,
	S" '" ACREATE DOCOL , [[ WORD FIND >CFA 8 + @ EXIT ]]
;
: R: $ MKROUTINE ;
: I; {} RETI ;

: FINISH
	CR
	BHERE @ . ." bytes, " $ . ." words" CR
	B0 BHERE @ 3 WRITE-FILE BYE ;

: V! ( to from -- ) \ TODO: make this sometimes long jump
	TUCK (RJMP) SWAP 2* B0 + S!
;

: RESET! 0 V! ;

: LOBYTE H# FF AND ;
: HIBYTE 8 >> LOBYTE ;

DECIMAL

16 VALUE CPU_MHZ

: OUTI
	SWAP
	16 {} LDI
	16 SWAP {} OUT
;
: STSI
	SWAP
	16 {} LDI
	16 SWAP {} STS
;

: LDIW ( w reg -- )
	2DUP
	SWAP H# FF AND
	SWAP {} LDI

	1+
	SWAP 8 >>
	SWAP {} LDI
;
: LDIFZ 2* Z LDIW ;

: LDSW ( addr reg -- )
	2DUP 	\ read low first
	{} LDS
	1+ SWAP 1+ SWAP {} LDS
;
: STSW ( reg addr -- )
	2DUP
	1+ SWAP 1+ SWAP {} STS	\ write high first
	{} STS
;

: LDZW ( reg )
	DUP {} LDZ
	1 SWAP 1+ {} LDDZ
;
: STZW ( reg )
	1 OVER 1+ {} STDZ
	{} STZ
;

: LDDZW ( q reg -- )
	2DUP {} LDDZ
	1+ SWAP 1+ SWAP {} LDDZ
;
: STDZW ( q reg -- )
	2DUP
	1+ SWAP 1+ SWAP {} STDZ
	{} STDZ
;

: LDYW ( reg )
	DUP {} LDY
	1 SWAP 1+ {} LDDY
;
: STYW ( reg )
	1 OVER 1+ {} STDY
	{} STY
;

: LDDYW ( q reg -- )
	2DUP {} LDDY
	1+ SWAP 1+ SWAP {} LDDY
;
: STDYW ( q reg -- )
	2DUP
	1+ SWAP 1+ SWAP {} STDY
	{} STDY
;
}

MK:;'

\{ HERE @ SWAP - DEC . HEX }
