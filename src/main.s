.define palette			$6800
.define screen1			$7000
.define emptyscreen		$7800
.define emptychar		$c000
.define twistcharset	$10000
.define moddata			$18000
.define bowlcharset		$40000
.define destcharset		$50000

.segment "MAIN"

entry_main

		sei

		lda #$35
		sta $01

		lda #%10000000									; Clear bit 7 - HOTREG
		trb $d05d

		lda #$00										; unmap
		tax
		tay
		taz
		map
		eom

		lda #$47										; enable C65GS/VIC-IV IO registers
		sta $d02f
		lda #$53
		sta $d02f
		eom

		lda #%10000000									; force PAL mode, because I can't be bothered with fixing it for NTSC
		trb $d06f										; clear bit 7 for PAL ; trb $d06f 
		;tsb $d06f										; set bit 7 for NTSC  ; tsb $d06f

		lda #$41										; enable 40MHz
		sta $00

		;lda #$70										; Disable C65 rom protection using hypervisor trap (see mega65 manual)
		;sta $d640
		;eom

		lda #%11111000									; unmap c65 roms $d030 by clearing bits 3-7
		trb $d030

		lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
		sta $d054

		lda #%10100000									; CLEAR bit7=40 column, bit5=Enable extended attributes and 8 bit colour entries
		trb $d031

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		ldx #$00
		lda #$00
:		sta emptychar,x
		inx
		cpx #64
		bne :-

		ldx #$00
:		lda #<(emptychar/64)
		sta screen1+0*$0100+0,x
		sta screen1+1*$0100+0,x
		sta screen1+2*$0100+0,x
		sta screen1+3*$0100+0,x
		sta screen1+4*$0100+0,x
		sta screen1+5*$0100+0,x
		sta screen1+6*$0100+0,x
		sta screen1+7*$0100+0,x
		lda #>(emptychar/64)
		sta screen1+0*$0100+1,x
		sta screen1+1*$0100+1,x
		sta screen1+2*$0100+1,x
		sta screen1+3*$0100+1,x
		sta screen1+4*$0100+1,x
		sta screen1+5*$0100+1,x
		sta screen1+6*$0100+1,x
		sta screen1+7*$0100+1,x
		inx
		inx
		bne :-

		DMA_RUN_JOB clearcolorramjob

		lda #<.loword(screen1)							; set pointer to screen ram
		sta $d060
		lda #>.loword(screen1)
		sta $d061
		lda #<.hiword(screen1)
		sta $d062
		lda #>.hiword(screen1)
		sta $d063

		lda #<$0800										; set (offset!) pointer to colour ram
		sta $d064
		lda #>$0800
		sta $d065

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$00
		sta $d012
		lda #<fastload_irq_handler
		sta $fffe
		lda #>fastload_irq_handler
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli

		jsr fl_init
		jsr fl_waiting
		FLOPPY_IFFL_FAST_LOAD_INIT "XMAS65.IFFLCRCH"
		FLOPPY_IFFL_FAST_LOAD ; twistcharset
		FLOPPY_IFFL_FAST_LOAD ; bowlcharset
		FLOPPY_IFFL_FAST_LOAD ; palette
		FLOPPY_IFFL_FAST_LOAD ; moddata
		jsr fl_exit

		sei

		lda #$35
		sta $01

		lda #<.loword(moddata)
		sta adrPepMODL+0
		lda #>.loword(moddata)
		sta adrPepMODL+1
		lda #<.hiword(moddata)
		sta adrPepMODH+0
		lda #>.hiword(moddata)
		sta adrPepMODH+1

		jsr peppitoInit

		lda #<screen1										; set pointer to screen ram
		sta $d060
		lda #>screen1
		sta $d061
		lda #(screen1 & $ff0000) >> 16
		sta $d062
		lda #$00
		sta $d063

		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

		ldx #$00										; set bitmap palette
:		lda palette+0*$0100,x
		sta $d100,x
		lda palette+1*$0100,x
		sta $d200,x
		lda palette+2*$0100,x
		sta $d300,x
		inx
		bne :-

		lda $d070
		and #%11001111									; clear bits 4 and 5 (BTPALSEL) so bitmap uses palette 0
		sta $d070

		lda #$00
		sta screenrow
		sta screencolumn

		ldx #<(destcharset/64)
		ldy #>(destcharset/64)

put0	stx screen1+0
put1	sty screen1+1

		clc
		txa
		adc #$01
		tax
		tya
		adc #$00
		tay

		clc
		lda put0+1
		adc #80
		sta put0+1
		lda put0+2
		adc #0
		sta put0+2

		clc
		lda put1+1
		adc #80
		sta put1+1
		lda put1+2
		adc #0
		sta put1+2

		inc screenrow
		lda screenrow
		cmp #25
		bne put0

		lda #0
		sta screenrow
		inc screencolumn
		inc screencolumn
		lda screencolumn
		cmp #80
		beq endscreenplot

		lda #>screen1
		sta put0+2
		sta put1+2
		clc
		lda screencolumn
		sta put0+1
		adc #$01
		sta put1+1

		jmp put0

endscreenplot

		lda #$7f										; disable CIA
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$ff										; setup IRQ interrupt
		sta $d012
		lda #<irq1
		sta $fffe
		lda #>irq1
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli

loop
		lda $d72a
		lda $d72a
		lda $d72a
		lda $d72a
		; inc $5000
		jmp loop

; ----------------------------------------------------------------------------------------------------

.align 256

irq1
		php
		pha
		txa
		pha
		tya
		pha

		lda #$01
		sta $d020
		sta $d021

		jsr peppitoPlay

		lda #$00
		sta plotcolumnlo
		sta plotcolumnhi

		;inc $d020

		DMA_RUN_JOB copyentirebitmapjob

		ldy sin2add

plotloop

		clc												; update destination column - this doesn't change
		lda #<dstcolumnslo
		adc plotcolumnlo
		sta pl1+1
		lda #>dstcolumnslo
		adc plotcolumnhi
		sta pl1+2

		clc
		lda #<dstcolumnshi
		adc plotcolumnlo
		sta pl2+1
		lda #>dstcolumnshi
		adc plotcolumnhi
		sta pl2+2

pl1		lda dstcolumnslo
		sta dstcopy+0
pl2		lda dstcolumnshi
		sta dstcopy+1

		clc
		lda plotcolumnlo
		adc frame
		lsr
		tax
		lda sine2,x
		lsr
		tax
		clc												; add Y sine offset
		lda times8tablelo,x
		adc dstcopy+0
		sta dstcopy+0
		lda times8tablehi,x
		adc dstcopy+1
		sta dstcopy+1


		ldx plotcolumnlo								; update source columns - 
		clc
		lda sine,x
		adc sine,y
		clc
		adc frame
		adc frame
		tax

		;lda sine,x
		;sta srcskiplo+1

		lda srccolumnslo,x
		sta srccopy+0
		lda srccolumnshi,x
		sta srccopy+1

		DMA_RUN_JOB copybitmapjob

		iny

		clc
		lda plotcolumnlo
		adc #$01
		sta plotcolumnlo
		lda plotcolumnhi
		adc #$00
		sta plotcolumnhi

		lda plotcolumnhi
		beq :+
		lda plotcolumnlo
		cmp #$40
		beq exitplotloop

:		jmp plotloop

sin2add
		.byte 0


exitplotloop

		;dec $d020

		inc frame

		dec sin2add
		dec sin2add

		lda #$02
		and chan0on
		clc
		adc #$01
		sta $d020
		ldy #$10
		ldx #0
:		lda $c000
		dex
		bne :-
		dey
		bne :-
		lda #$42
		and chan1on
		clc
		adc #$01
		sta $d020
		ldy #$10
		ldx #0
:		lda $c000
		dex
		bne :-
		dey
		bne :-
		lda #$82
		and chan2on
		clc
		adc #$01
		sta $d020
		ldy #$10
		ldx #0
:		lda $c000
		dex
		bne :-
		dey
		bne :-
		lda #$c2
		and chan3on
		clc
		adc #$01
		sta $d020
		ldy #$10
		ldx #0
:		lda $c000
		dex
		bne :-
		dey
		bne :-

		lda #$01
		sta $d020

		pla
		tay
		pla
		tax
		pla
		plp
		asl $d019
		rti

.align 256

sine
.byte 255, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254

.byte 254, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254

sine2

.byte 064, 067, 070, 073, 076, 079, 082, 085, 088, 090, 093, 096, 099, 101, 103, 106, 108, 110, 112, 114, 116, 118, 119, 120, 122, 123, 124, 125, 125, 126, 126, 126
.byte 126, 126, 126, 126, 125, 125, 124, 123, 122, 120, 119, 118, 116, 114, 112, 110, 108, 106, 103, 101, 099, 096, 093, 090, 088, 085, 082, 079, 076, 073, 070, 067
.byte 064, 060, 057, 054, 051, 048, 045, 042, 039, 037, 034, 031, 028, 026, 024, 021, 019, 017, 015, 013, 011, 009, 008, 007, 005, 004, 003, 002, 002, 001, 001, 001
.byte 001, 001, 001, 001, 002, 002, 003, 004, 005, 007, 008, 009, 011, 013, 015, 017, 019, 021, 024, 026, 028, 031, 034, 037, 039, 042, 045, 048, 051, 054, 057, 060
.byte 063, 067, 070, 073, 076, 079, 082, 085, 088, 090, 093, 096, 099, 101, 103, 106, 108, 110, 112, 114, 116, 118, 119, 120, 122, 123, 124, 125, 125, 126, 126, 126
.byte 126, 126, 126, 126, 125, 125, 124, 123, 122, 120, 119, 118, 116, 114, 112, 110, 108, 106, 103, 101, 099, 096, 093, 090, 088, 085, 082, 079, 076, 073, 070, 067
.byte 064, 060, 057, 054, 051, 048, 045, 042, 039, 037, 034, 031, 028, 026, 024, 021, 019, 017, 015, 013, 011, 009, 008, 007, 005, 004, 003, 002, 002, 001, 001, 001
.byte 001, 001, 001, 001, 002, 002, 003, 004, 005, 007, 008, 009, 011, 013, 015, 017, 019, 021, 024, 026, 028, 031, 034, 037, 039, 042, 045, 048, 051, 054, 057, 060

.byte 063, 067, 070, 073, 076, 079, 082, 085, 088, 090, 093, 096, 099, 101, 103, 106, 108, 110, 112, 114, 116, 118, 119, 120, 122, 123, 124, 125, 125, 126, 126, 126
.byte 126, 126, 126, 126, 125, 125, 124, 123, 122, 120, 119, 118, 116, 114, 112, 110, 108, 106, 103, 101, 099, 096, 093, 090, 088, 085, 082, 079, 076, 073, 070, 067
.byte 064, 060, 057, 054, 051, 048, 045, 042, 039, 037, 034, 031, 028, 026, 024, 021, 019, 017, 015, 013, 011, 009, 008, 007, 005, 004, 003, 002, 002, 001, 001, 001
.byte 001, 001, 001, 001, 002, 002, 003, 004, 005, 007, 008, 009, 011, 013, 015, 017, 019, 021, 024, 026, 028, 031, 034, 037, 039, 042, 045, 048, 051, 054, 057, 060
.byte 063, 067, 070, 073, 076, 079, 082, 085, 088, 090, 093, 096, 099, 101, 103, 106, 108, 110, 112, 114, 116, 118, 119, 120, 122, 123, 124, 125, 125, 126, 126, 126
.byte 126, 126, 126, 126, 125, 125, 124, 123, 122, 120, 119, 118, 116, 114, 112, 110, 108, 106, 103, 101, 099, 096, 093, 090, 088, 085, 082, 079, 076, 073, 070, 067
.byte 064, 060, 057, 054, 051, 048, 045, 042, 039, 037, 034, 031, 029, 026, 024, 021, 019, 017, 015, 013, 011, 009, 008, 007, 005, 004, 003, 002, 002, 001, 001, 001
.byte 001, 001, 001, 001, 002, 002, 003, 004, 005, 007, 008, 009, 011, 013, 015, 017, 019, 021, 024, 026, 028, 031, 034, 037, 039, 042, 045, 048, 051, 054, 057, 060 

times8tablelo
		.repeat 256, I
			.byte <((I+4)*8)
		.endrepeat

times8tablehi
		.repeat 256, I
			.byte >((I+4)*8)
		.endrepeat

dstcolumnslo
		.repeat 64, I
			.byte <((I*25*64) + 0), <((I*25*64) + 1), <((I*25*64) + 2), <((I*25*64) + 3), <((I*25*64) + 4), <((I*25*64) + 5), <((I*25*64) + 6), <((I*25*64) + 7)
		.endrepeat

dstcolumnshi
		.repeat 64, I
			.byte >((I*25*64) + 0), >((I*25*64) + 1), >((I*25*64) + 2), >((I*25*64) + 3), >((I*25*64) + 4), >((I*25*64) + 5), >((I*25*64) + 6), >((I*25*64) + 7)
		.endrepeat

srccolumnslo
		.repeat 256, I
			.byte <(I*128)
		.endrepeat

srccolumnshi
		.repeat 256, I
			.byte >(I*128)
		.endrepeat

frame
		.byte 0

frame2
		.byte 0

screenrow
		.byte 0

screencolumn
		.byte 0

plotcolumnlo
		.byte 0

plotcolumnhi
		.byte 0

; ----------------------------------------------------------------------------------------------------

clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (SAFE_COLOR_RAM) >> 20				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 40*26										; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM) & $ffff					; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000									; Command MSB

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 40*26										; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM+1) & $ffff				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM+1) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000								; Command MSB

				.word $0000

copybitmapjob
		;DMA_HEADER $20000 >> 20, $30000 >> 20
		; f018a = 11 bytes, f018b is 12 bytes
		.byte $0a ; Request format is F018A
		.byte $07 ; enable alpha
		.byte $80, (twistcharset >> 20) ; sourcebank
		.byte $81, (destcharset >> 20) ; destbank

srcskiplo
		.byte $82, 0 ; Source skip rate (256ths of bytes)
		.byte $83, 1 ; Source skip rate (whole bytes)

		.byte $84, 0 ; Destination skip rate (256ths of bytes)
		.byte $85, 8 ; Destination skip rate (whole bytes)

		.byte $00 ; No more options

		.byte $00 ; Copy and last request
		.word 128 ; Size of Copy
srccopy
		.word twistcharset & $ffff
		.byte ((twistcharset >> 16) & $0f)
dstcopy
		.word destcharset & $ffff
		.byte ((destcharset >> 16) & $0f)






copyentirebitmapjob
		;DMA_HEADER $20000 >> 20, $30000 >> 20
		; f018a = 11 bytes, f018b is 12 bytes
		.byte $0a ; Request format is F018A
		.byte $80, (bowlcharset >> 20) ; sourcebank
		.byte $81, (destcharset >> 20) ; destbank

		.byte $82, 0 ; Source skip rate (256ths of bytes)
		.byte $83, 1 ; Source skip rate (whole bytes)

		.byte $84, 0 ; Destination skip rate (256ths of bytes)
		.byte $85, 1 ; Destination skip rate (whole bytes)

		.byte $00 ; No more options

		.byte $00 ; Copy and last request
		.word 40*25*64 ; Size of Copy

		.word bowlcharset & $ffff
		.byte (bowlcharset >> 16)

		.word destcharset & $ffff
		.byte ((destcharset >> 16) & $0f)
