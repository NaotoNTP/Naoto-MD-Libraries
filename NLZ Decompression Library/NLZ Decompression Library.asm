; =============================================================================================================================
; NLZ Decompressor and Queue Library - by NaotoNTP (2025)
; =============================================================================================================================
; -----------------------------------------------------------------------------------------------------------------------------
; ASM68K Options (Comment these out if you're using an alternative assembler, including the 'popo' directive at the end of the file).
; -----------------------------------------------------------------------------------------------------------------------------
	pusho							; Backup the current set of options.
	opt	l.,ws+						; Use "." for local labels and allow white-space in operand parsing.
; -----------------------------------------------------------------------------------------------------------------------------
; Declarations 
; -----------------------------------------------------------------------------------------------------------------------------
; NOTE:	You should ideally move/convert these to whichever files you define your RAM declarations in; they're mostly just here 
;	for the purposes of distributing this library as a single .asm file. Some of the code also depends on the variables 
;	being in the order they are defined in by default for optimization purposes. Please take care, double check the code,
;	and modify it as necessary if you plan on moving things around.
; -----------------------------------------------------------------------------------------------------------------------------

; NLZ Configuration Constants.
NLZ_CONFIG	equ	4					; Set this value to one of the following values to configure the decompressor for your desired module size.
								; 1 = $200 byte modules; 2 = $400 byte modules; 3 = $800 byte modules; 4 = $1000 byte modules; 5 = $2000 byte modules.
NLZ_BUFFER_SIZE	equ	$100<<NLZ_CONFIG			; Size of the decompression buffer (in bytes).
NLZ_QUEUE_SIZE	equ	32					; Number of slots in the decompression queue.

; NLZ Queue Entry Offsets.
		rsreset
nque.next	rs.w	1					; Word-size pointer to the next entry in the queue.
nque.src	rs.l	1					; Source address (ROM) of the NLZ archive to be decompressed.
nque.dest	rs.w	1					; Destination address (VRAM) to transfer the decompressed art to. 
nque.size	rs.b	0					; Size of a single queue entry in bytes.

; NLZ Buffer/Variable Declarations.
nlzLgBuffer	equ	$FF0000					; Address of the large buffer to be used by non-moduled NLZ archives in the decompression queue (set to the beginning of RAM by default).
nlzRAM		equ	$FFFF8000				; Base address for the rest of the RAM declarations (can be changed to any valid RAM address that is reachable through absolute word addressing).

		rsset	nlzRAM
nlzBuffer	rs.b	NLZ_BUFFER_SIZE				; The main buffer used for decompressing moduled NLZ archives in the decompression queue.
nlzQueue	rs.b	nque.size*NLZ_QUEUE_SIZE		; The NLZ art decompression queue.

nlzQueueHead	rs.w	1					; Word-size pointer to the first occupied entry in the queue.
nlzQueueTail	rs.w	1					; Word-size pointer to the last occupied entry in the queue.
nlzQueueFree	rs.w	1					; Word-size pointer to the first free entry in the queue.
nlzLastModSize	rs.w	1					; Size of the last module in the current archive, in words. (Can also be used as a flag to indicate when the last module in an archive has been decompressed, as it gets cleared upon completion).

nlzBookmarkFlag	rs.b	1					; Flag used to indicate if a bookmark should be set upon returning from VBlank.
nlzFlushModule	rs.b	1					; Flag used to indicate if the module buffer is ready to be flushed.
nlzModuleCount	rs.b	1					; Number of modules left to decompress in the current archive.
nlzModuleConfig	rs.b	1					; Offset into the table that defines the configuration of the current archive.

nlzVRAMDest	rs.w	1					; VRAM destination for the current module to be transfered to.
nlzBufferPtr	rs.l	1					; The address of the decompression buffer to use for the current archive.
nlzNextModule	rs.l	1					; The address of the next module to be decompressed.
nlzVIntSP	rs.l	1					; The address that the stack pointer was set to immediately after a VBlank interrupt.

nlzBookmarkDn	rs.w	4					; Space to backup the data register when setting a bookmark. 
nlzBookmarkAn	rs.l	4					; Space to backup the address register when setting a bookmark. 
nlzBookmarkSR	rs.w	1					; Space to backup the status register flags when setting a bookmark. 
nlzBookmarkPC	rs.l	1					; Space to backup the program counter address when setting a bookmark.

; -----------------------------------------------------------------------------------------------------------------------------
; Initialize the NLZ art decompression queue.
; -----------------------------------------------------------------------------------------------------------------------------
; USED:
;	d0/a0-a1
; -----------------------------------------------------------------------------------------------------------------------------
NLZ_InitializeQueue:
		lea	(nlzQueue).w,a0				; Load the address of the first queue entry into register a0.
		move.w	a0,(nlzQueueFree).w			; Initialize the pointer of the first free queue entry.
		clr.l	(nlzQueueHead).w			; Clear both the head and tail queue entry pointers.
		clr.l	(nlzBookmarkFlag).w			; Clear the bookmark flag, flush module flag, module count, and module config variables.
		clr.w	(nlzLastModSize).w			; Clear the last module size variable.

		move.w	#NLZ_QUEUE_SIZE-1-1,d0			; Initialize the loop counter to loop through all but the final entry.

.initEntries:
		lea	nque.size(a0),a1			; Load the address of the next queue entry.
		move.w	a1,nque.next(a0)			; Intitialize the next entry pointer for the current entry.
		move.w	a1,a0					; Move to the next entry.
		dbf	d0,.initEntries				; Loop until all but the final queue entry have been initialized.

		clr.w	nque.next(a0)				; Nullify the final entry's next entry pointer.
		rts						; Return.

; -----------------------------------------------------------------------------------------------------------------------------
; Adds a file to the NLZ art decompression queue.
; -----------------------------------------------------------------------------------------------------------------------------
; INPUT:
;	d1.w -	Destination address in VRAM
;	a1.l -	Source address of the compressed data
; -----------------------------------------------------------------------------------------------------------------------------
NLZ_AddArtToQueue:
		tst.w	(nlzQueueFree).w			; Is there any space in the queue for a new entry?
		beq.s	.queueIsFull				; If there isn't, exit with a failure condition.

		movem.l	d0/a5-a6,-(sp)				; Back up registers used by this subroutine.
		movea.w	(nlzQueueFree).w,a6			; Load the address of the first free entry into a6.
		move.w	nque.next(a6),(nlzQueueFree).w		; Update the first free entry address pointer.
		
		move.w	(nlzQueueTail).w,d0			; Is the queue entirely empty?
		bne.s	.queueNotEmpty				; If not, branch.
		
		move.w	a6,(nlzQueueHead).w			; Update the head entry address pointer.
		bra.s	.queueIsEmpty				; Branch ahead to avoid updating the 'next' pointer on a null entry.

.queueNotEmpty:
		movea.w	d0,a5					; Load the address of the current tail entry into a5.
		move.w	a6,nque.next(a5)			; Update the previous tail entry's 'next' pointer.

.queueIsEmpty:
		move.w	a6,(nlzQueueTail).w			; Update the tail entry address pointer.
		clr.w	nque.next(a6)				; Nullify the new tail entry's 'next' pointer.
		move.l	a1,nque.src(a6)				; Set the source address for this entry.
		move.w	d1,nque.dest(a6)			; Set the VRAM destination address for this entry.

		movem.l	(sp)+,d0/a5-a6				; Restore backed up registers.
		andi	#~$C,ccr				; Clear both the zero and negative flags on the ccr, indicating success.
		rts						; Return.
; -----------------------------------------------------------------------------------------------------------------------------
.queueIsFull:
		ori	#8,ccr					; Set the negative flag on the ccr, indicating failure.
		rts						; Return.

; -----------------------------------------------------------------------------------------------------------------------------
; Sets a bookmark for the NLZ decompressor if the bookmark flag is set and flushes the module buffer if necessary (called during V-Int).
; -----------------------------------------------------------------------------------------------------------------------------
; NOTE:	You MUST update the 'nlzVIntSP' variable at the very beginning of V-Int before backing up any registers; it needs to be 
;	pointing at the interrupt's stack frame in order for this routine to correctly hijack the return address.
;	
;	In less technical terms, we need to save the address that (sp) points to right after the program jumps to V-Int.
;	This can be done like so:
;
;	VBlankInterrupt:
;		move.l	sp,(nlzVIntSP).w			; Update the interrupt SP address used by the bookmark logic.
;		...
; -----------------------------------------------------------------------------------------------------------------------------
NLZ_FlushAndBookmark:
		tst.b	(nlzFlushModule).w			; Is a module ready to be flushed to VRAM?
		beq.s	.attemptBookmark			; If not, proceed with the bookmark logic.
		bra.s	NLZ_FlushBuffer				; Otherwise, flush the module from the buffer instead.

; -----------------------------------------------------------------------------------------------------------------------------
.attemptBookmark:
		tst.b	(nlzBookmarkFlag).w			; Is the bookmark flag set?
		beq.s	.exit					; If not, branch and return.

		movea.l	(nlzVIntSP).w,a0			; Load the address of the V-Int stack frame.
		addq.w	#2,a0					; Point to the position of the return address.
		
		move.l	(a0),(nlzBookmarkPC).w			; Save the return address as the bookmark progam counter value.
		move.l	#.setBookmark,(a0)			; Hijack the return address with the 2nd part of this routine.

.exit:
		rts						; Return.

; -----------------------------------------------------------------------------------------------------------------------------
.setBookmark:
		movem.w	d0-d3,(nlzBookmarkDn).w			; Save the state of the mutable data registers (d0-d3).
		movem.l	a0-a3,(nlzBookmarkAn).w			; Save the state of the address registers (a0-a3).
		move.w	sr,(nlzBookmarkSR).w			; Save the state of the status register.
		rts						; Return, effectively pausing decompression for now.

; -----------------------------------------------------------------------------------------------------------------------------
; Perform a flush of the NLZ module buffer to VRAM.
; -----------------------------------------------------------------------------------------------------------------------------
; USED:
;	d0-d3/a0
; -----------------------------------------------------------------------------------------------------------------------------
NLZ_FlushBuffer:
		move.l	(nlzBufferPtr).w,d2			; Load the address of the buffer into d2.
		lsr.l	#1,d2					; Shift right by one to make it DMA-compatible.
		move.w	(nlzVRAMDest).w,d3			; Load the VRAM destination for the current module into d3.

		tst.b	(nlzModuleCount).w			; Are we getting ready to transfer the last module?
		bne.s	.fullModule				; If not, branch to queue up the transfer of a full module to VRAM.
	
		move.w	(nlzLastModSize).w,d1			; Otherwise, load the size of the last module as the transfer length.
		clr.w	(nlzLastModSize).w			; Clear the last module size to signal that the archive has been fully decompressed.
		bra.s	.performFlush				; Branch ahead and perform the flush.

; -----------------------------------------------------------------------------------------------------------------------------
.fullModule:
		lea	NLZ_ModuleConfig(pc),a0			; Load the address of the module configuration table.
		clr.w	d0					; Clear register d0.
		move.b	(nlzModuleConfig).w,d0			; Load the module configuration offset.

		move.w	2(a0,d0.w),d1				; Load the size of a full module as the transfer length.
		add.w	d1,(nlzVRAMDest).w			; Add the buffer size to the VRAM destination to update it for the next module.
		lsr.w	#1,d1					; Put the transfer length in units of words instead of bytes.

.performFlush:
		lea	($C00004).l,a0				; Load the address of the VDP Control Port into register a0.

		move.w	#$9500,d0				; Set the low byte of the DMA transfer source address.
		move.b	d2,d0					; ^
		move.w	d0,(a0)					; ^

		move.l	#$977F9600,d0				; Set the middle and high bytes of the DMA transfer source address.
		lsr.w	#8,d2					; ^
		move.b	d2,d0					; ^
		move.l	d0,(a0)					; ^

		move.l	#$94009300,d0				; Set the length of the DMA transfer.
		move.b	d1,d0					; ^
		lsr.w	#8,d1					; ^
		swap	d0					; ^
		move.b	d1,d0					; ^
		move.l	d0,(a0)					; ^

		moveq	#0,d0					; Set the destination for the DMA transfer.
		move.w	d3,d0					; ^
		rol.l	#2,d0					; ^
		lsr.w	#2,d0					; ^
		swap	d0					; ^
		ori.l	#$40000080,d0				; ^

		move.l	d0,(a0)					; Initiate the DMA transfer.
		sf.b	(nlzFlushModule).w			; Clear the flush module flag.
		rts						; Return.

; -----------------------------------------------------------------------------------------------------------------------------
; Process the next item in the queue or resume in-progress decompression.
; -----------------------------------------------------------------------------------------------------------------------------
; USED:
;	d0-d6/a0-a3
; -----------------------------------------------------------------------------------------------------------------------------
NLZ_DecompressFromQueue:
		tst.b	(nlzFlushModule).w			; Has the previous module been flushed to VRAM?
		bne.s	.exit					; If not, exit.

		tst.b	(nlzBookmarkFlag).w			; Is the bookmark flag set?
		bne.s	.resumeFromBookmark			; If so, branch and pick up where we left off.

		subq.b	#1,(nlzModuleCount).w			; Decrement the module counter.		
		bcc.w	.decNextModule				; If the counter did not underflow, decompress the next full module.

.processNextEntry:
		clr.b	(nlzModuleCount).w			; Clear the module counter.
		move.w	(nlzQueueHead).w,d0			; Are there any entries in the queue?
		bne.s	.gotNextEntry				; If so, branch.

.exit:		
		rts						; Return.

; -----------------------------------------------------------------------------------------------------------------------------
.gotNextEntry:
		movea.w	d0,a0					; Load the address of the current entry in the queue into a0.
		move.w	nque.next(a0),(nlzQueueHead).w		; Update the queue head entry address to the next entry, removing the current entry from the queue.
		bne.s	.queueNotEmpty				; Branch ahead if we moved a valid pointer.
		clr.w	(nlzQueueTail).w			; Otherwise, nullify the queue's tail entry as well before moving on. 

.queueNotEmpty:
		move.w	(nlzQueueFree).w,nque.next(a0)		; Update the current entry's 'next' pointer to point to the first free entry.
		move.w	a0,(nlzQueueFree).w			; Update first free entry address to point to the current entry, adding it to the queue's expicit free list.

		move.w	nque.dest(a0),(nlzVRAMDest).w		; Save the destination VRAM address.
		movea.l	nque.src(a0),a0				; Load the source address of the NLZ archive into a0.
		move.w	(a0)+,(nlzLastModSize).w		; Save the size of the last module (in words, not bytes).
		move.b	(a0)+,(nlzModuleCount).w		; Save the module count (-1 since the first module will be immediately processed after this).

		lea	(nlzBuffer).w,a1			; Load the default buffer address into a1.
		clr.w	d0					; Clear the lower word of d0.
		move.b	(a0)+,d0				; Load the module configuration.
		bne.s	.isModuled				; If the archive is indeed moduled, branch ahead.
		lea	(nlzLgBuffer).l,a1			; Otherwise, we load the address of a larger buffer for the non-moduled archive.

.isModuled:
		move.b	d0,(nlzModuleConfig).w			; Save the the module configuration index.
		move.l	a1,(nlzBufferPtr).w			; Save the address of the decompression buffer we want to use.
		bra.s	.decModule				; Branch ahead and decompress the first module.

; -----------------------------------------------------------------------------------------------------------------------------
.resumeFromBookmark:
		lea	NLZ_ModuleConfig(pc),a1			; Load the address of the module configuration table.
		clr.w	d0					; Clear register d0.
		move.b	(nlzModuleConfig).w,d0			; Load the module configuration offset.
		adda.w	d0,a1					; Add it to get the configuration for this archive.

		move.b	(a1)+,d4				; Restore the shift count (d4).
		move.b	(a1)+,d5				; Restore the copy mask (d5).
		move.w	(a1)+,d6				; Restore the buffer size (d6).

		movem.w	(nlzBookmarkDn).w,d0-d3			; Restore the remaining data registers (d0-d3).
		movem.l	(nlzBookmarkAn).w,a0-a3			; Restore the address registers (a0-a3).
		move.l	(nlzBookmarkPC).w,-(sp)			; Restore the return address of the bookmark.
		move.w	(nlzBookmarkSR).w,sr			; Restore the status register state.
		rts						; Resume decompression from the point of bookmark.

; -----------------------------------------------------------------------------------------------------------------------------
.decNextModule:
		movea.l	(nlzNextModule).w,a0			; Load the source address of the next module in the current NLZ archive into a0.
		
.decModule:
		lea	NLZ_ModuleConfig(pc),a1			; Load the address of the module configuration table.
		clr.w	d0					; Clear register d0.
		move.b	(nlzModuleConfig).w,d0			; Load the module configuration offset.
		adda.w	d0,a1					; Add it to get the configuration for this archive.

		move.b	(a1)+,d4				; Load the shift count into d4.
		move.b	(a1)+,d5				; Load the copy mask into d5.
		move.w	(a1)+,d6				; Load the buffer size into d6.
		movea.l	(nlzBufferPtr).w,a1			; Load the address of the decompression buffer into a1.

		st.b	(nlzBookmarkFlag).w			; Set the bookmark flag.
		bra.s	NLZ_DecompressModule			; Decompress the next module (returns from this routine; cannot be a subroutine call due to bookmarking logic).

; -----------------------------------------------------------------------------------------------------------------------------
; Decompress an NLZ archive directly to a specified destination.
; -----------------------------------------------------------------------------------------------------------------------------
; INPUT:
;	a0.l -	Source (NLZ compressed archive)
;	a1.l -	Destination Address
; -----------------------------------------------------------------------------------------------------------------------------
; USED:
;	d0-d7/a0-a3
; -----------------------------------------------------------------------------------------------------------------------------
NLZ_DecompressDirect:
		addq.l	#2,a0					; Skip the 'last module size' section of the header
		
		clr.w	d7					; Clear the lower word of d7 to use as a loop counter.
		move.b	(a0)+,d7				; Load the module count - 1 as the loop counter.
		clr.w	d0					; Clear the lower word of d0 so we can use it as a table offset.
		move.b	(a0)+,d0				; Load the module configuration into d0.

		lea	NLZ_ModuleConfig(pc),a3			; Load the address of the module configuration table.
		adda.w	d0,a3					; Add the module configuration offset.
		move.b	(a3)+,d4				; Load the shift count into d4.
		move.b	(a3)+,d5				; Load the copy mask into d5.
		clr.w	d6					; Clear the lower word of d6, nullifying th buffer size parameter.

.decAllModules:
		bsr.s	NLZ_DecompressModule			; Decompress a single module in the archive.
		dbf	d7,.decAllModules			; Loop until all modules are decompressed in one continuous stream.
		rts						; Return.

; -----------------------------------------------------------------------------------------------------------------------------
; Decompress a single NLZ module.
; -----------------------------------------------------------------------------------------------------------------------------
; INPUT:
;	d4.b -	Shift count (Number of high address bits in a full dictionary match.)
;	d5.b -	Copy length mask (Mask to retrieve the copy length from a full dictionary match.)
;	d6.w -	Module size (Size of the decompression buffer (in bytes).)
;	a0.l -	Source (NLZ compressed module)
;	a1.l -	Destination Address
; -----------------------------------------------------------------------------------------------------------------------------
; USED:
;	d0-d6/a0-a3
; -----------------------------------------------------------------------------------------------------------------------------
NLZ_DecompressModule:
		movea.l	a1,a3
		moveq	#0,d1					; Clear the read-bit counter.
		bra.s	.rollDescField				; Branch and roll the description field.

; -----------------------------------------------------------------------------------------------------------------------------
.copyUncByte:
		move.b	(a0)+,(a1)+				; Copy an uncompressed byte to the output buffer.

.rollDescField:
		dbf	d1,.rollCurrent				; Decrement the read-bit counter and branch if there are still more bits to read from the current description field.
		moveq	#7,d1					; Otherwise, we reset the counter and load the next description field.
		move.b	(a0)+,d0				; ^

.rollCurrent:	
		add.b	d0,d0					; Roll the description field by a single bit
		bcc.s	.copyUncByte				; If the next packet is an uncompressed byte, branch. Otherwise, continue.

; -----------------------------------------------------------------------------------------------------------------------------
.chkFieldDepleted:
		dbf	d1,.bitsLeft				; Decrement the read-bit counter and branch if there are still more bits to read from the current description field.
		moveq	#7,d1					; Otherwise, we reset the counter and load the next description field.
		move.b	(a0)+,d0				; ^

.bitsLeft:	
		moveq	#0,d2					; Load the first byte of the dictionary match packet to registers d2 and d3.
		move.b	(a0)+,d2				; ^
		move.w	d2,d3					; ^

.rollMatchType:		
		add.b	d0,d0					; Roll the description field by a single bit.
		bcs.s	.fullMatch				; If the next packet is a full dictionary match, branch. Otherwise, continue.

; -----------------------------------------------------------------------------------------------------------------------------
.nearbyMatch:
		lsr.w	#2,d3					; Shift the d3 right by 2 and logically NOT it to get the match displacement.
		not.w	d3					; ^
		andi.w	#%11,d2					; Logically AND the lower 2 bits of d2 to get the copy length.
		beq.s	.readExtCopyLen				; If the copy length zero, read an extended copy byte.

		lea	(a1,d3.w),a2				; Load the location of the dictionary match into register a2.
		cmpa.l	a3,a2					; Is the dictionary match still in bounds?
		bhs.s	.copyLoop				; If so, branch.
		add.w	d6,a2					; Otherwise, add the buffer size to wrap the match location around.

.copyLoop:
		move.b	(a2)+,(a1)+				; Copy the bytes in a loop.
		dbf	d2,.copyLoop				; ^
		bra.s	.rollDescField				; Branch back and handle the next packet.

; -----------------------------------------------------------------------------------------------------------------------------
.readExtCopyLen:
		move.b	(a0)+,d2				; Load the extended copy length byte.
		bne.s	.gotCopyLen				; If the copy length is non-zero, branch and perform the match copy.

	; Otherwise, this is the terminating packet and we are finished with decompression.	
		tst.b	(nlzBookmarkFlag).w			; Is the bookmark flag set?
		beq.s	.exit					; If not, skip over logic related to decompressing from the queue.

		move.l	a0,(nlzNextModule).w			; Save the address where we left off as the beginning of the next module.
		sf.b	(nlzBookmarkFlag).w			; Clear the bookmark flag.
		st.b	(nlzFlushModule).w			; Set the flush module flag.

.exit:		
		rts						; Return.

; -----------------------------------------------------------------------------------------------------------------------------
.fullMatch:
		lsl.w	d4,d3					; Shift the d3 left by the shift count, load the next byte, and logically NOT the whole register to get the match displacement.
		move.b	(a0)+,d3				; ^
		not.w	d3					; ^
		and.w	d5,d2					; Logically AND d2 against the copy length bit mask to get the copy length.
		beq.s	.readExtCopyLen				; If the copy length is zero, branch and read the extneded copy length byte.
		
		addq.w	#1,d2					; Otherwise, increment the copy length to its true value and continue.

.gotCopyLen:	
		lea	(a1,d3.w),a2				; Load the location of the dictionary match into register a2.
		cmpa.l	a3,a2					; Is the dictionary match still in bounds?
		bhs.s	.inBounds				; If so, branch.
		add.w	d6,a2					; Otherwise, add the buffer size to wrap the match location around.

.inBounds:
		moveq	#%11,d3					; Separate the copy count into longwords and bytes.
		and.w	d2,d3					; ^
		lsr.w	#2,d2					; ^
		beq.s	.copyBytes				; ^

		subq.w	#1,d2					; Decrement the longword copy count to make it into a loop counter.

.copyLongs:
		move.b	(a2)+,(a1)+				; Copy unaligned longwords in a loop.
		move.b	(a2)+,(a1)+				; ^
		move.b	(a2)+,(a1)+				; ^
		move.b	(a2)+,(a1)+				; ^
		dbf	d2,.copyLongs				; ^

.copyBytes:
		move.b	(a2)+,(a1)+				; Copy the remaining bytes in a loop.
		dbf	d3,.copyBytes				; ^
		bra.w	.rollDescField				; Branch back and handle the next packet.

; -----------------------------------------------------------------------------------------------------------------------------
; Table containing information related to module configurations
; -----------------------------------------------------------------------------------------------------------------------------
NLZ_ModuleConfig:
	; Entry format:
	; Shift Count (byte)
	; Copy Length Mask (byte)
	; Module Size (word)

	; 00 - non-moduled
	dc.b	5, %111
	dc.w	0

	; 04 - moduled ($200 byte modules)
	dc.b	1, %1111111
	dc.w	$200

	; 08 - moduled ($400 byte modules)
	dc.b	2, %111111
	dc.w	$400

	; 0C - moduled ($800 byte modules)
	dc.b	3, %11111
	dc.w	$800

	; 10 - moduled ($1000 byte modules)
	dc.b	4, %1111
	dc.w	$1000

	; 14 - moduled ($2000 byte modules)
	dc.b	5, %111
	dc.w	$2000

; -----------------------------------------------------------------------------------------------------------------------------
	popo							; Restore the saved set of options.
; -----------------------------------------------------------------------------------------------------------------------------