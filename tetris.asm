 LIST	p=18F4520
 #include <P18f4520.INC>
	CONFIG	OSC = XT
	CONFIG	WDT	= OFF
	CONFIG 	LVP = OFF

 ;;;;;;;;;;;; CONSTANT DEFINITIONS ;;;;;;;;;;;;;;
;--------------------------------------------------------------------------------------
; These are used to draw the screen
; Needed to be able to iterate through the rows table in memory
 TOTAL_ROWS		 	EQU 0x0F
 ROWS_PER_SCREEN 	EQU 0x07
 ROWS_START_ADDRESS	EQU	0x03	; defined from cblock below
 SCREEN_START		EQU 0x00
 BLOCKS_START_ADDRESS		EQU	0x19	; Address offset for first block reg - after rows table
										; Should be 0x10 (start of cblock) + d15 (length of rows table)

;;;;;;;;;;;;;;;;;; END OF CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;; REGISTER DEFINITIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;
 cblock 0x000
	rows:19			; most important - stores the current state of the game 
					; each row is an 8 bit number, where a 1 indicates a block present
					; which means that the LED should be on. 0 means no block and off LED
;-----------------------------------------------------------------------------------------
	activeBlock:4	; stores the rows of the currently active block
;-----------------------------------------------------------------------------------------
	rowCount		; Both used for iterating through screen during DrawScreen
	colCount
;-----------------------------------------------------------------------------------------
	currentRow	; dummy register for storing the row to mask with rows table
	nextRow
	blockPosX		; X-coordinate of current active block
	blockPosY		; Y-coordinate of current active block
	currentWidth	; Width of drawing frame (either 4 or 2, depending on rotation of block)
	currentHeight	; Height of drawing frame (either 2 or 4, depending on rotation of block)
	writeLoopCount	; counting how much we need to iterate through
	shiftingCount	; counting how many times we need to shift
	heightOffset
	blockOffset
	blockOffsetCount
;-------------------------------------------------------------------------------------------
	W_temp			; store for the timer interrupt - to be put back after interrupt is over
	status_temp
	bsr_temp
	
	updateGameFlag		; if first bit set, clear frame
	debounce_count_1
	debounce_count_2
;-------------------------------------------------------------------------------------------
	rotationState		; keeps track of which way the block is rotated
	blockNumber			; keeps track of which type of block
	blockIndex			; used to index the lookup tables when generating new blocks
	
	genBlockFlag
	collisionDetected
	blockLookup
;-------------------------------------------------------------------------------------------
 endc
;;;;;;;;;;;;;;;; END OF REGISTER DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;;
 ORG 0x00
 goto Main

; Interrupt Handler
 ORG 0x08
 btfsc		INTCON, TMR0IF
 call		T0_ISR
 retfie

Main:
	call	InitializeBlockRegs ; initializing block values
	call 	InitializePorts		; setting inputs and outputs for all ports, disabling ADC
	call	InitializeRowTable	; need to clear the memory so we don't draw random things
	call	InitializeTimer
	
	; initializing debounce values for polling buttons
 	movlw	0x03
	movwf	debounce_count_2
	movlw	0xFF
	movwf	debounce_count_1
 main_loop
	call 	PollButtons
	btfsc	updateGameFlag, 1
	call	UpdateGame
	call 	DrawScreen
	
	goto 	main_loop

;;;;;;;;;;;;;;;;;;;;;; FUNCTION DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;

;--------------------- Initialization Funcs --------------------------
InitializeBlockRegs:
	; Initializing the offset and number of each block, and generating block
	; at start of the game
	movlw	0x00
	movwf	blockOffset
	movwf	blockNumber
	movwf	genBlockFlag
	call	GenerateNewBlock
	return
InitializePorts:
	; Sets all ports to the appropriate states
	; Disables ADC, sets PORTB and PORTD as output
	; Sets PORTA as input
	movlw 	0x0F
	movwf 	ADCON1
	setf 	TRISA
	clrf 	TRISB
	clrf 	TRISC
	clrf 	TRISD

	; Clearing PORTC and PORTD so as not to accidentally
	; write random values to them in the beginning
	clrf	PORTC
	clrf	PORTD
	return

InitializeRowTable:
	; Function to clear existing memory in row table
	; Rows are accessed by FSR0 through pointers
	lfsr	0, SCREEN_START
	movlw	TOTAL_ROWS+3
	movwf	rowCount
 init_row_loop
	clrf	POSTINC0
	decf	rowCount
	bnz		init_row_loop
	clrf	POSTINC0
	return

InitializeTimer:
	; Timer 0 initialized for updating the screen
	; specific values put into Timer were found experimentally
	movlw	0x02
	movwf	T0CON
	movlw	0x85
	movwf	TMR0H
	movlw	0xED
	movwf	TMR0L
	bcf		INTCON, TMR0IF
	bsf		INTCON, TMR0IE


	bsf		INTCON, GIE
	bsf		T0CON,  TMR0ON
	bsf		T1CON, TMR1ON
	return

;-------------------------- Button Funcs ------------------------------------
PollButtons:
	; Cycles through all the buttons and calls functions based on which button is pressed
 wait_button_1
	btfsc	PORTA, 0		; checks button
	bra		wait_button_3	; if nothing, skip to next button
	call	RotateBlockDebounce
 wait_button_3
	; button 3 shifts the block left
	btfsc	PORTA, 2
	bra		wait_button_4
	movlw	0x07
	cpfseq	blockPosX		; we want to check if we're already at the boundary
	call	ShiftRight
 wait_button_4
	; shifts the block right
	btfsc	PORTA, 3
	bra		exit_func	
	movlw	0x00
	cpfseq	blockPosX
	call	ShiftLeft		; want to check if at boundary
 exit_func
	return

ShiftRight:
	; we delay the input here by a running count
	; so that the active block doesn't move too quickly
	decf	debounce_count_1
	bz		shift_block_inner_r
	return
  shift_block_inner_r
	movlw	0xFF
	movwf	debounce_count_1
	decf	debounce_count_2
	bz		shift_block_right
	return
  shift_block_right
	call	ClearFrameFromMem	; clear the frame before incrementing
	incf	blockPosX
	call	WriteFrameToMem		; write the new, incremented frame to memory
	movlw	0x02
	movwf	debounce_count_2	; reset count
	return

ShiftLeft:
	; see comments for ShiftRight
	decf	debounce_count_1
	bz		shift_block_inner_l
	return
  shift_block_inner_l
	movlw	0xFF
	movwf	debounce_count_1
	decf	debounce_count_2
	bz		shift_block_left
	return
 shift_block_left
	call	ClearFrameFromMem
	decf	blockPosX
	call	WriteFrameToMem
	movlw	0x02
	movwf	debounce_count_2
	return

RotateBlockDebounce:
	decf	debounce_count_1
	bz		rotate_delay_inner
	return
 rotate_delay_inner
	movlw	0xFF
	movwf	debounce_count_1
	decf	debounce_count_2
	bz		rotate_block_final
 	return
 rotate_block_final
	movlw	0x05	
	cpfslt	rotationState		; here we want to check if we're exceeding the max rotation state
	clrf	rotationState		; and if so, reset it. Otherwise, we just increment
	incf	rotationState

	call	ClearFrameFromMem	; similar to above, we want to clear before getting the rotated block
	call	GetBlockFromMem
	call	WriteFrameToMem		; only after clearing and retrieving do we write the the rows table
	movlw	0x02
	movwf	debounce_count_2
	return

GetBlockFromMem:
	; retrives block from ROM Table, puts it in RAM - doesn't change rotationState or blockIndex
	clrf	blockOffset				; resets blockOffset, which is what we use as the height correction
	lfsr	1, BLOCKS_START_ADDRESS	; setting up FSR1 to take input
	movlw	0x01
	cpfseq	rotationState			; essentially a case-switch, where we want to call one of four
	bra		second_state			; functions depending on the rotation state
	bra		right_rotation
 second_state
	movlw	0x02
	cpfseq	rotationState
	bra		third_state
	bra		up_rotation	
 third_state
	movlw	0x03
	cpfseq	rotationState
	bra		fourth_state
	bra		left_rotation
 fourth_state
	movlw	0x04
	cpfseq	rotationState
	return
	bra		down_rotation
	
 right_rotation
	call	GetBlockRight			; calling the four different functions based on rotation state
	call	AdjustHeight			; adjusting height of the blocks (ie, changing the height of a t-block from 4 to 3 since
									; we have an empty row
	return
 up_rotation
	call	GetBlockUp
	call	AdjustHeight
	return
 left_rotation
	call	GetBlockLeft
	call	AdjustHeight
	return
 down_rotation
	call	GetBlockDown
	call	AdjustHeight
	return

GetBlockRight:
	; indexes lookup table and puts it in BLOCKS_START_ADDRESS

	; changes height and width to appropriate values
	movlw	0x04
	movwf	currentHeight
	movlw	0x02
	movwf	currentWidth

	movf	blockNumber, W
	mulwf	currentHeight
	movff	PRODL, blockIndex
	; indexing table now
 	movlw	low BlockTableRight
	movwf	TBLPTRL
	movlw	high BlockTableRight
	movwf	TBLPTRH
	movlw	upper BlockTableRight
	movwf	TBLPTRU
	movf	blockIndex, W
	addwf	TBLPTRL, F
	movlw	0
	addwfc	TBLPTRH
	addwfc	TBLPTRU
	
	; now need to increment through ROM table and put the values into
	; the RAM table
	movff	currentHeight, writeLoopCount
  right_block_loop
	tblrd*+
   	movff	TABLAT, POSTINC1
	decf	writeLoopCount
	bnz		right_block_loop
	return

GetBlockUp:
	; indexes lookup table and puts it in BLOCKS_START_ADDRESS

	; changes height and width to appropriate values
	movlw	0x02
	movwf	currentHeight
	movlw	0x04
	movwf	currentWidth
	
	; correcting address of where we should write to in RAM
	movf	POSTINC1
	movf	POSTINC1

	movf	blockNumber, W
	mulwf	currentHeight
	movff	PRODL, blockIndex
	; indexing table now
 	movlw	low BlockTableUp
	movwf	TBLPTRL
	movlw	high BlockTableUp
	movwf	TBLPTRH
	movlw	upper BlockTableUp
	movwf	TBLPTRU
	movf	blockIndex, W
	addwf	TBLPTRL, F
	movlw	0
	addwfc	TBLPTRH
	addwfc	TBLPTRU
	
	; now need to increment through table, store in correct place
	movff	currentHeight, writeLoopCount
  up_block_loop
	tblrd*+
   	movff	TABLAT, POSTINC1
	decf	writeLoopCount
	bnz		left_block_loop
	return

GetBlockLeft:
	; indexes lookup table and puts it in BLOCKS_START_ADDRESS

	; changes height and width to appropriate values
	movlw	0x04
	movwf	currentHeight
	movlw	0x02
	movwf	currentWidth

	movf	blockNumber, W
	mulwf	currentHeight
	movff	PRODL, blockIndex
	; indexing table now
 	movlw	low BlockTableLeft
	movwf	TBLPTRL
	movlw	high BlockTableLeft
	movwf	TBLPTRH
	movlw	upper BlockTableLeft
	movwf	TBLPTRU
	movf	blockIndex, W
	addwf	TBLPTRL, F
	movlw	0
	addwfc	TBLPTRH
	addwfc	TBLPTRU
	
	; now need to increment through table, store in correct place
	movff	currentHeight, writeLoopCount
  left_block_loop
	tblrd*+
   	movff	TABLAT, POSTINC1
	decf	writeLoopCount
	bnz		left_block_loop
	return

GetBlockDown:
	; indexes lookup table and puts it in BLOCKS_START_ADDRESS

	; changes height and width to appropriate values
	movlw	0x02
	movwf	currentHeight
	movlw	0x04
	movwf	currentWidth

	movf	POSTINC1
	movf	POSTINC1

	movf	blockNumber, W
	mulwf	currentHeight
	movff	PRODL, blockIndex
	; indexing table now
 	movlw	low BlockTableDown
	movwf	TBLPTRL
	movlw	high BlockTableDown
	movwf	TBLPTRH
	movlw	upper BlockTableDown
	movwf	TBLPTRU
	movf	blockIndex, W
	addwf	TBLPTRL, F
	movlw	0
	addwfc	TBLPTRH
	addwfc	TBLPTRU
	
	; now need to increment through table, store in correct place
	movff	currentHeight, writeLoopCount
  down_block_loop
	tblrd*+
   	movff	TABLAT, POSTINC1
	decf	writeLoopCount
	bnz		down_block_loop
	return
;------------------------------- Screen Funcs ---------------------------------------------------------
DrawScreen:
	; Draws top screen then bottom screen by calling two helper funcs
	lfsr	0, ROWS_START_ADDRESS		; we need to reset the FSR0 so it points to the correct reg
	call	DrawScreenOne
	call	DrawScreenTwo
	return

DrawScreenOne:
	; Draws the screen by iterating through each row
	; and accessing the corresponding row through pointers
	; colCount is misnamed here, but I'm too lazy to change it
	; what it should really be, is a separate rowCount
	movlw	ROWS_PER_SCREEN
	movwf	colCount
repeat_one
	movff	colCount, PORTC
	movf	POSTINC0, W
	movwf	PORTD
	bsf		PORTC, 3			; the last thing we want to do is to turn this bit on
								; since when this turns on, the row will display
								; we want to make sure both the rest of the PORTC val is applied
								; and PORTD already has the correct value in it

	decf	colCount			; if we're not at the last row in the screen, keep going, else stop
	bnz		repeat_one
	bra		end_one
 end_one
	; this part is to make sure we don't end too early, otherwise we skip the last row of the screen
	movff	colCount, PORTC
	movf	POSTINC0, W
	movwf	PORTD
	bsf		PORTC, 3
	return

DrawScreenTwo:
	; See DrawScreenOne, but just sets a different bit to access the second screen
	movlw	ROWS_PER_SCREEN
	movwf	colCount
repeat_two
	movff	colCount, PORTC
	movf	POSTINC0, W
	movwf	PORTD
	bsf		PORTC,4
	decf	colCount
	bnz		repeat_two
	bra		end_two
 end_two
	movff	colCount, PORTC
	movf	POSTINC0, W
	movwf	PORTD
	bsf		PORTC, 4
	return

;--------------------------------- Drawing Frame Funcs ------------------------------------
WriteFrameToMem:
	; (1) - start at bottom address
	lfsr	0, SCREEN_START					; setting FSR0 to point to ROWS_START_ADDRESS
	movff	blockPosY, writeLoopCount
  row_loop									; looping to get to correct place in FSR0
	movf	POSTINC0
	decf	writeLoopCount
	bnz		row_loop

	movff	currentHeight, writeLoopCount	; here, we know how many times we have to iterate through
	lfsr	1, BLOCKS_START_ADDRESS+3		; setting FSR1 to point to bottom block that we need to draw
	movlw	0x00
	cpfseq	blockOffset
	call	CorrectBlock					; need to correct the height of the block in order to not draw
											; an empty row
  write_loop
	movff	POSTDEC1, currentRow		; moving from register to current row dummy reg

	; shifting into x position
	movff	blockPosX, shiftingCount
  shifting_loop
	rrncf	currentRow
	decf	shiftingCount
	bnz		shifting_loop				; end of shifting loop
	
	movf	currentRow, 0				; move currentRow into W
	iorwf	POSTDEC0, 1					; inclusive-OR to write the new line
	decf	writeLoopCount
	bnz		write_loop
	return

ClearFrameFromMem:
	
	lfsr	0, SCREEN_START							; setting FSR0 to point to ROWS_START_ADDRESS
	movff	blockPosY, writeLoopCount
  clear_row_loop									; looping to get to correct place in FSR0
	movf	POSTINC0
	decf	writeLoopCount
	bnz		clear_row_loop

	movff	currentHeight, writeLoopCount	; here, we know how many times we have to iterate through
	lfsr	1, BLOCKS_START_ADDRESS+3	; setting FSR1 to point to bottom block that we need to draw
  	movlw	0x00
	cpfseq	blockOffset
	call	CorrectBlock
  clear_loop
	movff	POSTDEC1, currentRow		; moving from register to current row dummy reg

	; Complementing the currentRow
	comf	currentRow, 1
	; shifting into x position
	movff	blockPosX, shiftingCount
  clear_shifting_loop
	rrncf	currentRow
	decf	shiftingCount
	bnz		clear_shifting_loop
	movf	currentRow, 0
	andwf	POSTDEC0, 1					; we AND the complemented currentRow and the row in memory
										; this removes the currently active blocks	
	decf	writeLoopCount
	bnz		clear_loop

	; after the clearing loop
 end_clear
	return

CorrectBlock:
	; function to make sure we're pointing in the right place in the
	; active block RAM table
	movff	blockOffset, blockOffsetCount
  block_offset_loop
	movf	POSTDEC1
	decf	blockOffsetCount
	bnz		block_offset_loop
	return

AdjustHeight:
	; correcting the height by checking if there are any empty rows
	; at the end of the RAM table
	lfsr	1, BLOCKS_START_ADDRESS+3
 height_check_loop
	movlw	0x00
	cpfseq	PLUSW1
	return
	movf	POSTDEC1
	decf	currentHeight
	incf	blockOffset
	bra		height_check_loop
	return

;--------------------------------- Game logic ---------------------------------------------
UpdateGame:
	; Main function that implements the logic of the game

	; first, check if we've detected any collisions
	call	DetectCollision
	btfsc	genBlockFlag, 1
	bra		if_coll_detected
 clear_write
	; if no collision, then check if we're at the bottom of the screen
	movlw	0x12
	cpfslt	blockPosY
	bra		if_16

	; if neither of the above conditions are true, then we can clear the frame
	; and then increment the y-coordinate of the block
	call	ClearFrameFromMem
	incf	blockPosY
 write_screen
	; when we jump here, we want to write the frame to memory
	; this is the final step before returning, so we also want to 
	; clear the updateGameFlag
	call	WriteFrameToMem
	bcf		updateGameFlag, 1
	return
 if_16
	; if we're at the bottom of the screen, then we want to signal
	; that we need to generate a new block
	bsf		genBlockFlag, 1
	bra		write_screen
 if_coll_detected
	; if there's a collision detected, then we want to check if the game is over
	; by seeing if the block is already currently at the top of the screen
	movlw	0x04
	cpfsgt	blockPosY
	bra		game_over
	call	ClearFullLines			; otherwise, we want to clear any full lines in the screen
	call	GenerateNewBlock		; and we want to generate a new block in any case
	bra		clear_write
 game_over
	; we basically just reset the screen and make a new block to restart the game
	call	InitializeRowTable	; reset the screen
	call	GenerateNewBlock
	bra		clear_write 

DetectCollision:
	lfsr	1, BLOCKS_START_ADDRESS+3
	lfsr	0, SCREEN_START
	movlw	0x00
	cpfseq	blockOffset
	call	CorrectBlock
	movff	currentHeight, blockOffsetCount
	movff	blockPosY, heightOffset

	call 	CheckBottomRow					; first, we check only the bottom row for coll
	; if no collision, then check next row
	btfsc	genBlockFlag, 1					; if there's a collision, then we return
	return			

	movlw	0x03							; square block number special case
	cpfseq	blockNumber						; don't want to check other rows for square block
	bra		check_other_rows
	return
  check_other_rows
	decf	heightOffset
	call	CheckOtherRow					; we now check other rows for collision
	btfss	genBlockFlag, 1
	return
	decf	blockOffsetCount
	bnz		check_other_rows
	return

CheckBottomRow:
	movf	heightOffset, W
	movff	PLUSW0, currentRow
	incf	blockPosY, W			; store back in WREG
	movff	PLUSW0, nextRow
	movf	currentRow, W			; put currentRow into W
	andwf	nextRow, 1				; store results in nextRow
	movff	blockPosX, shiftingCount


	tstfsz	shiftingCount			; check if 0 to prevent underflow
	bra		bottom_row_loop
	bra		end_shift_bottom	
 bottom_row_loop					; un-shift row so that the block is left-aligned
	rlncf	nextRow					; so that it lines up with how the blocks are stored in 
									; the RAM table
	decf	shiftingCount
	bnz		bottom_row_loop

 end_shift_bottom
	movf	POSTDEC1, W				; and the left-aligned row with the bottom of the block
	andwf	nextRow, 1				; in the RAM table. If it's not 0, then there's a collision
	movlw	0x00
	cpfsgt	nextRow
	return
	; collision is detected, we want to stop moving the block and reset the y position
	bsf		genBlockFlag, 1
	return

CheckOtherRow:
	movf	heightOffset, W
	movff	PLUSW0, currentRow
	incf	heightOffset, W			; store back in WREG
	movff	PLUSW0, nextRow
	movf	currentRow, W			; put currentRow into W
	andwf	nextRow, 1				; store results in nextRow
	
	movff	POSTINC1, currentRow	; this is the second row in the block
	movf	POSTDEC1, W				; bottom row in W
	andwf	currentRow, F			; currentRow should contain the redundant blocks
	comf	currentRow, F			; complemented to mask the later result
	
	movff	blockPosX, shiftingCount	
	tstfsz	blockPosX
	bra		collision_shift_loop
	bra		end_shifting_coll

 collision_shift_loop				; left aligning blocks
	rlncf	nextRow
	decf	shiftingCount
	bnz		collision_shift_loop

 end_shifting_coll
	movf	POSTDEC1, W
	andwf	nextRow				; nextRow contains the result of row and the next above row in the
								; active block RAM table
	movf	currentRow, W
	andwf	nextRow				; nextRow AND the complement of the redundant blocks (in currentRow)
	movlw	0x00
	cpfsgt	nextRow
	return
	; collision is detected, we want to stop moving the block and reset the y position
	bsf		genBlockFlag, 1
	return

ClearFullLines:
	lfsr	0, ROWS_START_ADDRESS+TOTAL_ROWS  ; start at bottom of screen
	movlw	TOTAL_ROWS
	movwf	rowCount						  ; for keeping track of which row we're in

 clearing_loop
	; checking if the line is completely on 
	movlw	0xFF
	cpfseq	POSTDEC0
	bra		back_from_clear
	bra		need_to_clear
 back_from_clear
	decf	rowCount
	bnz		clearing_loop
	return
 need_to_clear
	; need to shift every line down now
	movff	rowCount, shiftingCount		; move current rows left into shifting count
 shifting_down_loop
	movff	POSTINC0, POSTDEC0
	movf	POSTDEC0				; decrement again to get next line
	decf	shiftingCount
	bnz		shifting_down_loop
	
	; return to the row we were at before shifting the blocks down
	; then return to the other loop
	movff	rowCount, shiftingCount
	lfsr	0, ROWS_START_ADDRESS
 fsr0_loop
	movf	POSTINC0
	decf	shiftingCount
	bnz		fsr0_loop
	bra		back_from_clear

GenerateNewBlock:
	bcf		genBlockFlag, 1

	;reset x position, rotation state, etc.
	movlw	0x03
	movwf	blockPosX
	movwf	blockPosY
	movlw	0x01
	movwf	rotationState
	incf	blockLookup
	movlw	d'40'
	cpfslt	blockLookup				; our lookup table for the blocks has size 40
									; we don't want to accidentally index beyond that
	bra		reset_block_number
 call_rotate_block
	call	GetBlockNumber
	call	ClearFrameFromMem
	call	GetBlockFromMem
	call	WriteFrameToMem
	return
 reset_block_number
	clrf	blockLookup
	bra		call_rotate_block
	return

GetBlockNumber:
	; lookup table of randomly generated numbers to retrive the value of the next
	; block
 	movlw	low BlockNumberTable
	movwf	TBLPTRL
	movlw	high BlockNumberTable
	movwf	TBLPTRH
	movlw	upper BlockNumberTable
	movwf	TBLPTRU
	movf	blockLookup, W
	addwf	TBLPTRL, F
	movlw	0
	addwfc	TBLPTRH
	addwfc	TBLPTRU
	tblrd*
	movff	TABLAT, blockNumber
	return
;--------------------------------- Interrupt Handlers -------------------------------------
T0_ISR:
	movwf	W_temp
	movff	STATUS, status_temp
	movff	BSR, bsr_temp
	; just need to set the flag so it gets taken care of in main
	bsf		updateGameFlag, 1
	movff	bsr_temp, BSR
	movff	status_temp, STATUS
	movf	W_temp, W
	bcf		INTCON, TMR0IF
	retfie
;--------------------------------- ROM Tables ------------------------------------

BlockTableRight
	; Look-up Table 
	db  0x80, 0xC0, 0x80, 0x00	; the right rotated t block is block 0
	db  0x40, 0xC0, 0x80, 0x00	; z block is block 1
	db	0x80, 0x80, 0xC0, 0x00	; L block is block 2
	db  0xC0, 0xC0, 0x00, 0x00  ; square is block 3
	db	0x80, 0x80, 0x80, 0x80	; line is block 4

BlockTableUp
	db	0x40, 0xE0, 0xC0, 0x60
	db	0x20, 0xE0, 0xC0, 0xC0
	db	0xF0, 0x00

BlockTableLeft
	db	0x40, 0xC0, 0x40, 0x00
	db	0x40, 0xC0, 0x80, 0x00
	db 	0xC0, 0x40, 0x40, 0x00
	db	0xC0, 0xC0, 0x00, 0x00
	db	0x80, 0x80, 0x80, 0x80

BlockTableDown
	db	0xE0, 0x40, 0xC0, 0x60
	db	0xE0, 0x80, 0xC0, 0xC0
	db	0xF0, 0x00

BlockNumberTable
	; randomly generated to have the feeling of random blocks
	db	0x02, 0x03, 0x03, 0x00
	db	0x01, 0x02, 0x04, 0x04
	db	0x03, 0x03, 0x00, 0x02
	db	0x03, 0x04, 0x01, 0x00
	db	0x04, 0x02, 0x00, 0x02
	db	0x03, 0x01, 0x02, 0x03
	db	0x00, 0x02, 0x03, 0x02
	db	0x00, 0x02, 0x03, 0x03
	db	0x04, 0x04, 0x01, 0x00
	db	0x00, 0x01, 0x03, 0x04
	db	0x03, 0x02, 0x04, 0x00

END