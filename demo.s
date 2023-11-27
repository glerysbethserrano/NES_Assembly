
.include "constants.inc"

.segment "HEADER"
  .byte $4E, $45, $53, $1A
  .byte 2
  .byte 1
  .byte $01, $00

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

.segment "STARTUP"      ; "nes" linker config requires a STARTUP section, even if it's empty

.segment "CODE"         ; Main code segment for the program

reset:
  sei		                ; disable IRQs
  cld		                ; disable decimal mode
  ldx #$40
  stx $4017	            ; disable APU frame IRQ
  ldx #$ff       	      ; Set up stack
  txs		
  inx		                ; now X = 0
  stx PPUCTRL	          ; disable NMI
  stx PPUMASK 	        ; disable rendering
  stx $4010 	          ; disable DMC IRQs

;---------------------- first wait for vblank to make sure PPU is ready
vblankwait1:          ;
  bit PPUSTATUS       ;
  bpl vblankwait1     ;
                      ;
clear_memory:         ;
  lda #$00            ;
  sta $0000, x        ;
  sta $0100, x        ;
  sta $0200, x        ;
  sta $0300, x        ;
  sta $0400, x        ;
  sta $0500, x        ;
  sta $0600, x        ;
  sta $0700, x        ;
  inx                 ;
  bne clear_memory    ;
                      ;
                      ; second wait for vblank, PPU is ready after this
vblankwait2:          ;
  bit PPUSTATUS       ;
  bpl vblankwait2     ;
                      ;
;----------------------

;***************************   Main   ***************************

main:
    jsr initializePlayer1
    jsr load_palettes
    jsr LoadAttr
    jsr LoadBackground1

;***************************   Main   ***************************

;PPUCTRL
; 7  bit  0
; ---- ----
; VPHB SINN
; |||| ||||
; |||| ||++- Base nametable address
; |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
; |||| |+--- VRAM address increment per CPU read/write of PPUDATA
; |||| |     (0: add 1, going across; 1: add 32, going down)
; |||| +---- Sprite pattern table address for 8x8 sprites
; ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
; |||+------ Background pattern table address (0: $0000; 1: $1000)
; ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
; |+-------- PPU master/slave select
; |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
; +--------- Generate an NMI at the start of the
;            vertical blanking interval (0: off; 1: on)

; PPUMASK
; 7  bit  0
; ---- ----
; BGRs bMmG
; |||| ||||
; |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
; |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
; |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
; |||| +---- 1: Show background
; |||+------ 1: Show sprites
; ||+------- Emphasize red (green on PAL/Dendy)
; |+-------- Emphasize green (red on PAL/Dendy)
; +--------- Emphasize blue

;-----------------------;
enable_rendering:       ;
  lda #%10010000	    ; Enable NMI
  sta PPUCTRL           ; $2000
  lda #%00011000	    ; Enable Sprites
  sta PPUMASK           ; $2001
;-----------------------;

;----------------
forever:        ;
  jmp forever   ;
;----------------

; ***************************   Start of NMI   ***************************
nmi:  

  ;-----;
  PHA   ;                                
  TXA   ;                                                         
  PHA   ; Save the registers on the stack                                                    
  TYA   ;                                                         
  PHA   ;       
  ;-----;

 ;------------;
  ldx #$0000  ;
  stx $2003   ; This is where the 64 sprites will be stored.
  ;-----------;

  jsr readController1

  lda COUNTER_HEALTH
  cmp #$03
  bmi notDead
  jsr loadDeadFrame
  jmp endButtonCheck

  notDead:
  jsr checkLeftButtonPressed
  jsr checkRightButtonPressed
  jsr checkAButtonPressed 
  jsr checkBButtonPressed 
  jsr checkSelectButtonPressed
  jsr checkNotMovingPressed

  lda SELECT_PRESSED
  cmp #$01
  bne endButtonCheck
  inc $0226
  inc $022A
  inc $022E
  inc $0232

  endButtonCheck:
  jsr gravityEffect
  

  ;-----------;
  lda #$02    ;
  sta $4014   ; This makes a copy of the memory space at address $0200 to $02FF in the PPU OAM.
  ;-----------;
  
  lda #$0000
  sta $2005

  ;-----;
  PLA   ;                                    
  TAY   ;                                                        
  PLA   ;                                                        
  TAX   ; Restore values from stack                                                    
  PLA   ;                                                         
  RTI   ; NMI is done 
  ;-----;    

; ***************************   End of NMI   ***************************


;--------------- Subrutines that make everything possible! ---------------

;------Load Attribute Table-----;
LoadAttr:                       ;
    lda PPUSTATUS               ;
    LDA #$23                    ;
    STA $2006                   ;
    LDA #$c0                    ;
    STA $2006                   ;
    LDX #$00                    ;
LoadAttrLoop:                   ;
    LDA attributeTable, x      ;
    STA $2007                   ;
    INX                         ;
    CPX #$40                    ;
    BNE LoadAttrLoop            ;
    rts                         ;
;-------------------------------;

;-----------Load Palettes-----------;
load_palettes:                      ;
  lda PPUSTATUS                     ;
  lda #$3f                          ;
  sta PPUADDR                       ;
  lda #$00                          ;
  sta PPUADDR                       ;
  ldx #$00                          ;
                                    ;
@load_palettes_loop:                ;
  lda palettes, x                   ;
  sta PPUDATA                       ;
  inx                               ;
  cpx #$20                          ;
  bne @load_palettes_loop           ;
  rts                               ;
;-----------------------------------;

;------------Load Background------------;
LoadBackground1:                        ;
    ;reset scroll                       ;
    lda #$00                            ;
    sta $2005                           ;
    sta $2005                           ;
                                        ;
    lda PPUSTATUS                       ;
    LDA #$20                            ;
    STA $2006                           ;
    LDA #$00                            ;
    STA $2006                           ;
    LDX #$00                            ;
LoadBackgroundLoop1:                    ;
    LDA background, x                   ;
    STA $2007                           ;
    INX                                 ;
    CPX #$00                            ;
    BNE LoadBackgroundLoop1             ;
                                        ;
LoadBackgroundLoop2:                    ;
    LDA background+256, x               ;
    STA $2007                           ;
    INX                                 ;
    CPX #$00                            ;
    BNE LoadBackgroundLoop2             ;
                                        ;
LoadBackgroundLoop3:                    ;
    LDA background+512, x               ;
    STA $2007                           ;
    INX                                 ;
    CPX #$00                            ;
    BNE LoadBackgroundLoop3             ;
                                        ;
LoadBackgroundLoop4:                    ;
    LDA background+768, x               ;
    STA $2007                           ;
    INX                                 ;
    CPX #$c0                            ;
    BNE LoadBackgroundLoop4             ;
    rts                                 ;
;---------------------------------------;

;-----Initialize values for player-----;
initializePlayer1:                     ;
  ldx #$00                             ;
  stx INDEX_FRAME1                     ; INDEX_FRAME = 0
  stx COUNTER_FRAME1                   ; COUNTER_FRAME1 = 0
                                       ;
  ldx #$0006                           ;
  stx NMI_FRECUENCY_FRAME1             ; NMI_FRECUENCY_FRAME1 = 6
                                       ;
  ldx #$01                             ;
  stx DIRECTION1                       ; DIRECTION1 = 1 = left    ***0 = right***
                                       ;
  jsr initializePlayer1Sprites         ; loads the sprites of the player looking still to the left
  rts                                  ;
                                       ;
initializePlayer1Sprites:              ;
  ldx #$00                             ;
  @loop:                               ;        
    lda initializationSprites, x       ;
    sta $0224, x                       ; 
    inx                                ;
    cpx #$10                           ;
    bne @loop                          ;
  rts                                  ;
;--------------------------------------;

; Load dead character sprites
;----------------------------------;
loadDeadFrame:                     ;
  lda DIRECTION1                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadDeadRight              ; If islooking to the left
    jmp endLoadDeadFrame           ;
  @else:                           ;
    jsr loadDeadLeft               ; If islooking to the right
endLoadDeadFrame:                  ;
  rts                              ;
                                   ;
loadDeadLeft:                      ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda deadLeft, x                ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda deadLeft, x                ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
loadDeadRight:                     ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda deadRight, x               ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda deadRight, x               ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;

; Load character sprite while not moving
;----------------------------------;
loadStillFrame:                    ;
  lda DIRECTION1                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadStillRight             ; If islooking to the left
    jmp endLoadStillFrame          ;
  @else:                           ;
    jsr loadStillLeft              ; If islooking to the right
endLoadStillFrame:                 ;
  rts                              ;
                                   ;
loadStillLeft:                     ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda stillLeft, x               ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda stillLeft, x               ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
loadStillRight:                    ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda stillRight, x              ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda stillRight, x              ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;

; Load character sprite when attacking
;----------------------------------;
loadAttackFrame:                   ;
  lda DIRECTION1                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadAttackRight            ; If islooking to the left
    jmp endLoadAttackFrame         ;
  @else:                           ;
    jsr loadAttackLeft             ; If islooking to the right
endLoadAttackFrame:                ;
  rts                              ;
                                   ;
loadAttackLeft:                    ;
  clc                              ;
  lda IS_TOUCHING_FLOOR            ;
  cmp #$0001                       ;
  beq loadFloorAttackLeft          ;
                                   ;
  loadAirAttackLeft:               ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda leftFrame5, x              ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda leftFrame5, x              ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
  loadFloorAttackLeft:             ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda leftFrame6, x              ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda leftFrame6, x              ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
loadAttackRight:                   ;
  clc                              ;
  lda IS_TOUCHING_FLOOR            ;
  cmp #$0001                       ;
  beq loadFloorAttackRight         ;
                                   ;
  loadAirAttackRight:              ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda rightFrame5, x             ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda rightFrame5, x             ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
  loadFloorAttackRight:            ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda rightFrame6, x             ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda rightFrame6, x             ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;

; Load the animation of walking to the left
;-----------------------------------;
loadLeftAnimation:                  ;
  lda $0008                         ;
  cmp #$01                          ;
  bne continueLeftAnimation         ;
                                    ;
  jmp skipLeftAnimation             ;
                                    ;
continueLeftAnimation:              ;
  lda #$01                          ;
  sta DIRECTION1                    ;
                                    ;
  lda INDEX_FRAME1                  ;
  ldx $00                           ;
                                    ;
  cmp #$00                          ;
  beq loadLeftFrame1                ;
  cmp #$01                          ;
  beq loadLeftFrame2                ;
  cmp #$02                          ;
  beq loadLeftFrame3                ;
  cmp #$03                          ;
  beq loadLeftFrame4                ;
endLoadLeftAnimation:               ;
  clc                               ;
  lda COUNTER_FRAME1                ;
  adc #$01                          ;
  sta COUNTER_FRAME1                ;
  cmp NMI_FRECUENCY_FRAME1          ;
  bne @else1                        ;
    lda #$00                        ;
    sta COUNTER_FRAME1              ;
                                    ;
    lda INDEX_FRAME1                ;
    adc #$01                        ;
    cmp #$04                        ;
    bne @else2                      ;
      lda #$00                      ;
    @else2:                         ;
      sta INDEX_FRAME1              ;
  @else1:                           ;
    rts                             ;
                                    ;
                                    ;
loadLeftFrame1:                     ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda leftFrame1, x               ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda leftFrame1, x               ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation          ;
                                    ;
loadLeftFrame2:                     ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda leftFrame2, x               ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda leftFrame2, x               ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation          ;
                                    ;
loadLeftFrame3:                     ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda leftFrame3, x               ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda leftFrame3, x               ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation          ;
                                    ;
loadLeftFrame4:                     ;
  ldx #$00                          ;
  ldy #$00                          ; 
  @loop:                            ;  
    lda leftFrame4, x               ;
    sta $0225, y                    ; 
    iny                             ; 
    inx                             ; 
    lda leftFrame4, x               ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ; 
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation          ;
                                    ;
skipLeftAnimation:                  ;
  rts                               ;
;-----------------------------------;

; Load the animation of walking to the right
;-----------------------------------;
loadRightAnimation:                 ;
  lda $0008                         ;
  cmp #$01                          ;
  bne continueRightAnimation        ;
                                    ;
  jmp skipRightAnimation            ;
                                    ;
continueRightAnimation:             ;
  lda #$00                          ; 
  sta DIRECTION1                    ;
                                    ;
  lda INDEX_FRAME1                  ;
  ldx $00                           ;
                                    ;
  cmp #$00                          ;
  beq loadRightFrame1               ;
  cmp #$01                          ;
  beq loadRightFrame2               ;
  cmp #$02                          ;
  beq loadRightFrame3               ;
  cmp #$03                          ;
  beq loadRightFrame4               ; 
endLoadRightAnimation:              ;
  clc                               ;
  lda COUNTER_FRAME1                ;
  adc #$01                          ;
  sta COUNTER_FRAME1                ;
  cmp NMI_FRECUENCY_FRAME1          ;
  bne @else1                        ;
    lda #$00                        ;
    sta COUNTER_FRAME1              ;
                                    ;
    lda INDEX_FRAME1                ;
    adc #$01                        ;
    cmp #$04                        ;
    bne @else2                      ;
      lda #$00                      ; 
    @else2:                         ;
      sta INDEX_FRAME1              ; 
  @else1:                           ;
    rts                             ;
                                    ;
                                    ;
loadRightFrame1:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda rightFrame1, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda rightFrame1, x              ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ; 
  jmp endLoadRightAnimation         ;
                                    ;
loadRightFrame2:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ; 
    lda rightFrame2, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda rightFrame2, x              ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ; 
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadRightAnimation         ;
                                    ;
loadRightFrame3:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda rightFrame3, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda rightFrame3, x              ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadRightAnimation         ;
                                    ;
loadRightFrame4:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda rightFrame4, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda rightFrame4, x              ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadRightAnimation         ;
                                    ;
skipRightAnimation:                 ;
  rts                               ;
;-----------------------------------;

; Load the animation of jumping
;-----------------------------------;
loadJumpFrame:                      ;
lda DIRECTION1                      ;
  and #%00000001                    ; 
  bne @else                         ;
    jsr loadJumpRight               ; If islooking to the left
    jmp endLoadJumpFrame            ;
  @else:                            ;
    jsr loadJumpLeft                ; If islooking to the right
endLoadJumpFrame:                   ;
  rts                               ;
                                    ;
loadJumpLeft:                       ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda leftFrame3, x               ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda leftFrame3, x               ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  rts                               ;
                                    ; 
loadJumpRight:                      ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda rightFrame3, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda rightFrame3, x              ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ; 
    bne @loop                       ;
  rts                               ;
;-----------------------------------;

;-----read inputs of control 1-------;
  readController1:                   ;
    lda #1                           ;
    sta INPUT1                       ;
                                     ;
    sta $4016                        ;
    lda #0                           ;
    sta $4016                        ;
  readLoop:                          ;
    lda $4016                        ;
    lsr a                            ;
    rol INPUT1                       ;
    bcc readLoop                     ;
    rts                              ;
;------------------------------------;

;-----------check left button------------;
  checkLeftButtonPressed:                ;
    lda INPUT1                           ;
    and #%00000010                       ;
    beq endCheckLeftButtonPressed        ;
    jsr loadLeftAnimation                ;
    jsr moveToLeftPlayer1                ;
  endCheckLeftButtonPressed:             ;
    rts                                  ;
;----------------------------------------;

;-----------check right button-----------;
  checkRightButtonPressed:               ;
    lda INPUT1                           ;
    and #%00000001                       ;
    beq endCheckRightButtonPressed       ;
    jsr loadRightAnimation               ;
    jsr moveToRightPlayer1               ;
  endCheckRightButtonPressed:            ;
    rts                                  ;
;----------------------------------------;

;-----------check A button---------------;
  checkAButtonPressed:                   ;
    lda INPUT1                           ;
    and #%10000000                       ;
    beq AButtonNotPressed                ;
    jsr jumpingEffect                    ;
    jmp endCheckAButtonPressed           ;
  AButtonNotPressed:                     ;
    lda #$19                             ;
    sta COUNTER_JUMP                     ;
  endCheckAButtonPressed:                ;
    rts                                  ;
;----------------------------------------;

;-----------check B button---------------;
  checkBButtonPressed:                   ;
    lda INPUT1                           ;
    and #%01000000                       ;
    beq endCheckBButtonPressed           ;
    jsr loadAttackFrame                  ;
  endCheckBButtonPressed:                ;
    rts                                  ;
;----------------------------------------;

;-----------check select button----------;
  checkSelectButtonPressed:              ;
    lda INPUT1                           ;
    and #%00100000                       ;
    beq @else                            ; 
    lda SELECT_PRESSED                   ;
    cmp #$01                             ;
    beq endCheckSelectButtonPressed      ;
    inc COUNTER_HEALTH                   ;
    lda #$01                             ;
    sta SELECT_PRESSED                   ;
    jmp endCheckSelectButtonPressed      ;
  @else:                                 ;
    lda #$00                             ;
    sta SELECT_PRESSED                   ;
  endCheckSelectButtonPressed:           ;
    rts                                  ;
;----------------------------------------;

;-----------check if not moving----------;
  checkNotMovingPressed:                 ;
    lda INPUT1                           ;
    and #%11000011                       ;
    bne @else                            ;
      jsr loadStillFrame                 ;  <--- if not moving
    jmp endcheckNotMovingPressed         ;
  @else:                                 ;
                                         ;  <--- if moving
  endcheckNotMovingPressed:              ;
    rts                                  ;
;----------------------------------------;

; Moves to the left the 4 tiles saved in RAM by 1 pixel.
;---------------------;
moveToLeftPlayer1:    ;
  clc                 ;
  lda $0227           ;
  cmp #$0010          ;
  bcc endMoveToLeft   ;
                      ;
  clc                 ;
  lda $0227           ;
  sbc #$0001          ;
  sta $0227           ;
                      ;
  clc                 ; 
  lda $022B           ;
  sbc #$0001          ;
  sta $022B           ;
                      ;
  clc                 ;
  lda $022F           ;
  sbc #$0001          ;
  sta $022F           ;
                      ;
  clc                 ;
  lda $0233           ;
  sbc #$0001          ;
  sta $0233           ;
                      ;
  lda #$00            ;
  sta $0008           ;
  rts                 ;
                      ;
endMoveToLeft:        ;
  lda #$01            ;
  sta $0008           ;
  rts                 ;
;---------------------;


; Moves to the right the 4 tiles saved in RAM by 2 pixels.
;---------------------;
moveToRightPlayer1:   ;
  clc                 ;
  lda $0227           ;
  cmp #$00F6          ;
  bcs endMoveToRight  ;
                      ;
  clc                 ;
  lda $0227           ;
  adc #$0002          ;
  sta $0227           ;
                      ;
  clc                 ;
  lda $022B           ;
  adc #$0002          ;
  sta $022B           ;
                      ;
  clc                 ;
  lda $022F           ;
  adc #$0002          ;
  sta $022F           ;
                      ;
  clc                 ;
  lda $0233           ;
  adc #$0002          ;
  sta $0233           ;
                      ;
  lda #$00            ;
  sta $0008           ;
  rts                 ;
                      ;
endMoveToRight:       ;  
  lda #$01            ;
  sta $0008           ;       
  rts                 ;
;---------------------;

;-------------------------------;
gravityEffect:                  ;
  ldx COUNTER_JUMP              ;
  cpx #$15                      ;
  bmi endGravityEffect          ;
  jsr checkIfTouchingFloor      ;
  jsr checkIfTouchingPlatform   ;
                                ;
  clc                           ;
  lda IS_TOUCHING_FLOOR         ;
  cmp #$0001                    ;
  beq endGravityEffect          ;
                                ;
  clc                           ;
  lda $0224                     ;
  adc #GRAVITY                  ;
  sta $0224                     ;
                                ;
  clc                           ;
  lda $0228                     ;
  adc #GRAVITY                  ;
  sta $0228                     ;
                                ;
  clc                           ;
  lda $022C                     ;
  adc #GRAVITY                  ;
  sta $022C                     ;
                                ;
  clc                           ;
  lda $0230                     ;
  adc #GRAVITY                  ;
  sta $0230                     ;
                                ;
endGravityEffect:               ;
    rts                         ; 
;-------------------------------;

;-------------------------------;
jumpingEffect:                  ;
  ldx COUNTER_JUMP              ;
  inx                           ;
  stx COUNTER_JUMP              ;
  cpx #$15                      ;
  bpl endJumpingEffect          ;
  jsr loadJumpFrame             ;
                                ;
  clc                           ;
  lda $0224                     ;
  sbc #GRAVITY                  ;
  sta $0224                     ;
                                ;
  clc                           ;
  lda $0228                     ;
  sbc #GRAVITY                  ;
  sta $0228                     ;
                                ;
  clc                           ;
  lda $022C                     ;
  sbc #GRAVITY                  ;
  sta $022C                     ;
                                ;
  clc                           ;
  lda $0230                     ;
  sbc #GRAVITY                  ;
  sta $0230                     ;
                                ;
endJumpingEffect:               ;
    rts                         ; 
;-------------------------------;

;-------------------------------;
checkIfTouchingFloor:           ;
  clc                           ;
  lda #$0000                    ;
  sta IS_TOUCHING_FLOOR         ;
                                ;
  clc                           ;
  lda $0224                     ;
  cmp #$B5                      ;
  bpl endCheckIfTouchingFloor   ;   
  cmp #$AF                      ;
  bmi endCheckIfTouchingFloor   ;
  lda #$0001                    ;
  sta IS_TOUCHING_FLOOR         ;
  lda #$00                      ;
  sta COUNTER_JUMP              ;
                                ;
endCheckIfTouchingFloor:        ;
  rts                           ;
;-------------------------------;

;-----------------------------------;
checkIfTouchingPlatform:            ;
  lda $0227                         ;
  cmp #$4D                          ;    <--- left
  bmi endCheckIfTouchingPlatform    ;
  cmp #$C0                          ;    <--- right
  bpl endCheckIfTouchingPlatform    ;
  lda $0224                         ;
  cmp #$7F                          ;    <--- bottom
  bpl endCheckIfTouchingPlatform    ;
  cmp #$6F                          ;    <--- top
  bmi endCheckIfTouchingPlatform    ;
  lda #$0001                        ;
  sta IS_TOUCHING_FLOOR             ;
  lda #$00                          ;
  sta COUNTER_JUMP                  ;
                                    ;
endCheckIfTouchingPlatform:         ;
  rts                               ;
;-----------------------------------;

;--------------------------- Binary/Hexadecimal Data ---------------------------

;********************** Animation **********************

initializationSprites:
  .byte INITIAL1_Y, $01, $40, INITIAL1_X + $08
  .byte INITIAL1_Y, $02, $40, INITIAL1_X
  .byte INITIAL1_Y + $08, $11, $40, INITIAL1_X + $08
  .byte INITIAL1_Y + $08, $12, $40, INITIAL1_X 

leftFrame1:
  .byte $07, $40 
  .byte $08, $40
  .byte $17, $40
  .byte $18, $40

leftFrame2:
  .byte $04, $40
  .byte $05, $40 
  .byte $14, $40
  .byte $15, $40

leftFrame3:
  .byte $0A, $40
  .byte $0B, $40
  .byte $1A, $40
  .byte $1B, $40

leftFrame4:
  .byte $04, $40
  .byte $05, $40
  .byte $14, $40
  .byte $15, $40

leftFrame5:               ; jumping attack
  .byte $21, $40
  .byte $22, $40
  .byte $31, $40
  .byte $32, $40

leftFrame6:               ; walking attack
  .byte $24, $40
  .byte $25, $40
  .byte $34, $40
  .byte $35, $40

stillLeft:
  .byte $01, $40 
  .byte $02, $40
  .byte $11, $40 
  .byte $12, $40 

deadLeft:
  .byte $00, $00
  .byte $00, $00
  .byte $1D, $40 
  .byte $1E, $40
  

stillRight:
  .byte $02, $04 
  .byte $01, $04  
  .byte $12, $04 
  .byte $11, $04

deadRight:
  .byte $00, $00
  .byte $00, $00
  .byte $1E, $04 
  .byte $1D, $04  

rightFrame1:
  .byte $08, $04 
  .byte $07, $04
  .byte $18, $04
  .byte $17, $04

rightFrame2:
  .byte $05, $04
  .byte $04, $04
  .byte $15, $04
  .byte $14, $04

rightFrame3:
  .byte $0B, $04 
  .byte $0A, $04
  .byte $1B, $04
  .byte $1A, $04

rightFrame4:
  .byte $05, $04 
  .byte $04, $04
  .byte $15, $04
  .byte $14, $04

rightFrame5:               ; jumping attack
  .byte $22, $04
  .byte $21, $04
  .byte $32, $04
  .byte $31, $04

rightFrame6:               ; walking attack
  .byte $25, $04
  .byte $24, $04
  .byte $35, $04
  .byte $34, $04

  lifeSprite:
  .byte $27, $04
  .byte $28, $04
  .byte $37, $04
  .byte $38, $04

;*******************************************************

palettes:                            
        ;00  01   10   11                   
  .byte $0f, $37, $17, $27; 00              
  .byte $0f, $30, $17, $27; 01   background     
  .byte $0f, $3c, $38, $19; 02              
  .byte $0f, $2d, $0c, $09; 03              
                                            
  .byte $0f, $37, $07, $0c; 04               
  .byte $0f, $26, $07, $0c; 05   foreground         
  .byte $0f, $16, $0f, $0f; 06              
  .byte $0f, $3d, $0f, $0f; 07              

attributeTable:
  ; 7654 3210
  ; |||| ||++- Color bits 3-2 for top left quadrant of this byte
  ; |||| ++--- Color bits 3-2 for top right quadrant of this byte
  ; ||++------ Color bits 3-2 for bottom left quadrant of this byte
  ; ++-------- Color bits 3-2 for bottom right quadrant of this byte

  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa 
  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa 
  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa 
  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa  
  .byte $aa, $aa, $a2, $a0, $a0, $a0, $aa, $aa 
  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa  
  .byte $00, $00, $00, $00, $00, $00, $40, $10 
  .byte $00, $00, $00, $00, $00, $00, $00, $00 

background:
	.byte $02,$02,$15,$02,$02,$15,$15,$15,$02,$02,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$15,$02,$15,$15,$15,$15,$15,$15,$15,$02,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$02,$15,$15,$15,$15,$15,$15,$15,$02,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$15,$02,$15,$15,$15,$15,$15,$15,$15,$02,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$15,$02,$02,$15,$15,$15,$02,$02,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$15,$15,$02,$02,$02,$02,$02,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$08,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$09,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$04,$03,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$06,$07,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$05,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$0f,$0e,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$05,$01,$0c,$0d,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$05,$01,$0a,$0b,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$05,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$14,$11,$13,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
                                    
background_tiles:                           
.segment "CHARS"                            
.incbin "Project2_background_sprites.chr"                         