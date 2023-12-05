
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

;---------------------; first wait for vblank to make sure PPU is ready
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

; -----------------------   Main   -----------------------

main:
    jsr initializePlayer1
    jsr initializePlayer2
    jsr load_palettes
    jsr LoadAttr
    jsr LoadBackground1

; -----------------------   Main   -----------------------

;-----------------------;
enable_rendering:       ;
  lda #%10010000	      ; Enable NMI
  sta PPUCTRL           ; $2000
  lda #%00011000	      ; Enable Sprites
  sta PPUMASK           ; $2001
;-----------------------;

;----------------
forever:        ;
  jmp forever   ;
;----------------

; -----------------------   Start of NMI   -----------------------
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

  ;--------- Player 1 -----------;
  jsr readController1

  lda COUNTER_HEALTH1
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
  jsr checkIfColliding

  endButtonCheck:
  jsr gravityEffect

  ;------------------------------;

  ;********** Player 2 **********;
  jsr readController2

  lda COUNTER_HEALTH2
  cmp #$03
  bmi notDead2
  jsr loadDeadFrame2
  jmp Hurt2

  notDead2:
    jsr checkLeftButtonPressed2
    jsr checkRightButtonPressed2
    jsr checkAButtonPressed2 
    jsr checkBButtonPressed2 
    jsr checkSelectButtonPressed2
    jsr checkNotMovingPressed2
    jsr checkIfColliding

  Hurt2:
    lda B_PRESSED2
    cmp #$01
    bne checkIfP2isHurt
    lda IS_COLLIDING1
    cmp #$01
    bne checkIfP2isHurt
    inc $0226
    inc $022A
    inc $022E
    inc $0232
    jsr loadHeartSprites

  checkIfP2isHurt:
    jsr loadHeartSprites
    lda B_PRESSED1
    cmp #$01
    bne endButtonCheck2
    lda IS_COLLIDING1
    cmp #$01
    bne endButtonCheck2
    inc $0236
    inc $023A
    inc $023E
    inc $0242
  
  endButtonCheck2:
    jsr loadHeartSprites2
    jsr gravityEffect2
  ;******************************;

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

; -----------------------   End of NMI   -----------------------


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
    LDA attributeTable, x       ;
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

;----Initialize values for player 1----;
initializePlayer1:                     ;
  ldx #$00                             ;
  stx INDEX_FRAME1                     ;
  stx COUNTER_FRAME1                   ;
                                       ;
  ldx #$0006                           ;
  stx NMI_FRECUENCY_FRAME1             ;
                                       ;
  ldx #$00                             ;
  stx DIRECTION1                       ;
                                       ;
  jsr initializePlayer1Sprites         ;
  rts                                  ;
                                       ;
initializePlayer1Sprites:              ;
  ldx #$00                             ;
  @loop:                               ;        
    lda initializationSprites1, x      ;
    sta $0224, x                       ; 
    inx                                ;
    cpx #$10                           ;
    bne @loop                          ;
  rts                                  ;
;--------------------------------------;

;****Initialize values for player 2****;
initializePlayer2:                     ;
  ldx #$00                             ;
  stx INDEX_FRAME2                     ;
  stx COUNTER_FRAME2                   ;
                                       ;
  ldx #$0006                           ;
  stx NMI_FRECUENCY_FRAME2             ;
                                       ;
  ldx #$01                             ;
  stx DIRECTION2                       ;
                                       ;
  jsr initializePlayer2Sprites         ;
  rts                                  ;
                                       ;
initializePlayer2Sprites:              ;
  ldx #$00                             ;
  @loop:                               ;        
    lda initializationSprites2, x      ;
    sta $0234, x                       ; 
    inx                                ;
    cpx #$10                           ;
    bne @loop                          ;
  rts                                  ;
;**************************************;

;---------------------------;
loadHeartSprites:           ;
  lda COUNTER_HEALTH1       ;
  cmp #$01                  ;
  beq DrawOneHeart          ; If 2, draw 1 heart
  cmp #$02                  ;
  beq DrawTwoHearts         ; If 1, draw 2 hearts
                            ; If it is 0 (or any other value), then draw 3 hearts by default
  DrawThreeHearts:          ;
  ldx #$00                  ; Reset index for sprite data
  @loop1:                   ;
    lda Life, x             ; Load a byte from the heart sprite data
    sta $0244, x            ; Store it in the sprite memory
    inx                     ; Increment the index
    cpx #$30                ; Compare the index with the byte count for 3 hearts
    bne @loop1              ; Continue the loop if not reached the count
  rts                       ; Return from subroutine
                            ;
DrawTwoHearts:              ;
  ldx #$00                  ;
  @loop2:                   ;
    lda Life, x             ;
    sta $0245, x            ;
    inx                     ;
    cpx #$20                ;
    bne @loop2              ;
  rts                       ;
                            ;
DrawOneHeart:               ;
  ldx #$00                  ;
  @loop3:                   ;
    lda Life, x             ;
    sta $0246, x            ;
    inx                     ;
    cpx #$10                ;
    bne @loop3              ;
  rts                       ;
;---------------------------;

;***************************;
loadHeartSprites2:          ;
  lda COUNTER_HEALTH2       ;
  cmp #$01                  ;
  beq DrawOneHeart2         ;
  cmp #$02                  ;
  beq DrawTwoHearts2        ;
                            ;
  DrawThreeHearts2:         ;
  ldx #$00                  ;
  @2loop1:                  ;
    lda Life2, x            ;
    sta $0274, x            ;
    inx                     ;
    cpx #$30                ;
    bne @2loop1             ;
  rts                       ;
                            ;
DrawTwoHearts2:             ;
  ldx #$00                  ;
  @2loop2:                  ;
    lda Life2, x            ;
    sta $0275, x            ;
    inx                     ;
    cpx #$20                ;
    bne @2loop2             ;
  rts                       ;
                            ;
DrawOneHeart2:              ;
  ldx #$00                  ;
  @2loop3:                  ;
    lda Life2, x            ;
    sta $0276, x            ;
    inx                     ;
    cpx #$10                ;
    bne @2loop3             ;
  rts                       ;
;***************************;



; Load dead player 1 sprites
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
    lda dead1Left, x               ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda dead1Left, x               ;
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
    lda dead1Right, x              ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda dead1Right, x              ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;

; Load dead player 2 sprites
;**********************************;
loadDeadFrame2:                    ;
  lda DIRECTION2                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadDeadRight2             ; If islooking to the left
    jmp endLoadDeadFrame2          ;
  @else:                           ;
    jsr loadDeadLeft2              ; If islooking to the right
endLoadDeadFrame2:                 ;
  rts                              ;
                                   ;
loadDeadLeft2:                     ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda dead2Left, x               ;
    sta $0235, y                   ;
    iny                            ;
    inx                            ;
    lda dead2Left, x               ;
    sta $0235, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
loadDeadRight2:                    ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda dead2Right, x              ;
    sta $0235, y                   ;
    iny                            ;
    inx                            ;
    lda dead2Right, x              ;
    sta $0235, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;**********************************;

; Load player 1 sprite while not moving
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
    lda still1Left, x              ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda still1Left, x              ;
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
    lda still1Right, x             ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda still1Right, x             ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;

; Load player 2 sprite while not moving
;**********************************;
loadStillFrame2:                   ;
  lda DIRECTION2                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadStillRight2            ; If islooking to the left
    jmp endLoadStillFrame2         ;
  @else:                           ;
    jsr loadStillLeft2             ; If islooking to the right
endLoadStillFrame2:                ;
  rts                              ;
                                   ;
loadStillLeft2:                    ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda still2Left, x              ;
    sta $0235, y                   ;
    iny                            ;
    inx                            ;
    lda still2Left, x              ;
    sta $0235, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
loadStillRight2:                   ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda still2Right, x             ;
    sta $0235, y                   ;
    iny                            ;
    inx                            ;
    lda still2Right, x             ;
    sta $0235, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;**********************************;

; Load player 1 sprite when attacking
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
  lda IS_TOUCHING_FLOOR1           ;
  cmp #$0001                       ;
  beq loadFloorAttackLeft          ;
                                   ;
  loadAirAttackLeft:               ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda left1Frame5, x             ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda left1Frame5, x             ;
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
    lda left1Frame6, x             ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda left1Frame6, x             ;
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
  lda IS_TOUCHING_FLOOR1           ;
  cmp #$0001                       ;
  beq loadFloorAttackRight         ;
                                   ;
  loadAirAttackRight:              ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda right1Frame5, x            ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda right1Frame5, x            ;
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
    lda right1Frame6, x            ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda right1Frame6, x            ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;

; Load player 2 sprite when attacking
;----------------------------------;
loadAttackFrame2:                  ;
  lda DIRECTION2                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadAttackRight2           ; If islooking to the left
    jmp endLoadAttackFrame2        ;
  @else:                           ;
    jsr loadAttackLeft2            ; If islooking to the right
endLoadAttackFrame2:               ;
  rts                              ;
                                   ;
loadAttackLeft2:                   ;
  clc                              ;
  lda IS_TOUCHING_FLOOR2           ;
  cmp #$0001                       ;
  beq loadFloorAttackLeft2         ;
                                   ;
  loadAirAttackLeft2:              ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda left2Frame5, x             ;
    sta $0235, y                   ;
    iny                            ;
    inx                            ;
    lda left2Frame5, x             ;
    sta $0235, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
  loadFloorAttackLeft2:            ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda left2Frame6, x             ;
    sta $0235, y                   ;
    iny                            ;
    inx                            ;
    lda left2Frame6, x             ;
    sta $0235, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
loadAttackRight2:                  ;
  clc                              ;
  lda IS_TOUCHING_FLOOR2           ;
  cmp #$0001                       ;
  beq loadFloorAttackRight2        ;
                                   ;
  loadAirAttackRight2:             ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda right2Frame5, x            ;
    sta $0235, y                   ;
    iny                            ;
    inx                            ;
    lda right2Frame5, x            ;
    sta $0235, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
  loadFloorAttackRight2:           ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda right2Frame6, x            ;
    sta $0235, y                   ;
    iny                            ;
    inx                            ;
    lda right2Frame6, x            ;
    sta $0235, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;

; Load the animation of player 1 walking to the left
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
    lda left1Frame1, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda left1Frame1, x              ;
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
    lda left1Frame2, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda left1Frame2, x              ;
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
    lda left1Frame3, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda left1Frame3, x              ;
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
    lda left1Frame4, x              ;
    sta $0225, y                    ; 
    iny                             ; 
    inx                             ; 
    lda left1Frame4, x              ;
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

; Load the animation of player 1 walking to the right
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
    lda right1Frame1, x             ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda right1Frame1, x             ;
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
    lda right1Frame2, x             ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda right1Frame2, x             ;
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
    lda right1Frame3, x             ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda right1Frame3, x             ;
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
    lda right1Frame4, x             ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda right1Frame4, x             ;
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

; Load the animation of player 2 walking to the left
;***********************************;
loadLeftAnimation2:                 ;
  lda $0008                         ;
  cmp #$01                          ;
  bne continueLeftAnimation2        ;
                                    ;
  jmp skipLeftAnimation2            ;
                                    ;
continueLeftAnimation2:             ;
  lda #$01                          ;
  sta DIRECTION2                    ;
                                    ;
  lda INDEX_FRAME2                  ;
  ldx $00                           ;
                                    ;
  cmp #$00                          ;
  beq load2LeftFrame1               ;
  cmp #$01                          ;
  beq load2LeftFrame2               ;
  cmp #$02                          ;
  beq load2LeftFrame3               ;
  cmp #$03                          ;
  beq load2LeftFrame4               ;
endLoadLeftAnimation2:              ;
  clc                               ;
  lda COUNTER_FRAME2                ;
  adc #$01                          ;
  sta COUNTER_FRAME2                ;
  cmp NMI_FRECUENCY_FRAME2          ;
  bne @else1                        ;
    lda #$00                        ;
    sta COUNTER_FRAME2              ;
                                    ;
    lda INDEX_FRAME2                ;
    adc #$01                        ;
    cmp #$04                        ;
    bne @else2                      ;
      lda #$00                      ;
    @else2:                         ;
      sta INDEX_FRAME2              ;
  @else1:                           ;
    rts                             ;
                                    ;
                                    ;
load2LeftFrame1:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda left2Frame1, x              ;
    sta $0235, y                    ;
    iny                             ;
    inx                             ;
    lda left2Frame1, x              ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation2         ;
                                    ;
load2LeftFrame2:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda left2Frame2, x              ;
    sta $0235, y                    ;
    iny                             ;
    inx                             ;
    lda left2Frame2, x              ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation2         ;
                                    ;
load2LeftFrame3:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda left2Frame3, x              ;
    sta $0235, y                    ;
    iny                             ;
    inx                             ;
    lda left2Frame3, x              ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation2         ;
                                    ;
load2LeftFrame4:                    ;
  ldx #$00                          ;
  ldy #$00                          ; 
  @loop:                            ;  
    lda left2Frame4, x              ;
    sta $0235, y                    ; 
    iny                             ; 
    inx                             ; 
    lda left2Frame4, x              ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ; 
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation2         ;
                                    ;
skipLeftAnimation2:                 ;
  rts                               ;
;***********************************;

; Load the animation of player 2 walking to the right
;***********************************;
loadRightAnimation2:                ;
  lda $0008                         ;
  cmp #$01                          ;
  bne continueRightAnimation2       ;
                                    ;
  jmp skipRightAnimation2           ;
                                    ;
continueRightAnimation2:            ;
  lda #$00                          ; 
  sta DIRECTION2                    ;
                                    ;
  lda INDEX_FRAME2                  ;
  ldx $00                           ;
                                    ;
  cmp #$00                          ;
  beq load2RightFrame1              ;
  cmp #$01                          ;
  beq load2RightFrame2              ;
  cmp #$02                          ;
  beq load2RightFrame3              ;
  cmp #$03                          ;
  beq load2RightFrame4              ; 
endLoadRightAnimation2:             ;
  clc                               ;
  lda COUNTER_FRAME2                ;
  adc #$01                          ;
  sta COUNTER_FRAME2                ;
  cmp NMI_FRECUENCY_FRAME2          ;
  bne @else1                        ;
    lda #$00                        ;
    sta COUNTER_FRAME2              ;
                                    ;
    lda INDEX_FRAME2                ;
    adc #$01                        ;
    cmp #$04                        ;
    bne @else2                      ;
      lda #$00                      ; 
    @else2:                         ;
      sta INDEX_FRAME2              ; 
  @else1:                           ;
    rts                             ;
                                    ;
                                    ;
load2RightFrame1:                   ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda right2Frame1, x             ;
    sta $0235, y                    ;
    iny                             ;
    inx                             ;
    lda right2Frame1, x             ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ; 
  jmp endLoadRightAnimation2        ;
                                    ;
load2RightFrame2:                   ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ; 
    lda right2Frame2, x             ;
    sta $0235, y                    ;
    iny                             ;
    inx                             ;
    lda right2Frame2, x             ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ; 
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadRightAnimation2        ;
                                    ;
load2RightFrame3:                   ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda right2Frame3, x             ;
    sta $0235, y                    ;
    iny                             ;
    inx                             ;
    lda right2Frame3, x             ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadRightAnimation2        ;
                                    ;
load2RightFrame4:                   ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda right2Frame4, x             ;
    sta $0235, y                    ;
    iny                             ;
    inx                             ;
    lda right2Frame4, x             ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadRightAnimation2        ;
                                    ;
skipRightAnimation2:                ;
  rts                               ;
;***********************************;

; Load the animation of player 1 jumping
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
    lda left1Frame3, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda left1Frame3, x              ;
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
    lda right1Frame3, x             ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda right1Frame3, x             ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ; 
    bne @loop                       ;
  rts                               ;
;-----------------------------------;

; Load the animation of player 2 jumping
;***********************************;
loadJumpFrame2:                     ;
lda DIRECTION2                      ;
  and #%00000001                    ; 
  bne @else                         ;
    jsr loadJumpRight2              ; If islooking to the left
    jmp endLoadJumpFrame2           ;
  @else:                            ;
    jsr loadJumpLeft2               ; If islooking to the right
endLoadJumpFrame2:                  ;
  rts                               ;
                                    ;
loadJumpLeft2:                      ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda left2Frame3, x              ;
    sta $0235, y                    ;
    iny                             ;
    inx                             ;
    lda left2Frame3, x              ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  rts                               ;
                                    ; 
loadJumpRight2:                     ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda right2Frame3, x             ;
    sta $0235, y                    ;
    iny                             ;
    inx                             ;
    lda right2Frame3, x             ;
    sta $0235, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ; 
    bne @loop                       ;
  rts                               ;
;***********************************;

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
    sta COUNTER_JUMP1                    ;
  endCheckAButtonPressed:                ;
    rts                                  ;
;----------------------------------------;

;-----------check B button---------------;
  checkBButtonPressed:                   ;
    lda INPUT1                           ;
    and #%01000000                       ;
    beq @else                            ;
    jsr loadAttackFrame                  ;
    lda B_PRESSED1                       ;
    cmp #$01                             ;
    beq endCheckBButtonPressed           ;
    lda #$01                             ;
    sta B_PRESSED1                       ;
    lda COUNTER_HEALTH2                  ;
    cmp #$03                             ;
    beq endCheckBButtonPressed           ;
    lda IS_COLLIDING1                    ;
    cmp #$01                             ;
    bne endCheckBButtonPressed           ;
    inc COUNTER_HEALTH2                  ;
    jmp endCheckBButtonPressed           ;
  @else:                                 ;
    lda #$00                             ;
    sta B_PRESSED1                       ;
  endCheckBButtonPressed:                ;
    rts                                  ;
;----------------------------------------;

;-----------check select button----------;
  checkSelectButtonPressed:              ;
    lda INPUT1                           ;
    and #%00100000                       ;
    beq @else                            ; 
    ; lda SELECT_PRESSED1                ;
    ; cmp #$01                           ;
    ; beq endCheckSelectButtonPressed    ;
    ; inc COUNTER_HEALTH1                ;
    ; lda #$01                           ;
    ; sta SELECT_PRESSED1                ;
    ; jmp endCheckSelectButtonPressed    ;
  @else:                                 ;
    ; lda #$00                           ;
    ; sta SELECT_PRESSED1                ;
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

;******read inputs of control 2******;
  readController2:                   ;
    lda #1                           ;
    sta INPUT2                       ;
                                     ;
    sta $4016                        ;
    lda #0                           ;
    sta $4016                        ;
  readLoop2:                         ;
    lda $4017                        ;
    lsr a                            ;
    rol INPUT2                       ;
    bcc readLoop2                    ;
    rts                              ;
;************************************;

;************check left button***********;
  checkLeftButtonPressed2:               ;
    lda INPUT2                           ;
    and #%00000010                       ;
    beq endCheckLeftButtonPressed2       ;
    jsr loadLeftAnimation2               ;
    jsr moveToLeftPlayer2                ;
  endCheckLeftButtonPressed2:            ;
    rts                                  ;
;****************************************;

;***********check right button***********;
  checkRightButtonPressed2:              ;
    lda INPUT2                           ;
    and #%00000001                       ;
    beq endCheckRightButtonPressed2      ;
    jsr loadRightAnimation2              ;
    jsr moveToRightPlayer2               ;
  endCheckRightButtonPressed2:           ;
    rts                                  ;
;****************************************;

;*************check A button*************;
  checkAButtonPressed2:                  ;
    lda INPUT2                           ;
    and #%10000000                       ;
    beq AButtonNotPressed2               ;
    jsr jumpingEffect2                   ;
    jmp endCheckAButtonPressed2          ;
  AButtonNotPressed2:                    ;
    lda #$19                             ;
    sta COUNTER_JUMP2                    ;
  endCheckAButtonPressed2:               ;
    rts                                  ;
;****************************************;

;*************check B button*************;
  checkBButtonPressed2:                  ;
    lda INPUT2                           ;
    and #%01000000                       ;
    beq @else                            ;
    jsr loadAttackFrame2                 ;
    lda B_PRESSED2                       ;
    cmp #$01                             ;
    beq endCheckBButtonPressed2          ;
    lda #$01                             ;
    sta B_PRESSED2                       ;
    lda COUNTER_HEALTH1                  ;
    cmp #$03                             ;
    beq endCheckBButtonPressed2          ;
    lda IS_COLLIDING1                    ;
    cmp #$01                             ;
    bne endCheckBButtonPressed2          ;
    inc COUNTER_HEALTH1                  ;
    jmp endCheckBButtonPressed2          ;
  @else:                                 ;
    lda #$00                             ;
    sta B_PRESSED2                       ;
  endCheckBButtonPressed2:               ;
    rts                                  ;
;****************************************;

;***********check select button**********;
  checkSelectButtonPressed2:             ;
    lda INPUT2                           ;
    and #%00100000                       ;
    beq @else                            ; 
    ; lda SELECT_PRESSED2                ;
    ; cmp #$01                           ;
    ; beq endCheckSelectButtonPressed2   ;
    ; inc COUNTER_HEALTH2                ;
    ; lda #$01                           ;
    ; sta SELECT_PRESSED2                ;
    ; jmp endCheckSelectButtonPressed2   ;
  @else:                                 ;
    ; lda #$00                           ;
    ; sta SELECT_PRESSED2                ;
  endCheckSelectButtonPressed2:          ;
    rts                                  ;
;****************************************;

;**********check if not moving***********;
  checkNotMovingPressed2:                ;
    lda INPUT2                           ;
    and #%11000011                       ;
    bne @else                            ;
      jsr loadStillFrame2                ;  <--- if not moving
    jmp endcheckNotMovingPressed2        ;
  @else:                                 ;
                                         ;  <--- if moving
  endcheckNotMovingPressed2:             ;
    rts                                  ;
;****************************************;

; Moves player 1 to the left the 4 tiles saved in RAM by 1 pixel.
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

; Moves player 1 to the right the 4 tiles saved in RAM by 2 pixels.
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

; Moves player 2 to the left the 4 tiles saved in RAM by 1 pixel.
;*********************;
moveToLeftPlayer2:    ;
  clc                 ;
  lda $0237           ;
  cmp #$0010          ;
  bcc endMoveToLeft2  ;
                      ;
  clc                 ;
  lda $0237           ;
  sbc #$0001          ;
  sta $0237           ;
                      ;
  clc                 ; 
  lda $023B           ;
  sbc #$0001          ;
  sta $023B           ;
                      ;
  clc                 ;
  lda $023F           ;
  sbc #$0001          ;
  sta $023F           ;
                      ;
  clc                 ;
  lda $0243           ;
  sbc #$0001          ;
  sta $0243           ;
                      ;
  lda #$00            ;
  sta $0008           ;
  rts                 ;
                      ;
endMoveToLeft2:       ;
  lda #$01            ;
  sta $0008           ;
  rts                 ;
;*********************;

; Moves player 2 to the right the 4 tiles saved in RAM by 2 pixels.
;*********************;
moveToRightPlayer2:   ;
  clc                 ;
  lda $0237           ;
  cmp #$00F6          ;
  bcs endMoveToRight2 ;
                      ;
  clc                 ;
  lda $0237           ;
  adc #$0002          ;
  sta $0237           ;
                      ;
  clc                 ;
  lda $023B           ;
  adc #$0002          ;
  sta $023B           ;
                      ;
  clc                 ;
  lda $023F           ;
  adc #$0002          ;
  sta $023F           ;
                      ;
  clc                 ;
  lda $0243           ;
  adc #$0002          ;
  sta $0243           ;
                      ;
  lda #$00            ;
  sta $0008           ;
  rts                 ;
                      ;
endMoveToRight2:      ;  
  lda #$01            ;
  sta $0008           ;       
  rts                 ;
;*********************;

;-------------------------------;
gravityEffect:                  ;
  ldx COUNTER_JUMP1             ;
  cpx #$15                      ;
  bmi endGravityEffect          ;
  jsr checkIfTouchingFloor      ;
  jsr checkIfTouchingPlatform   ;
                                ;
  clc                           ;
  lda IS_TOUCHING_FLOOR1        ;
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
  ldx COUNTER_JUMP1             ;
  inx                           ;
  stx COUNTER_JUMP1             ;
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
  sta IS_TOUCHING_FLOOR1        ;
                                ;
  clc                           ;
  lda $0224                     ;
  cmp #$B5                      ;
  bpl endCheckIfTouchingFloor   ;   
  cmp #$AF                      ;
  bmi endCheckIfTouchingFloor   ;
  lda #$0001                    ;
  sta IS_TOUCHING_FLOOR1        ;
  lda #$00                      ;
  sta COUNTER_JUMP1             ;
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
  sta IS_TOUCHING_FLOOR1            ;
  lda #$00                          ;
  sta COUNTER_JUMP1                 ;
                                    ;
endCheckIfTouchingPlatform:         ;
  rts                               ;
;-----------------------------------;

;*******************************;
gravityEffect2:                 ;
  ldx COUNTER_JUMP2             ;
  cpx #$15                      ;
  bmi endGravityEffect2         ;
  jsr checkIfTouchingFloor2     ;
  jsr checkIfTouchingPlatform2  ;
                                ;
  clc                           ;
  lda IS_TOUCHING_FLOOR2        ;
  cmp #$0001                    ;
  beq endGravityEffect2         ;
                                ;
  clc                           ;
  lda $0234                     ;
  adc #GRAVITY2                 ;
  sta $0234                     ;
                                ;
  clc                           ;
  lda $0238                     ;
  adc #GRAVITY2                 ;
  sta $0238                     ;
                                ;
  clc                           ;
  lda $023C                     ;
  adc #GRAVITY2                 ;
  sta $023C                     ;
                                ;
  clc                           ;
  lda $0240                     ;
  adc #GRAVITY2                 ;
  sta $0240                     ;
                                ;
endGravityEffect2:              ;
    rts                         ; 
;*******************************;

;*******************************;
jumpingEffect2:                 ;
  ldx COUNTER_JUMP2             ;
  inx                           ;
  stx COUNTER_JUMP2             ;
  cpx #$15                      ;
  bpl endJumpingEffect2         ;
  jsr loadJumpFrame2            ;
                                ;
  clc                           ;
  lda $0234                     ;
  sbc #GRAVITY2                 ;
  sta $0234                     ;
                                ;
  clc                           ;
  lda $0238                     ;
  sbc #GRAVITY2                 ;
  sta $0238                     ;
                                ;
  clc                           ;
  lda $023C                     ;
  sbc #GRAVITY2                 ;
  sta $023C                     ;
                                ;
  clc                           ;
  lda $0240                     ;
  sbc #GRAVITY2                 ;
  sta $0240                     ;
                                ;
endJumpingEffect2:              ;
    rts                         ; 
;*******************************;

;*******************************;
checkIfTouchingFloor2:          ;
  clc                           ;
  lda #$0000                    ;
  sta IS_TOUCHING_FLOOR2        ;
                                ;
  clc                           ;
  lda $0234                     ;
  cmp #$B5                      ;
  bpl endCheckIfTouchingFloor2  ;   
  cmp #$AF                      ;
  bmi endCheckIfTouchingFloor2  ;
  lda #$0001                    ;
  sta IS_TOUCHING_FLOOR2        ;
  lda #$00                      ;
  sta COUNTER_JUMP2             ;
                                ;
endCheckIfTouchingFloor2:       ;
  rts                           ;
;*******************************;

;***********************************;
checkIfTouchingPlatform2:           ;
  lda $0237                         ;
  cmp #$4D                          ;    <--- left
  bmi endCheckIfTouchingPlatform2   ;
  cmp #$C0                          ;    <--- right
  bpl endCheckIfTouchingPlatform2   ;
  lda $0234                         ;
  cmp #$7F                          ;    <--- bottom
  bpl endCheckIfTouchingPlatform2   ;
  cmp #$6F                          ;    <--- top
  bmi endCheckIfTouchingPlatform2   ;
  lda #$0001                        ;
  sta IS_TOUCHING_FLOOR2            ;
  lda #$00                          ;
  sta COUNTER_JUMP2                 ;
                                    ;
endCheckIfTouchingPlatform2:        ;
  rts                               ;
;***********************************;

; Collision between two players (Axis-aligned Bounding Box)
;---------------------------------;
checkIfColliding:                 ;
clc                               ;
lda #$0000                        ;
sta IS_COLLIDING1                 ;
                                  ;
lda $0237                         ; X position of player 2
clc                               ;
adc #$10                          ; + width of player two
cmp $0227                         ; X position of player 1
bmi endCheckIfColliding           ;
lda $0227                         ;
clc                               ;
adc #$10                          ;
cmp $0237                         ;
bmi endCheckIfColliding           ;
lda $0234                         ;
clc                               ;
adc #$10                          ;
cmp $0224                         ;
bmi endCheckIfColliding           ;
lda $0224                         ;
clc                               ;
adc #$10                          ;
cmp $0234                         ;
bmi endCheckIfColliding           ;
lda #$0001                        ;
sta IS_COLLIDING1                 ;
                                  ;
endCheckIfColliding:              ;
rts                               ;
;---------------------------------;

;--------------------------- Binary/Hexadecimal Data ---------------------------

;----------------------------- Animation -----------------------------

;--------------------- Player 1 ---------------------

initializationSprites1:
  .byte INITIAL1_Y, $01, $04, INITIAL1_X + $08
  .byte INITIAL1_Y, $02, $04, INITIAL1_X
  .byte INITIAL1_Y + $08, $11, $04, INITIAL1_X + $08
  .byte INITIAL1_Y + $08, $12, $04, INITIAL1_X 

left1Frame1:
  .byte $07, $40 
  .byte $08, $40
  .byte $17, $40
  .byte $18, $40

left1Frame2:
  .byte $04, $40
  .byte $05, $40 
  .byte $14, $40
  .byte $15, $40

left1Frame3:
  .byte $0A, $40
  .byte $0B, $40
  .byte $1A, $40
  .byte $1B, $40

left1Frame4:
  .byte $04, $40
  .byte $05, $40
  .byte $14, $40
  .byte $15, $40

left1Frame5:               ; jumping attack
  .byte $21, $40
  .byte $22, $40
  .byte $31, $40
  .byte $32, $40

left1Frame6:               ; walking attack
  .byte $24, $40
  .byte $25, $40
  .byte $34, $40
  .byte $35, $40

still1Left:
  .byte $01, $40 
  .byte $02, $40
  .byte $11, $40 
  .byte $12, $40 

dead1Left:
  .byte $00, $00
  .byte $00, $00
  .byte $1D, $40 
  .byte $1E, $40
  

still1Right:
  .byte $02, $04 
  .byte $01, $04  
  .byte $12, $04 
  .byte $11, $04

dead1Right:
  .byte $00, $00
  .byte $00, $00
  .byte $1E, $04 
  .byte $1D, $04  

right1Frame1:
  .byte $08, $04 
  .byte $07, $04
  .byte $18, $04
  .byte $17, $04

right1Frame2:
  .byte $05, $04
  .byte $04, $04
  .byte $15, $04
  .byte $14, $04

right1Frame3:
  .byte $0B, $04 
  .byte $0A, $04
  .byte $1B, $04
  .byte $1A, $04

right1Frame4:
  .byte $05, $04 
  .byte $04, $04
  .byte $15, $04
  .byte $14, $04

right1Frame5:               ; jumping attack
  .byte $22, $04
  .byte $21, $04
  .byte $32, $04
  .byte $31, $04

right1Frame6:               ; walking attack
  .byte $25, $04
  .byte $24, $04
  .byte $35, $04
  .byte $34, $04

Life:
  heart1Sprite:
    .byte $00, $00, $00, $00
    .byte $00, $00, $00, $00
    .byte $10, $3A, $40, $AA
    .byte $00, $00, $00, $00

  heart1Sprite2:
    .byte $00, $00, $00, $00
    .byte $00, $00, $00, $00
    .byte $10, $3A, $40, $C0
    .byte $00, $00, $00, $00

  heart1Sprite3:
    .byte $00, $00, $00, $00
    .byte $00, $00, $00, $00
    .byte $10, $3A, $40, $D6
    .byte $00, $00, $00, $00

; ******************** Player 2 ********************

initializationSprites2:
  .byte INITIAL2_Y, $01, $42, INITIAL2_X + $08
  .byte INITIAL2_Y, $02, $42, INITIAL2_X
  .byte INITIAL2_Y + $08, $11, $42, INITIAL2_X + $08
  .byte INITIAL2_Y + $08, $12, $42, INITIAL2_X 

  left2Frame1:
  .byte $07, $42 
  .byte $08, $42
  .byte $17, $42
  .byte $18, $42

left2Frame2:
  .byte $04, $42
  .byte $05, $42 
  .byte $14, $42
  .byte $15, $42

left2Frame3:
  .byte $0A, $42
  .byte $0B, $42
  .byte $1A, $42
  .byte $1B, $42

left2Frame4:
  .byte $04, $42
  .byte $05, $42
  .byte $14, $42
  .byte $15, $42

left2Frame5:               ; jumping attack
  .byte $21, $42
  .byte $22, $42
  .byte $31, $42
  .byte $32, $42

left2Frame6:               ; walking attack
  .byte $24, $42
  .byte $25, $42
  .byte $34, $42
  .byte $35, $42

still2Left:
  .byte $01, $42 
  .byte $02, $42
  .byte $11, $42 
  .byte $12, $42 

dead2Left:
  .byte $00, $00
  .byte $00, $00
  .byte $1D, $42 
  .byte $1E, $42
  

still2Right:
  .byte $02, $06 
  .byte $01, $06  
  .byte $12, $06 
  .byte $11, $06

dead2Right:
  .byte $00, $00
  .byte $00, $00
  .byte $1E, $06 
  .byte $1D, $06  

right2Frame1:
  .byte $08, $06
  .byte $07, $06
  .byte $18, $06
  .byte $17, $06

right2Frame2:
  .byte $05, $06
  .byte $04, $06
  .byte $15, $06
  .byte $14, $06

right2Frame3:
  .byte $0B, $06 
  .byte $0A, $06
  .byte $1B, $06
  .byte $1A, $06

right2Frame4:
  .byte $05, $06 
  .byte $04, $06
  .byte $15, $06
  .byte $14, $06

right2Frame5:               ; jumping attack
  .byte $22, $06
  .byte $21, $06
  .byte $32, $06
  .byte $31, $06

right2Frame6:               ; walking attack
  .byte $25, $06
  .byte $24, $06
  .byte $35, $06
  .byte $34, $06

Life2:
  heart2Sprite:
    .byte $00, $00, $00, $00
    .byte $00, $00, $00, $00
    .byte $1A, $3A, $42, $AA
    .byte $00, $00, $00, $00

  heart2Sprite2:
    .byte $00, $00, $00, $00
    .byte $00, $00, $00, $00
    .byte $1A, $3A, $42, $C0
    .byte $00, $00, $00, $00

  heart2Sprite3:
    .byte $00, $00, $00, $00
    .byte $00, $00, $00, $00
    .byte $1A, $3A, $42, $D6
    .byte $00, $00, $00, $00

;----------------------------------------------------

palettes:                            
        ;00  01   10   11                   
  .byte $0f, $37, $17, $27; 00              
  .byte $0f, $30, $17, $27; 01   background     
  .byte $0f, $3c, $38, $19; 02              
  .byte $0f, $2d, $0c, $09; 03              
                                            
  .byte $0f, $37, $07, $0c; 04               
  .byte $0f, $26, $07, $0c; 05   foreground         
  .byte $0f, $37, $07, $16; 06              
  .byte $0f, $26, $07, $16; 07              

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