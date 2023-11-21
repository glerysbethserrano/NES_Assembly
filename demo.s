
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
  sei		            ; disable IRQs
  cld		            ; disable decimal mode
  ldx #$40
  stx $4017	            ; disable APU frame IRQ
  ldx #$ff       	    ; Set up stack
  txs		
  inx		            ; now X = 0
  stx PPUCTRL	        ; disable NMI
  stx PPUMASK 	        ; disable rendering
  stx $4010 	        ; disable DMC IRQs

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
  lda #%00011010	    ; Enable Sprites
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

 ;-----------;
  ldx #$0000  ;
  stx $2003   ;This is where the 64 sprites will be stored.
  ;-----------;
  
  jsr loadRightAnimation

  ;-----------;
  lda #$02    ;
  sta $4014   ; This makes a copy of the memory space at address $0200 to $02FF in the PPU OAM.
  ;-----------;

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

;----Load Player sprites----;
loadPlayerSprites:	        ;
  ldx #$00                  ;
  @loop1:                   ;
    lda playerSprites, x    ;
    sta $0224, x            ;
    inx                     ;
    cpx #$90                ;
    bne @loop1              ;
    rts                     ;
;---------------------------;

;------Load Attribute Table-----;
LoadAttr:                       ;
    lda PPUSTATUS               ;
    LDA #$23                    ;
    STA $2006                   ;
    LDA #$c0                    ;
    STA $2006                   ;
    LDX #$00                    ;
LoadAttrLoop:                   ;
    LDA attributeTable0, x      ;
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
  stx INDEX_FRAME1                     ; INDEX_FRAME = 0
  stx COUNTER_FRAME1                   ; COUNTER_FRAME1 = 0
  stx FIX_MIRRORING                    ; FIX_MIRRORING = 0
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


; Load character sprite while not moving
;----------------------------------;
loadStillFrame:                    ;
  lda DIRECTION1                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadStillRight             ;If islooking to the left
    jmp endLoadStillFrame          ;
  @else:                           ;
    jsr loadStillLeft              ;If islooking to the right
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


; Load the animation of walking to the left
;-----------------------------------;
loadLeftAnimation:                  ;
  ; lda PLAYER_STATUS1              ;
  ; ora %00000001                   ;
  ; sta PLAYER_STATUS1              ;
                                    ;
  lda DIRECTION1                    ; 
  cmp #$00                          ;
  bne @continue                     ;
  lda #$01                          ;
  sta FIX_MIRRORING                 ;
  @continue:                        ;
                                    ;
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
;-----------------------------------;

;Load the animation of walking to the right
;-----------------------------------;
loadRightAnimation:                 ;
  ; lda PLAYER_STATUS1              ;
  ; and %11111110                   ;
  ; sta PLAYER_STATUS1              ;
                                    ;
  lda DIRECTION1                    ; 
  cmp #$01                          ;
  bne @continue                     ;
  lda #$01                          ;
  sta FIX_MIRRORING                 ;
  @continue:                        ;
                                    ;
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
;-----------------------------------;


; Fixes a problem that occurs when reversing the tiles
; vertically when making the character change direction.
; This problem arises since we must remember that the
; character is made of 4 tiles and what is inverted
; are those 4 tiles, not the complete character.
; So you have to reflect the tiles but also exchange positions.
;-----------------------------------;
checkAndFixMirroring:               ;
  lda FIX_MIRRORING                 ;
  cmp #$01                          ;
  bne endCheckAndFixMirroring       ;
    ldx $0227                       ;
    ldy $022B                       ;
                                    ;
    sty $0227                       ;
    sty $022F                       ;
    stx $022B                       ;
    stx $0233                       ;
                                    ;
    lda #$00                        ;
    sta FIX_MIRRORING               ;
endCheckAndFixMirroring:            ;
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

stillLeft:
  .byte $01, $40 
  .byte $02, $40
  .byte $11, $40 
  .byte $12, $40 

stillRight:
  .byte $02, $04 
  .byte $01, $04  
  .byte $12, $04 
  .byte $11, $04

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

;*******************************************************

; First byte
; Y position

; Second byte
; NEXXT sprite tile index

; Third byte
; 7654 3210
; |||| ||||
; |||| ||++- Palette (4 to 7) of sprite
; |||+ ++--- Unimplemented (read 0)
; ||+------ Priority (0: in front of background; 1: behind background)
; |+------- Flip sprite horizontally
; +-------- Flip sprite vertically

; Fourth byte
; X position

playerSprites:

  moving1Right:
    .byte $AD, $04, $04, $4C  ; Y=$AD(173), Sprite=04, Attribute=$04(00000100), X=$4C(76)
    .byte $AD, $05, $04, $54  ; Y=$AD(173), Sprite=05, Attribute=$04(00000100), X=$54(84)
    .byte $B5, $14, $04, $4C  ; Y=$B5(181), Sprite=14, Attribute=$04(00000100), X=$4C(76)
    .byte $B5, $15, $04, $54  ; Y=$B5(181), Sprite=15, Attribute=$04(00000100), X=$54(84)

  moving2Right:
    .byte $AD, $07, $04, $5C  ; Y=$AD(173), Sprite=$07, Attribute=$04(00000100), X=$5C(92)
    .byte $AD, $08, $04, $64  ; Y=$AD(173), Sprite=$08, Attribute=$04(00000100), X=$64(100)
    .byte $B5, $17, $04, $5C  ; Y=$B5(181), Sprite=$17, Attribute=$04(00000100), X=$5C(92)
    .byte $B5, $18, $04, $64  ; Y=$B5(181), Sprite=$18, Attribute=$04(00000100), X=$64(100)
  
  moving3Right:
    .byte $AD, $0A, $04, $6C  ; Y=$AD(173), Sprite=$0A, Attribute=$04(00000100), X=$6C(108)
    .byte $AD, $0B, $04, $74  ; Y=$AD(173), Sprite=$0B, Attribute=$04(00000100), X=$74(116)
    .byte $B5, $1A, $04, $6C  ; Y=$B5(181), Sprite=$1A, Attribute=$04(00000100), X=$6C(108)
    .byte $B5, $1B, $04, $74  ; Y=$B5(181), Sprite=$1B, Attribute=$04(00000100), X=$74(116)

  deadRight:
    .byte $40, $1D, $04, $60  ; Y=$40(64), Sprite=$1D, Attribute=$04(00000100), X=$60(58)
    .byte $40, $1E, $04, $68  ; Y=$40(64), Sprite=$1E, Attribute=$04(00000100), X=$68(58)

  moving1Left:
    .byte $6D, $04, $40, $67  ; Y=$6D(109), Sprite=$04, Attribute=$40(01000000), X=$67(103)
    .byte $6D, $05, $40, $5F  ; Y=$6D(109), Sprite=$05, Attribute=$40(01000000), X=$5F(95)
    .byte $75, $14, $40, $67  ; Y=$B5(117), Sprite=$14, Attribute=$40(01000000), X=$8C(103)
    .byte $75, $15, $40, $5F  ; Y=$B5(117), Sprite=$15, Attribute=$40(01000000), X=$67(95)

  moving2Left:
    .byte $6D, $07, $40, $77  ; Y=$6D(109), Sprite=$07, Attribute=$40(01000000), X=$77(119)
    .byte $6D, $08, $40, $6F  ; Y=$6D(109), Sprite=$08, Attribute=$40(01000000), X=$6F(111)
    .byte $75, $17, $40, $77  ; Y=$B5(117), Sprite=$17, Attribute=$40(01000000), X=$77(119)
    .byte $75, $18, $40, $6F  ; Y=$B5(117), Sprite=$18, Attribute=$40(01000000), X=$6F(111)
  
  moving3Left:
    .byte $6D, $0A, $40, $87  ; Y=$6D(109), Sprite=$0A, Attribute=$40(01000000), X=$87(135)
    .byte $6D, $0B, $40, $7F  ; Y=$6D(109), Sprite=$0B, Attribute=$40(01000000), X=$7F(127)
    .byte $75, $1A, $40, $87  ; Y=$B5(117), Sprite=$1A, Attribute=$40(01000000), X=$87(135)
    .byte $75, $1B, $40, $7F  ; Y=$B5(117), Sprite=$1B, Attribute=$40(01000000), X=$7F(127)

  deadLeft:
    .byte $40, $1E, $40, $80  ; Y=$40(64), Sprite=$1E, Attribute=$40(01000000), X=$80(58)
    .byte $40, $1D, $40, $88  ; Y=$40(64), Sprite=$1D, Attribute=$40(01000000), X=$88(58)

                                      
palettes:                            
        ;00  01   10   11                   
  .byte $0f, $37, $17, $27; 00              
  .byte $0f, $30, $17, $27; 01   background     
  .byte $0f, $3c, $38, $19; 02              
  .byte $0f, $2d, $0c, $09; 03              
                                            
  .byte $0f, $37, $17, $27; 04              
  .byte $0f, $30, $17, $27; 05   foreground         
  .byte $0f, $3c, $38, $19; 06              
  .byte $0f, $2d, $0c, $09; 07              


attributeTable0:
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
	.byte $02,$15,$02,$15,$15,$15,$15,$15,$15,$15,$02,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$16,$02,$16,$02,$16,$02,$02
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
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$02,$02,$02,$02,$02,$02,$02,$02
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