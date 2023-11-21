
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
  
  jsr loadPlayerSprites

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


;--------------------------- Binary/Hexadecimal Data ---------------------------

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
  stillRight:
    .byte $AD, $01, $04, $3C  ; Y=$AD(173), Sprite=01, Palette=01, X=$3C(60)
    .byte $AD, $02, $04, $44  ; Y=$AD(173), Sprite=02, Palette=01, X=$44(68)
    .byte $B5, $11, $04, $3C  ; Y=$B5(181), Sprite=11, Palette=01, X=$3C(60)
    .byte $B5, $12, $04, $44  ; Y=$B5(181), Sprite=12, Palette=01, X=$44(68)

  moving1Right:
    .byte $AD, $04, $04, $4C  ; Y=$AD(173), Sprite=04, Palette=01, X=$4C(76)
    .byte $AD, $05, $04, $54  ; Y=$AD(173), Sprite=05, Palette=01, X=$54(84)
    .byte $B5, $14, $04, $4C  ; Y=$B5(181), Sprite=14, Palette=01, X=$4C(76)
    .byte $B5, $15, $04, $54  ; Y=$B5(181), Sprite=15, Palette=01, X=$54(84)

  moving2Right:
    .byte $AD, $07, $04, $5C  ; Y=$AD(173), Sprite=07, Palette=01, X=$5C(92)
    .byte $AD, $08, $04, $64  ; Y=$AD(173), Sprite=08, Palette=01, X=$64(100)
    .byte $B5, $17, $04, $5C  ; Y=$B5(181), Sprite=17, Palette=01, X=$5C(92)
    .byte $B5, $18, $04, $64  ; Y=$B5(181), Sprite=18, Palette=01, X=$64(100)
  
  moving3Right:
    .byte $AD, $0A, $04, $6C  ; Y=$AD(173), Sprite=0A, Palette=01, X=$6C(108)
    .byte $AD, $0B, $04, $74  ; Y=$AD(173), Sprite=0B, Palette=01, X=$74(116)
    .byte $B5, $1A, $04, $6C  ; Y=$B5(181), Sprite=1A, Palette=01, X=$6C(108)
    .byte $B5, $1B, $04, $74  ; Y=$B5(181), Sprite=1B, Palette=01, X=$74(116)

  stillLeft:
    .byte $6D, $01, $40, $57  ; Y=$AD(173), Sprite=01, Palette=01, X=$57(87)
    .byte $6D, $02, $40, $4F  ; Y=$AD(173), Sprite=02, Palette=01, X=$4F(79)
    .byte $75, $11, $40, $57  ; Y=$B5(117), Sprite=11, Palette=01, X=57(87)
    .byte $75, $12, $40, $4F  ; Y=$B5(117), Sprite=12, Palette=01, X=$4F(79)

  moving1Left:
    .byte $6D, $04, $40, $67  ; Y=$AD(173), Sprite=04, Palette=01, X=$67(103)
    .byte $6D, $05, $40, $5F  ; Y=$AD(173), Sprite=05, Palette=01, X=$5F(95)
    .byte $75, $14, $40, $67  ; Y=$B5(181), Sprite=14, Palette=01, X=$8C(103)
    .byte $75, $15, $40, $5F  ; Y=$B5(181), Sprite=15, Palette=01, X=$67(95)

  moving2Left:
    .byte $6D, $07, $40, $77  ; Y=$AD(173), Sprite=07, Palette=01, X=$77(119)
    .byte $6D, $08, $40, $6F  ; Y=$AD(173), Sprite=08, Palette=01, X=$6F(111)
    .byte $75, $17, $40, $77  ; Y=$B5(181), Sprite=17, Palette=01, X=$77(119)
    .byte $75, $18, $40, $6F  ; Y=$B5(181), Sprite=18, Palette=01, X=$6F(111)
  
  moving3Left:
    .byte $6D, $0A, $40, $87  ; Y=$AD(173), Sprite=0A, Palette=01, X=$87(135)
    .byte $6D, $0B, $40, $7F  ; Y=$AD(173), Sprite=0B, Palette=01, X=$7F(127)
    .byte $75, $1A, $40, $87  ; Y=$B5(181), Sprite=1A, Palette=01, X=$87(135)
    .byte $75, $1B, $40, $7F  ; Y=$B5(181), Sprite=1B, Palette=01, X=$7F(127)

   deadLeft:
    .byte $40, $1D, $04, $60  ; Y=$6c(108), Sprite=04(J), Palette=01, X=%6c(58)
    .byte $40, $1E, $04, $68  ; Y=$6c(108), Sprite=04(J), Palette=01, X=%6c(58)

  deadRight:
    .byte $40, $1E, $40, $80  ; Y=$6c(108), Sprite=04(J), Palette=01, X=%6c(58)
    .byte $40, $1D, $40, $88  ; Y=$6c(108), Sprite=04(J), Palette=01, X=%6c(58)


                                            
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