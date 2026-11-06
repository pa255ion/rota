; The boot splash's Super Nintendo demo (M7 step 7.13; docs/brand.md,
; "Rendition B", "The Super Nintendo as built"). Every picture and sound is
; data from the generator (demos/rota-brand: Rota.Snes, Rota.Snes.Rom,
; Rota.Sound.Snes) in rota_data.inc, and the SPC700's driver with its
; samples (rota-spc.bin, demos/snes/rota-spc.s with rota_spc.inc); this
; file only plays it, as the Sega demos do theirs. Assembled by `make
; demos-snes` (WLA-DX) into build/demos/rota.sfc: 256 KiB LoROM, no SRAM,
; no chip.
;
; Init (forced blank): mode 1, BG1 (4bpp, its characters at 0, its tilemap
; at 7000h, unscrolled: its row 0 shows on the first line) and
; the sprites (8 x 8, names at 0); the sprite patterns, picture frame 0's
; tiles, the tilemap, the sprites' colours; every sprite hidden; the SPC700
; driver uploaded through the boot ROM and started. Then each NMI (line
; 225) plays one record of each stream, VRAM's, CGRAM's and OAM's first
; (they need V-blank), the sound last (a write waits for the driver's
; echo): the tiles (a VRAM word address and a library graphic's address:
; 32 bytes by DMA), the colours (a CGRAM index and its colour; FFh: the
; display on), the sparks' and the lights' sprite entries (slot, X, Y,
; tile), the sound (DSP writes: register, value). A record is a count and
; that many operations; FEh and an address jump to the stream's idle loop;
; FFh ends the sound.

.MEMORYMAP
SLOTSIZE $8000
DEFAULTSLOT 0
SLOT 0 $8000
.ENDME
.ROMBANKSIZE $8000
.ROMBANKS 8
.LOROM

.SNESHEADER
ID "SNES"
NAME "ROTA BOOT SPLASH     "
SLOWROM
ROMSIZE $08
CARTRIDGETYPE $00
SRAMSIZE $00
COUNTRY $01
LICENSEECODE $00
VERSION $00
.ENDSNES

.SNESNATIVEVECTOR
COP Halt
BRK Halt
ABORT Halt
NMI Nmi
IRQ Halt
.ENDNATIVEVECTOR

.SNESEMUVECTOR
COP Halt
ABORT Halt
NMI Halt
RESET Reset
IRQBRK Halt
.ENDEMUVECTOR

; direct page: the streams' 24-bit pointers, the sound's sequence number
; and end, a count
.DEFINE PT $00          ; tiles
.DEFINE PC $03          ; colours
.DEFINE PS $06          ; sparks
.DEFINE PL $09          ; lights
.DEFINE PN $0C          ; sound
.DEFINE SEQ $0F
.DEFINE SNDDONE $10
.DEFINE CNT $11
.DEFINE PP $13          ; a stream pointer, for the sprite player

; A := the byte at [P], then P on (A 8 bits, the pointer's low word 16)
.MACRO NEXT ARGS P
        lda [P]
        rep #$20
        inc P
        sep #$20
.ENDM

.BANK 0 SLOT 0
.ORG $0000
.SECTION "code" SEMIFREE

Halt:
        stp

Reset:
        sei
        clc
        xce
        rep #$38
        ldx.w #$1FFF
        txs
        lda.w #$0000
        tcd
        sep #$20
        pha
        plb
        lda.b #$80
        sta.w $2100             ; forced blank
        stz.w $4200
        ; the PPU
        stz.w $2101             ; OBSEL: 8 x 8 and 16 x 16, names at 0
        lda.b #$01
        sta.w $2105             ; mode 1
        lda.b #$70
        sta.w $2107             ; BG1's tilemap at 7000h, 32 x 32
        stz.w $210B             ; BG1's characters at 0
        stz.w $210D
        stz.w $210D             ; BG1HOFS 0
        stz.w $210E
        stz.w $210E             ; BG1VOFS 0: row 0 on the first line
        lda.b #$11
        sta.w $212C             ; the main screen: BG1 and the sprites
        stz.w $212D
        stz.w $2130
        stz.w $2131
        stz.w $2133
        lda.b #$80
        sta.w $2115             ; VRAM: a word at a time
        ; the sprite patterns, tiles 0 on
        rep #$20
        stz.w $2116
        lda.w #Patterns
        ldx.w #PatternsEnd - Patterns
        sep #$20
        ldy.w #:Patterns
        jsr VramDma
        ; picture frame 0's tiles
        rep #$20
        lda.l InitTiles
        sta.b CNT
        lda.w #InitTiles + 2
        sta.b PP
        sep #$20
        lda.b #:InitTiles
        sta.b PP + 2
-       rep #$20
        lda.b CNT
        beq +
        dec.b CNT
        sep #$20
        jsr TileOp
        bra -
+       sep #$20
        ; the tilemap, then its last 4 rows blank
        rep #$20
        lda.w #$7000
        sta.w $2116
        lda.w #TileMap
        ldx.w #TileMapEnd - TileMap
        sep #$20
        ldy.w #:TileMap
        jsr VramDma
        ldx.w #128
-       stz.w $2118
        stz.w $2119
        dex
        bne -
        ; the sprites' colours, CGRAM 129 on
        lda.b #129
        sta.w $2121
        ldx.w #0
-       lda.l SpriteCgram,x
        sta.w $2122
        inx
        cpx.w #SpriteCgramEnd - SpriteCgram
        bne -
        ; every sprite hidden (Y E0h), the high table 0
        stz.w $2102
        stz.w $2103
        ldx.w #128
-       stz.w $2104
        lda.b #$E0
        sta.w $2104
        stz.w $2104
        stz.w $2104
        dex
        bne -
        ldx.w #32
-       stz.w $2104
        dex
        bne -
        ; the SPC700's driver
        jsr SpcUpload
        ; the streams
        rep #$20
        lda.w #SnesTiles
        sta.b PT
        lda.w #SnesCgram
        sta.b PC
        lda.w #SnesSparks
        sta.b PS
        lda.w #SnesLights
        sta.b PL
        lda.w #SnesSound
        sta.b PN
        sep #$20
        lda.b #:SnesTiles
        sta.b PT + 2
        lda.b #:SnesCgram
        sta.b PC + 2
        lda.b #:SnesSparks
        sta.b PS + 2
        lda.b #:SnesLights
        sta.b PL + 2
        lda.b #:SnesSound
        sta.b PN + 2
        stz.b SNDDONE
        lda.b #$80
        sta.w $4200             ; the NMI on
Forever:
        wai
        bra Forever

; VRAM from the current address: A (16 bits, from 8-bit mode's
; caller as set) the source's address, Y its bank, X the length; DMA 0
VramDma:
        rep #$20
        sta.w $4302
        stx.w $4305
        sep #$20
        tya
        sta.w $4304
        lda.b #$01
        sta.w $4300             ; two registers, written once each
        lda.b #$18
        sta.w $4301             ; $2118
        lda.b #$01
        sta.w $420B
        rts

; one tile operation from [PP]: a VRAM word address, a graphic's address
TileOp:
        rep #$20
        lda.b [PP]
        sta.w $2116
        inc.b PP
        inc.b PP
        lda.b [PP]
        sta.w $4302
        inc.b PP
        inc.b PP
        sep #$20
        lda.b [PP]
        sta.w $4304
        rep #$20
        inc.b PP
        lda.w #32
        sta.w $4305
        sep #$20
        lda.b #$01
        sta.w $4300
        lda.b #$18
        sta.w $4301
        lda.b #$01
        sta.w $420B
        rts

; the boot ROM's protocol: SpcBin (512 bytes) to 0200h, then started there;
; then its ready (5Ah on port 3)
SpcUpload:
-       lda.w $2140
        cmp.b #$AA
        bne -
-       lda.w $2141
        cmp.b #$BB
        bne -
        rep #$20
        lda.w #$0200
        sta.w $2142
        sep #$20
        lda.b #$01
        sta.w $2141
        lda.b #$CC
        sta.w $2140
-       cmp.w $2140
        bne -
        ldx.w #0
@byte:  lda.l SpcBin,x
        sta.w $2141
        txa
        sta.w $2140
-       cmp.w $2140
        bne -
        inx
        cpx.w #$200
        bne @byte
        rep #$20
        lda.w #$0200
        sta.w $2142
        sep #$20
        stz.w $2141
        txa
        inc a
        ora.b #$01
        sta.w $2140
        sta.b SEQ               ; the driver's last number: this one
-       lda.w $2143
        cmp.b #$5A
        bne -
        rts

Nmi:
        rep #$30
        pha
        phx
        phy
        phb
        phd
        lda.w #$0000
        tcd
        sep #$20
        pha
        plb
        lda.w $4210             ; the NMI acknowledged
        jsr PlayTiles
        jsr PlayCgram
        rep #$20
        lda.b PS
        sta.b PP
        sep #$20
        lda.b PS + 2
        sta.b PP + 2
        jsr PlaySprites
        rep #$20
        lda.b PP
        sta.b PS
        sep #$20
        lda.b PP + 2
        sta.b PS + 2
        rep #$20
        lda.b PL
        sta.b PP
        sep #$20
        lda.b PL + 2
        sta.b PP + 2
        jsr PlaySprites
        rep #$20
        lda.b PP
        sta.b PL
        sep #$20
        lda.b PP + 2
        sta.b PL + 2
        jsr PlaySound
        rep #$30
        pld
        plb
        ply
        plx
        pla
        rti

; a stream's count at [P], following an FEh jump to its loop
.MACRO COUNT ARGS P
-       NEXT P
        cmp.b #$FE
        bne +
        rep #$20
        lda.b [P]
        tax
        sep #$20
        ldy.w #2
        lda.b [P],y
        sta.b P + 2
        stx.b P
        bra -
+
.ENDM

PlayTiles:
        rep #$20
        lda.b PT
        sta.b PP
        sep #$20
        lda.b PT + 2
        sta.b PP + 2
        COUNT PP
        sta.b CNT
        cmp.b #0                ; (sta sets no flags: the count's own)
        beq @done
@op:    jsr TileOp
        dec.b CNT
        bne @op
@done:  rep #$20
        lda.b PP
        sta.b PT
        sep #$20
        lda.b PP + 2
        sta.b PT + 2
        rts

PlayCgram:
        COUNT PC
        sta.b CNT
        cmp.b #0                ; (sta sets no flags: the count's own)
        beq @done
@op:    NEXT PC
        cmp.b #$FF
        beq @display
        sta.w $2121
        NEXT PC
        sta.w $2122
        NEXT PC
        sta.w $2122
        bra @next
@display:
        NEXT PC
        sta.w $2100
        NEXT PC
@next:  dec.b CNT
        bne @op
@done:  rts

; a sprite stream's record at [PP]: slot, X, Y, tile (priority 2, palette 0)
PlaySprites:
        COUNT PP
        sta.b CNT
        cmp.b #0                ; (sta sets no flags: the count's own)
        beq @done
@op:    NEXT PP
        asl a
        sta.w $2102
        stz.w $2103
        NEXT PP
        sta.w $2104
        NEXT PP
        sta.w $2104
        NEXT PP
        sta.w $2104
        lda.b #$20
        sta.w $2104
        dec.b CNT
        bne @op
@done:  rts

PlaySound:
        lda.b SNDDONE
        bne @done
        NEXT PN
        cmp.b #$FF
        bne +
        lda.b #1
        sta.b SNDDONE
        bra @done
+       sta.b CNT
        cmp.b #0
        beq @done
@op:    NEXT PN
        sta.w $2141
        NEXT PN
        sta.w $2142
        inc.b SEQ
        lda.b SEQ
        sta.w $2140
-       cmp.w $2140
        bne -
        dec.b CNT
        bne @op
@done:  rts

.ENDS

.SECTION "spc" SUPERFREE
SpcBin: .INCBIN "rota-spc.bin"
.ENDS

.INCLUDE "rota_data.inc"
