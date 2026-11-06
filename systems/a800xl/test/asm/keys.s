; The keyboard (M4 step 4.8, a layer 4 run and its spec golden): with the
; display off, the background is the last key's code (KBCODE) times two
; while a key is held (SKSTAT bit 2 low), black while none is. Layer 4
; holds A (code $3F: colour $7E) over the PS link (A800.Loader's key
; bytes); the spec golden holds it through POKEY's key input.
.include "a800.inc"
.include "xex.inc"

COLBK   = $D01A
KBCODE  = $D209
SKSTAT  = $D20F

start:  sei
        lda #0
        sta NMIEN
        sta DMACTL
loop:   lda SKSTAT
        and #4
        bne none
        lda KBCODE
        asl
        sta COLBK
        jmp loop
none:   lda #0
        sta COLBK
        jmp loop
