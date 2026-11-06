; Atari 800XL equates for our regression programs (M4; addresses from
; "Mapping the Atari" and the Altirra Hardware Reference Manual).

; OS
SAVMSC  = $58           ; screen memory pointer
RUNAD   = $02E0
INITAD  = $02E2
ICCOM   = $0342         ; IOCB 0: command
ICBAL   = $0344         ;         buffer address
ICBLL   = $0348         ;         buffer length
CIOV    = $E456
PUTCHR  = $0B           ; CIO: put characters
EOL     = $9B

; Hardware
GTIA    = $D000
CONSOL  = $D01F
POKEY   = $D200
AUDF1   = $D200
AUDC1   = $D201
AUDCTL  = $D208
SKCTL   = $D20F
PORTA   = $D300
PORTB   = $D301
PACTL   = $D302
PBCTL   = $D303
DMACTL  = $D400
WSYNC   = $D40A
VCOUNT  = $D40B
NMIEN   = $D40E

; Print the zero-terminated-free string at addr (length len) on the
; screen editor (IOCB 0 is open after the OS's start-up).
.macro print addr, len
        ldx #0
        lda #PUTCHR
        sta ICCOM
        lda #<addr
        sta ICBAL
        lda #>addr
        sta ICBAL+1
        lda #<len
        sta ICBLL
        lda #>len
        sta ICBLL+1
        jsr CIOV
.endmacro

TRIG3   = $D013

; Report: print "Pass" or "FAIL" and the failed check's number (in
; `failed`, 0 = none), then stop. Needs `failed` (1 byte) and the
; messages below; `check n` jumps to fail with n if Z is clear.
.macro check n
.local ok
        beq ok
        lda #n
        jmp fail
ok:
.endmacro
