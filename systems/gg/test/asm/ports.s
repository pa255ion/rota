; The ports and their partial decode, the VDP's port protocol (M5 step
; 5.2; docs/systems/gamegear.md, "Machine summary"; MacDonald's VDP notes).
;   1  port 00h: C0h (Start up, export, NTSC)
;   2  port DCh: FFh with no button pressed
;   3  port C0h mirrors DCh
;   4  port DDh: FFh
;   5  ports 01h-05h: their reset values 7F FF 00 FF 00
;   6  VRAM written through the data port reads back after a read setup
;   7  ... (second byte)
;   8  the first control byte moves the address's low byte at once
;   9  a data write also loads the read buffer
;  10  ports 80h/81h mirror BEh/BFh
;  11  a status read resets the control port's first/second byte flag
;  12  port DDh bits 6-7 follow TH-A/TH-B driven low through port 3Fh
;      (an export unit; Master System software's region check)
;  13  ... and read high again with TH back as inputs

.INCLUDE "gg.inc"

; VRAM write / read address
.MACRO VWRITE
  ld a, <\1
  out ($BF), a
  ld a, (>\1) | $40
  out ($BF), a
.ENDM
.MACRO VREAD
  ld a, <\1
  out ($BF), a
  ld a, >\1
  out ($BF), a
.ENDM

.BANK 0 SLOT 0
.SECTION "main" FREE
main:
  in a, ($00)
  EXPECT $C0, 1
  in a, ($DC)
  EXPECT $FF, 2
  in a, ($C0)
  EXPECT $FF, 3
  in a, ($DD)
  EXPECT $FF, 4
  in a, ($01)
  EXPECT $7F, 5
  in a, ($02)
  EXPECT $FF, 5
  in a, ($03)
  EXPECT $00, 5
  in a, ($04)
  EXPECT $FF, 5
  in a, ($05)
  EXPECT $00, 5

  VWRITE $1234
  ld a, $A5
  out ($BE), a
  ld a, $5A
  out ($BE), a
  VREAD $1234
  in a, ($BE)
  EXPECT $A5, 6
  in a, ($BE)
  EXPECT $5A, 7

  VWRITE $1000
  ld a, $05               ; first byte only: the address becomes 1005h
  out ($BF), a
  ld a, $C3
  out ($BE), a            ; resets the first/second byte flag, writes 1005h
  VREAD $1005
  in a, ($BE)
  EXPECT $C3, 8

  VREAD $2000             ; buffer = VRAM[2000h], address 2001h
  ld a, $77
  out ($BE), a            ; VRAM[2001h] = 77h, and the buffer
  in a, ($BE)
  EXPECT $77, 9

  ld a, $00
  out ($81), a
  ld a, $70 | $40         ; write 3000h through the mirror
  out ($81), a
  ld a, $99
  out ($80), a
  VREAD $3000
  in a, ($BE)
  EXPECT $99, 10

  ld a, $FF
  out ($BF), a            ; a first byte ...
  in a, ($BF)             ; ... forgotten by the status read
  VREAD $1234
  in a, ($BE)
  EXPECT $A5, 11

  ld a, $55               ; TH-A and TH-B outputs, low
  out ($3F), a
  in a, ($DD)
  and $C0
  EXPECT $00, 12
  ld a, $FF               ; all inputs
  out ($3F), a
  in a, ($DD)
  and $C0
  EXPECT $C0, 13

  jp pass
.ENDS
