; iNES header for nrom.cfg: 1 x 16 KiB PRG, 1 x 8 KiB CHR, mapper 0,
; vertical mirroring.
.segment "HEADER"
    .byte "NES", $1A
    .byte 1, 1
    .byte $01, $00
    .res 8, 0
