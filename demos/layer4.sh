#!/bin/sh
# Layer 4 for the demos (PLAN.md, side track J): a demo ROM through the
# Arty's Verilator model, every HDMI frame against the spec's screens that
# rota-demos (layer 3) wrote. Run inside nix develop, from the repository:
#
#   demos/layer4.sh gb                  (build/demos/rota.gb)
#   demos/layer4.sh nes [split]         (build/demos/rota.nes, rota-split.nes)
#   demos/layer4.sh a800xl              (build/demos/rota.xex, baked in)
#   demos/layer4.sh gg                  (build/demos/rota.gg)
#   demos/layer4.sh sms                 (build/demos/rota.sms)
#   demos/layer4.sh snes                (build/demos/rota.sfc; M7)
#
# Needs the Arty model built (make sim SYSTEM=... once) and rota-demos run.
set -eu
frames=430
case "$1" in
  gb)
    make --no-print-directory resim SYSTEM=gb BOARD=arty-z7-20 ROM=build/demos/rota.gb FRAMES=$frames SIM_GOLDEN= >build/demos/layer4-gb.log 2>&1
    cabal run -v0 rota-brand:exe:rota-compare -- build/arty-z7-20/gb/frames build/demos/test/gb-spec-shades.bin 160 144 3 20 $((frames - 1))
    ;;
  nes)
    suffix=${2:+-$2}
    make --no-print-directory resim SYSTEM=nes BOARD=arty-z7-20 ROM=build/demos/rota$suffix.nes FRAMES=$frames SIM_GOLDEN= SIM_SYS_ARGS= >build/demos/layer4-nes$suffix.log 2>&1
    cabal run -v0 rota-brand:exe:rota-compare -- build/arty-z7-20/nes/frames build/demos/test/nes-spec-colours$suffix.bin 256 240 2 20 $((frames - 1))
    ;;
  a800xl)
    # AltirraOS boots and the loader starts the program first: the
    # comparison starts at frame 200 (after the first beat)
    make --no-print-directory resim SYSTEM=a800xl BOARD=arty-z7-20 XEX=build/demos/rota.xex FRAMES=$((frames + 40)) SIM_GOLDEN= >build/demos/layer4-a800xl.log 2>&1
    cabal run -v0 rota-brand:exe:rota-compare -- build/arty-z7-20/a800xl/frames build/demos/test/a800-spec-colours.bin 320 240 2 200 $((frames + 39))
    ;;
  gg)
    # the ROM's init and the lead-in first: frames 130 to 530 (the spec run's end)
    make --no-print-directory resim SYSTEM=gg BOARD=arty-z7-20 ROM=build/demos/rota.gg FRAMES=$((frames + 101)) SIM_GOLDEN= >build/demos/layer4-gg.log 2>&1
    cabal run -v0 rota-brand:exe:rota-compare -- build/arty-z7-20/gg/frames build/demos/test/gg-spec-colours.bin 160 144 3 130 $((frames + 100))
    ;;
  sms)
    # as the Game Gear: frames 130 to 530; the whole 256 x 192 picture
    # (the harness's frames: the 640 x 480 screen, the picture x2 at (64, 48))
    make --no-print-directory resim SYSTEM=sms BOARD=arty-z7-20 ROM=build/demos/rota.sms FRAMES=$((frames + 101)) SIM_GOLDEN= >build/demos/layer4-sms.log 2>&1
    cabal run -v0 rota-brand:exe:rota-compare -- build/arty-z7-20/sms/frames build/demos/test/sms-spec-colours.bin 256 192 2 130 $((frames + 100))
    ;;
  snes)
    # (M7) as the Master System: frames 130 to 530; the whole 256 x 224
    # picture (the harness's frames: the picture x2 at (64, 16), the line-
    # locked display's)
    make --no-print-directory resim SYSTEM=snes BOARD=arty-z7-20 ROM=build/demos/rota.sfc FRAMES=$((frames + 101)) SIM_GOLDEN= >build/demos/layer4-snes.log 2>&1
    cabal run -v0 rota-brand:exe:rota-compare -- build/arty-z7-20/snes/frames build/demos/test/snes-spec-colours.bin 256 224 2 130 $((frames + 100))
    ;;
  *)
    echo "usage: demos/layer4.sh gb | nes [split] | a800xl | gg | sms | snes" >&2
    exit 2
    ;;
esac
