#!/usr/bin/env bash
# Fetch the test ROMs into roms/ (git-ignored; ROMs are never committed,
# CLAUDE.md rule 7). Every file is checked against a pinned SHA-256, so a
# changed upstream file fails loudly instead of silently changing tests.
#
#   tools/fetch-roms.sh          fetch whatever is missing
#
# Sources:
#   roms/chip8/timendus/  Timendus chip8-test-suite v4.2 (GPL-3.0),
#                         https://github.com/Timendus/chip8-test-suite
#                         Hashes recorded from the v4.2 release assets on
#                         2026-10-02.
#   roms/chip8/games/     games for the milestone 1 "playable" check:
#                         space-invaders.ch8, David Winter's Space Invaders
#                         (1990; freeware, not explicitly public domain),
#                         from the Revival Studios program pack as mirrored
#                         by github.com/kripod/chip8-roms at commit c723a9e.
#                         Keys: 5 starts and shoots, 4/6 move. Reported
#                         glitchy by the user (it was written for CHIP-48).
#                         From the CHIP-8 Archive (github.com/JohnEarnest/
#                         chip8Archive, commit 761e3ff; everything there is
#                         CC0), the plain CHIP-8 games whose metadata asks
#                         for exactly the VIP quirks (clipping, display
#                         wait, VF reset; all switches down):
#                         dinorun.ch8 (Dino Run, 5 = start/jump),
#                         wdl.ch8 (a word game), knumberknower.ch8.
set -euo pipefail
# one locale on every machine: 7z decodes an archive's non-ASCII names by
# it, and the worker (Debian, no LANG) and the desktop differ
export LC_ALL=C.UTF-8

cd "$(dirname "$0")/.."

fetch() { # dir url name sha256
    local dir=$1 url=$2 name=$3 sum=$4 out="$1/$3"
    mkdir -p "$dir"
    if [[ ! -f $out ]]; then
        echo "fetch-roms: $out"
        curl -sSfL --retry 3 -o "$out.part" "$url"
        mv "$out.part" "$out"
    fi
    if ! echo "$sum  $out" | sha256sum --quiet -c -; then
        echo "fetch-roms: $out: SHA-256 mismatch (delete it to re-fetch)" >&2
        exit 1
    fi
}

# Files from an archive into DIR under their own names, through a fresh
# temporary directory; a file the archive didn't give stops here (side
# track K, 2026-10-14: extracting straight into an existing relative
# directory, p7zip 17.05 wrote raposadosol_v20.sms under the working
# directory's name in one worktree, and sms-spec found no ROM; not
# understood, avoided this way)
unpack() { # archive dir file...
    local a=$1 d=$2 t f
    shift 2
    t=$(mktemp -d) || exit 1
    7z e -y -bso0 -bsp0 -o"$t" "$a" "$@" || { rm -rf "$t"; exit 1; }
    for f in "$@"; do
        [[ -f $t/$f ]] || { echo "fetch-roms: $a: no $f in it" >&2; rm -rf "$t"; exit 1; }
        mv -f "$t/$f" "$d/$f"
    done
    rm -rf "$t"
}

T=https://github.com/Timendus/chip8-test-suite/releases/download/v4.2
D=roms/chip8/timendus
fetch $D "$T/1-chip8-logo.ch8" 1-chip8-logo.ch8 15f7fb887ea4cb8e40615bb20b0cfd993ef55ca84c535c8c8f3fafa8bf129724
fetch $D "$T/2-ibm-logo.ch8"   2-ibm-logo.ch8   00072a250d2f7ccaa3ecc0182bac73e63c168abab96d7ea2df5eba6a4da49067
fetch $D "$T/3-corax%2B.ch8"   3-corax+.ch8     1c7e14eae14d6d5e1e47693804110354cbc4081defe4e6e5d9167c25ffc7b4b0
fetch $D "$T/4-flags.ch8"      4-flags.ch8      f00ddadd37bc878473de0c8f16faecf9985dea39036a3a796d551bc9fec47cfa
fetch $D "$T/5-quirks.ch8"     5-quirks.ch8     d839350268a3e73c7a16562b3d23c85aa1b92a567f5f61bd6727b1ea44635679
fetch $D "$T/6-keypad.ch8"     6-keypad.ch8     558902b0e406bb97dc808c16d55abf493706598246e3c77aea9d9401063169c9
fetch $D "$T/7-beep.ch8"       7-beep.ch8       bb8eb25fbfb7520af47d084b9e222914c308d6eec37c10435efa732894cb41c5
fetch $D "$T/8-scrolling.ch8"  8-scrolling.ch8  3f43507c45a949e5b014445853205dd1f36bb532cf25baa22209b7c300c596d7
K=https://raw.githubusercontent.com/kripod/chip8-roms/c723a9ed1205a215c5b1e45e994eb54acc243c9e/games
fetch roms/chip8/games "$K/Space%20Invaders%20%5BDavid%20Winter%5D.ch8" space-invaders.ch8 00bb7001de52b562d98357d33375406c6d0308466d7d95c2580537d2451a75cd
A=https://raw.githubusercontent.com/JohnEarnest/chip8Archive/761e3ffc63f43e6a849e80714122feff2afca208/roms
fetch roms/chip8/games "$A/dinorun.ch8"       dinorun.ch8       7663be25d36d057c278ea29f614ccf4a931f82cb57ab7cde0d2ac953932e6c9b
fetch roms/chip8/games "$A/wdl.ch8"           wdl.ch8           63f9c2180ec780e7106d316492a655f52e9a5bfe5bee87338af3ec7ff5148123
fetch roms/chip8/games "$A/knumberknower.ch8" knumberknower.ch8 c9da4677a9d81f4a3df15c1b984cab30a181e19029cf6245090bd7778a7bb697

# --- Game Boy (milestone 2) -------------------------------------------------
# roms/gb/blargg/     Blargg's GB test ROMs (no formal licence; freely
#                     distributed by the author, so fetched, never committed),
#                     from the mirror github.com/retrio/gb-test-roms at
#                     commit c240dd7. Renamed to names without spaces or
#                     commas: "03-op sp,hl.gb" -> 03-op_sp_hl.gb, and so on.
# roms/gb/mooneye/    Mooneye Test Suite (MIT, LICENSE in the archive),
#                     release mts-20260714-0944-31510e1 from gekkio.fi; the
#                     archive is hash-checked, then unpacked whole.
# roms/gb/dmg-acid2/  dmg-acid2 v1.0 (MIT), github.com/mattcurrie/dmg-acid2:
#                     the release ROM and img/reference-dmg.png at commit
#                     8a98ce7 (160x144, 2-bit grey: the screen to match).
# Hashes recorded on 2026-10-02.
B=https://raw.githubusercontent.com/retrio/gb-test-roms/c240dd7d700e5c0b00a7bbba52b53e4ee67b5f15
D=roms/gb/blargg/cpu_instrs
fetch $D "$B/cpu_instrs/cpu_instrs.gb"                             cpu_instrs.gb           8c5e12f41e0ba5bbca796944f92ffe6de28809198682c4332e38d1b3cf56fcf2
fetch $D "$B/cpu_instrs/individual/01-special.gb"                  01-special.gb           fe61349cbaee10cc384b50f356e541c90d1bc380185716706b5d8c465a03cf89
fetch $D "$B/cpu_instrs/individual/02-interrupts.gb"               02-interrupts.gb        fb90b0d2b9501910c49709abda1d8e70f757dc12020ebf8409a7779bbfd12229
fetch $D "$B/cpu_instrs/individual/03-op%20sp%2Chl.gb"             03-op_sp_hl.gb          ca553e606d9b9c86fbd318f1b916c6f0b9df0cf1774825d4361a3fdff2e5a136
fetch $D "$B/cpu_instrs/individual/04-op%20r%2Cimm.gb"             04-op_r_imm.gb          7686aa7a39ef3d2520ec1037371b5f94dc283fbbfd0f5051d1f64d987bdd6671
fetch $D "$B/cpu_instrs/individual/05-op%20rp.gb"                  05-op_rp.gb             d504adfa0a4c4793436a154f14492f044d38b3c6db9efc44138f3c9ad138b775
fetch $D "$B/cpu_instrs/individual/06-ld%20r%2Cr.gb"               06-ld_r_r.gb            17ada54b0b9c1a33cd5429fce5b765e42392189ca36da96312222ffe309e7ed1
fetch $D "$B/cpu_instrs/individual/07-jr%2Cjp%2Ccall%2Cret%2Crst.gb" 07-jr_jp_call_ret_rst.gb ab31d3daaaa3a98bdbd9395b64f48c1bdaa889aba5b19dd5aaff4ec2a7d228a3
fetch $D "$B/cpu_instrs/individual/08-misc%20instrs.gb"            08-misc_instrs.gb       974a71fe4c67f70f5cc6e98d4dc8c096057ff8a028b7bfa9f7a4330038cf8b7e
fetch $D "$B/cpu_instrs/individual/09-op%20r%2Cr.gb"               09-op_r_r.gb            b28e1be5cd95f22bd1ecacdd33c6f03e607d68870e31a47b15a0229033d5ba2a
fetch $D "$B/cpu_instrs/individual/10-bit%20ops.gb"                10-bit_ops.gb           7f5b8e488c6988b5aaba8c2a74529b7c180c55a58449d5ee89d606a07c53514a
fetch $D "$B/cpu_instrs/individual/11-op%20a%2C%28hl%29.gb"        11-op_a_hl.gb           0ec0cf9fda3f00becaefa476df6fb526c434abd9d4a4beac237c2c2692dac5d3
D=roms/gb/blargg/mem_timing
fetch $D "$B/mem_timing/mem_timing.gb"                             mem_timing.gb           791cb418fd054b482a6ada5017e2405b96c2373a0f3914c6b23afeee76fa98ce
fetch $D "$B/mem_timing/individual/01-read_timing.gb"              01-read_timing.gb       52724532c5709e38e947eb429337c124c38bc68f373874435a7460548098b617
fetch $D "$B/mem_timing/individual/02-write_timing.gb"             02-write_timing.gb      eea92d3f4e95aab5910e0f7080916a3c42a2b8deae1ee5d45d1e3751d648f3f6
fetch $D "$B/mem_timing/individual/03-modify_timing.gb"            03-modify_timing.gb     2e9067c670ff8b45916bf321677ad04a6896d06a057dbcb82ae9f208a1ae9c34
D=roms/gb/blargg
fetch $D "$B/instr_timing/instr_timing.gb"                         instr_timing.gb         646067b3d6c79fda810e9c3f1cb7c0efd5abb0a7ac06437c54e65720c15d9925
fetch $D "$B/mem_timing-2/mem_timing.gb"                           mem_timing-2.gb         ab9daf4c51f417c468ad0a61dab66872cf8b5a50e6eb08d989b581d15f3cf461
fetch $D "$B/halt_bug.gb"                                          halt_bug.gb             ddebfb81f992132954a5580e16cfb97346a4e061c762634d7701328e23fea7c6
fetch $D "$B/dmg_sound/dmg_sound.gb"                               dmg_sound.gb            c34e740664eb14b42c39750434e3e105fc92d774a98fb671594a48e972401630
D=roms/gb/blargg/dmg_sound   # the 12 single tests (results in cartridge RAM at $A000)
fetch $D "$B/dmg_sound/rom_singles/01-registers.gb" 01-registers.gb c6b9fa4b9d9d26919b33ebe78a6ef19ad2df854186cf741ca2746179cc9fc3f1
fetch $D "$B/dmg_sound/rom_singles/02-len%20ctr.gb" 02-len_ctr.gb 745544125a5065729cab22494a79f65c3836e4426f89e4cc43d330afe711b413
fetch $D "$B/dmg_sound/rom_singles/03-trigger.gb" 03-trigger.gb bb11e7266a7143bafb8aa2a73ca70957c6011f36cd4e0b6aaf0678378294e75c
fetch $D "$B/dmg_sound/rom_singles/04-sweep.gb" 04-sweep.gb 58bc14541d91bb020c7761b423825b3432cfde7ee2fa4d1116126e4cb9573c7e
fetch $D "$B/dmg_sound/rom_singles/05-sweep%20details.gb" 05-sweep_details.gb f582ca3a0b2544b9510797d7d4dd56a17f53e000511b0ae88a6353200dcd9167
fetch $D "$B/dmg_sound/rom_singles/06-overflow%20on%20trigger.gb" 06-overflow_on_trigger.gb 1a511e95e84ed6fe6077cabf451a98b4e01fdef59cadae3efbc178113eb064b1
fetch $D "$B/dmg_sound/rom_singles/07-len%20sweep%20period%20sync.gb" 07-len_sweep_period_sync.gb 56bf5b0c18b996929c9b052ba5a02b450cb619bb6c24cc1d34ea20951c977bf7
fetch $D "$B/dmg_sound/rom_singles/08-len%20ctr%20during%20power.gb" 08-len_ctr_during_power.gb 31cb41f7be106a708ec0bc94f2a9d0b506d247cd591e9d0a2a6a960dc3bf6595
fetch $D "$B/dmg_sound/rom_singles/09-wave%20read%20while%20on.gb" 09-wave_read_while_on.gb 378b86f6a25daa16855260d7ef0c24e48146ba05df434a0b5413983a1447e875
fetch $D "$B/dmg_sound/rom_singles/10-wave%20trigger%20while%20on.gb" 10-wave_trigger_while_on.gb fa63c8ed7473411e54285d318e33bf23ff6d637ed2caa7555ddeaf80578e3279
fetch $D "$B/dmg_sound/rom_singles/11-regs%20after%20power.gb" 11-regs_after_power.gb d27dab46e8b881028723f1975328572d38e25d295289b974cd698916a0be5dab
fetch $D "$B/dmg_sound/rom_singles/12-wave%20write%20while%20on.gb" 12-wave_write_while_on.gb 2efbecd2c6d40928d44f45da4f634626bcd2790165b3949aedfe377c73913774
M=mts-20260714-0944-31510e1
fetch roms/gb "https://gekkio.fi/files/mooneye-test-suite/$M/$M.tar.xz" $M.tar.xz 6d4fdda2f1d8d2f5f51b0ff3f6f3cc2fbae047aa395a39c82bda3a0e7cbd2641
if [[ ! -f roms/gb/mooneye/$M ]]; then
    echo "fetch-roms: roms/gb/mooneye/ (from $M.tar.xz)"
    rm -rf roms/gb/mooneye
    mkdir -p roms/gb/mooneye
    tar -xJf roms/gb/$M.tar.xz -C roms/gb/mooneye --strip-components=1
    touch roms/gb/mooneye/$M
fi
fetch roms/gb/dmg-acid2 https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb dmg-acid2.gb 464e14b7d42e7feea0b7ede42be7071dc88913f75b9ffa444299424b63d1dff1
fetch roms/gb/dmg-acid2 https://raw.githubusercontent.com/mattcurrie/dmg-acid2/8a98ce731f96dde032ffb22ec36dc985d78fdb18/img/reference-dmg.png reference-dmg.png ca966d50895c7efef05838590d148c2cbfd7fba57dab986f25b35b4da71abb57
# roms/gb/games/      a freely licensed homebrew game for the M2 exit check:
#                     libbet.gb, "Libbet and the Magic Floor" v0.08 by Damian
#                     Yerrick (zlib License, covering code and graphics;
#                     github.com/pinobatch/libbet). 32 KiB, no MBC, DMG
#                     compatible. Keys: D-pad rolls, A + direction jumps,
#                     Start plays, Select demo. Hash recorded 2026-10-03.
fetch roms/gb/games https://github.com/pinobatch/libbet/releases/download/v0.08/libbet.gb libbet.gb 3607412031c8287cf878299ce96e581e85b852dde703806343b95576fa3ff1a9

# --- Game Boy Color (side track I, phase 3) ----------------------------------
# roms/gbc/cgb-acid2/ cgb-acid2 v1.1 (MIT), github.com/mattcurrie/cgb-acid2:
#                     the release ROM and img/reference.png at the v1.1
#                     commit fa5b7f8 (the screen to match, 15-bit colour).
# roms/gbc/blargg/    Blargg's cgb_sound (all and the 12 singles; results in
#                     cartridge RAM at $A000, as dmg_sound) and
#                     interrupt_time (CGB double speed), from the same
#                     mirror and commit as the DMG ones (no formal licence;
#                     freely distributed). cpu_instrs, instr_timing and
#                     mem_timing run on the CGB too: roms/gb/blargg.
#                     The Mooneye archive above holds the CGB tests in
#                     misc/ (boot_regs-cgb, boot_div-cgbABCDE, boot_hwio-C,
#                     bits/unused_hwio-C, ppu/vblank_stat_intr-C).
# roms/gbc/games/     ucity.gbc, "µCity" v1.3 by Antonio Niño Díaz (code
#                     GPLv3+, graphics and music CC BY-SA 4.0;
#                     codeberg.org/SkyLyrac/ucity): Color only, MBC5 with
#                     128 KiB of battery RAM (the largest save there is).
# Hashes recorded 2026-10-05.
fetch roms/gbc/cgb-acid2 https://github.com/mattcurrie/cgb-acid2/releases/download/v1.1/cgb-acid2.gbc cgb-acid2.gbc 197fb0bcec544f0400527fc707e0a94f55435974986e6986b424ace5de81720e
fetch roms/gbc/cgb-acid2 https://raw.githubusercontent.com/mattcurrie/cgb-acid2/fa5b7f86d6fb599f79e55169494d981a7af75a31/img/reference.png reference.png 9ea9c262c5383353e77d715d021a0f7c5ccbe438f88082cb225756e50c4fdf01
D=roms/gbc/blargg
fetch $D "$B/cgb_sound/cgb_sound.gb" cgb_sound.gb de2101eb718a2bf5f778df76e73a63f4598d9b471559f9a85adc26efa692053b
fetch $D "$B/interrupt_time/interrupt_time.gb" interrupt_time.gb 1a8f8f3bc609a92a5838214c00906c81e7cc77b3b9a63be5a1ec6d513f234929
D=roms/gbc/blargg/cgb_sound
fetch $D "$B/cgb_sound/rom_singles/01-registers.gb" 01-registers.gb 8415d6c69f7954e9365a586bac235f5ce39270ece32c09cb8ebbbff043b9852c
fetch $D "$B/cgb_sound/rom_singles/02-len%20ctr.gb" 02-len_ctr.gb a1ccf47375a2bede3077ac85a4903cb037dd8a4c7c85fc5d0c176af52915271f
fetch $D "$B/cgb_sound/rom_singles/03-trigger.gb" 03-trigger.gb 70173be396d14ac955c47aededcee90ba994206cc403273d0bb49ba163ae23ef
fetch $D "$B/cgb_sound/rom_singles/04-sweep.gb" 04-sweep.gb abfd1f6fa9e6701e46135ac9f5bb5b10a46cde22d0fc34922636aa09a5647483
fetch $D "$B/cgb_sound/rom_singles/05-sweep%20details.gb" 05-sweep_details.gb f51a80c5538fe4b3afbc5c9af0e689a67869b1e2bbb73d4c60a8874826d056cb
fetch $D "$B/cgb_sound/rom_singles/06-overflow%20on%20trigger.gb" 06-overflow_on_trigger.gb 5701414d13ba55d25b59d13076e2e9cb59a9da6890595b41464f957605e317c0
fetch $D "$B/cgb_sound/rom_singles/07-len%20sweep%20period%20sync.gb" 07-len_sweep_period_sync.gb 47d57ba87c3a93050ee340cf49b9d276e62e9573542b92b1de5fa2f17e3716c1
fetch $D "$B/cgb_sound/rom_singles/08-len%20ctr%20during%20power.gb" 08-len_ctr_during_power.gb be7948c70946e0b81c178b8778ef1907231e734650062f8fa8cdb1ceb5ac8c65
fetch $D "$B/cgb_sound/rom_singles/09-wave%20read%20while%20on.gb" 09-wave_read_while_on.gb 9b101c68b2de1930df74c19aaf22cce74d37dd5aade93855c805ecb2e1e83fd3
fetch $D "$B/cgb_sound/rom_singles/10-wave%20trigger%20while%20on.gb" 10-wave_trigger_while_on.gb cbea1771a5384f4d8f8a249056224a750ad5ee4671297836e4ba87e689af2a23
fetch $D "$B/cgb_sound/rom_singles/11-regs%20after%20power.gb" 11-regs_after_power.gb d347dd195a78c2d0f665b94da493a857218f032b014654b84ddb01f7b8e9befc
fetch $D "$B/cgb_sound/rom_singles/12-wave.gb" 12-wave.gb 7bbd2e78e159de4297f567fec7851f9b90c3076d18d1043af5f30e14bd66cd0a
fetch roms/gbc/games https://codeberg.org/SkyLyrac/ucity/releases/download/v1.3/ucity.gbc ucity.gbc 9422ee2ca7b7ea1d46b58b2a429fff3f354dfd3e732dee1e7ae6220f148ce6e0

# --- NES and the shared 6502 (milestone 3) ----------------------------------
# roms/6502/          Tom Harte's SingleStepTests (MIT), github.com/
#                     SingleStepTests/65x02 at commit 2f6980a: every opcode
#                     of nes6502/v1 (the 2A03, no decimal mode; ~1 GB, fetched
#                     once) and the decimal-mode opcodes of 6502/v1 (ADC, SBC,
#                     ARR, RRA, ISC). SHA-256 per file in tools/roms-6502.sha256.
# roms/nes/nestest/   kevtris's nestest (freely distributed, no formal
#                     licence) with its log and notes;
# roms/nes/blargg/    Blargg's NES test ROMs (no formal licence; freely
#                     distributed, so fetched, never committed): both from
#                     the mirror github.com/christopherpow/nes-test-roms at
#                     commit 95d8f62, one folder per suite.
# roms/nes/accuracycoin-d103cbd/  AccuracyCoin (100thCoin, MIT) at commit d103cbd;
#                     the repo's assembled .nes (docs/systems/nes.md,
#                     "AccuracyCoin": update deliberately, note score changes).
# Hashes recorded on 2026-10-03.
S=https://raw.githubusercontent.com/SingleStepTests/65x02/2f6980a2d95757486c7bee24355c360e40e2a224
while read -r sum path; do
    case $path in
        nes6502/*) fetch roms/6502/nes6502 "$S/nes6502/v1/${path#*/}" "${path#*/}" "$sum" ;;
        6502/*)    fetch roms/6502/6502    "$S/6502/v1/${path#*/}"    "${path#*/}" "$sum" ;;
    esac
done < tools/roms-6502.sha256
N=https://raw.githubusercontent.com/christopherpow/nes-test-roms/95d8f621ae55cee0d09b91519a8989ae0e64753b
fetch roms/nes/nestest "$N/other/nestest.nes" nestest.nes f67d55fd6b3cf0bad1cc85f1df0d739c65b53e79cecb7fea8f77ec0eadab0004
fetch roms/nes/nestest "$N/other/nestest.log" nestest.log 442c4dd5539c7e88b3fd73c7b732a7eadbd22b47c2cd9e58397ef147f64f6f8f
fetch roms/nes/nestest "$N/other/nestest.txt" nestest.txt 8291241ba9a0885b9a604a4685101a1473e22b3aa070bc828e3b8c342d7f71fb
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/all_instrs.nes" all_instrs.nes 353870c157242e3d428ef7387109deaee0d2e158bdb432ab9aae4e657072c785
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/official_only.nes" official_only.nes 589b8835deb5cbc69618dac193a3dbd675540f7f2794e2d2a92e97beb8abc3cb
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/01-basics.nes" 01-basics.nes 4dd1cdd406bc3f747972e7da314ce8ca89321eb7a836c1ced569ee54ae44a384
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/02-implied.nes" 02-implied.nes 1c4d4fa130cf6feebc072543a5cd3627ae71063b56b08642bf43e9a6c6f44996
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/03-immediate.nes" 03-immediate.nes 6f7ad8ff31c762c37deaee0f323df03eb94025cf1f3b0343ebe6fe567da0e943
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/04-zero_page.nes" 04-zero_page.nes 7a8feada4bb4460250c8f05401e5d728878bbe71956756d0b11d488e57eb12fd
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/05-zp_xy.nes" 05-zp_xy.nes 767f422dc4e651e331456b207f7c6d60d19329fde0c0827e83591dbd91ae5e23
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/06-absolute.nes" 06-absolute.nes 98df36dc4fcc4f37d9eb0539c71283020776b1e5dc6a6ce58671739a8d6534af
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/07-abs_xy.nes" 07-abs_xy.nes 9ff58d77d8d384cc918fcd3ed877898c5e7330cd475ed2dafb11cbe80ff32eff
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/08-ind_x.nes" 08-ind_x.nes 2ec6f5d4a8caee5d8295cebe563f203c26ea9bc05f1dbc967feb88f5dc4f261f
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/09-ind_y.nes" 09-ind_y.nes 0fbc8b228d5daa83a4a083bf87ae3a61b5247ebdd91a6b91c8cf8c42784804ac
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/10-branches.nes" 10-branches.nes 63ab768e88931db6f7dfcfafe43d5e29ebc3dcb80da8fc7fcda8c930f34aef54
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/11-stack.nes" 11-stack.nes c534191fe3ea4c8940944fda98dd58eb42710268d453f97e8e2c4ae7f15f9cdb
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/12-jmp_jsr.nes" 12-jmp_jsr.nes f5b4652690fc04e6b573a2b3b54a29407ad0615d3c264e7cb618b6694b50de55
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/13-rts.nes" 13-rts.nes b711d25bc55585c252046a1304a0bc64c13cacce7c96a1bac5c8e91f9fc2597f
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/14-rti.nes" 14-rti.nes f084b00605be1840946b53935032581e68abe1bb24479942751cfe46ddfcb280
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/15-brk.nes" 15-brk.nes da7ae9a191c4483b540771e15b1f6f18df68f1d1ecd717b59ea8b1ee3596ec3e
fetch roms/nes/blargg/instr_test-v5 "$N/instr_test-v5/rom_singles/16-special.nes" 16-special.nes 7d03410b61784e49920901e84b00a4f31a19078391f20005c6fac9036d2190f7
fetch roms/nes/blargg/instr_timing "$N/instr_timing/rom_singles/1-instr_timing.nes" 1-instr_timing.nes e260068839fe3d0402376e97e4ee15f5790ee77c701fd0700bba057527910222
fetch roms/nes/blargg/instr_timing "$N/instr_timing/rom_singles/2-branch_timing.nes" 2-branch_timing.nes 0afaa393f375844ab98834c1ecba7fa6d8c44880c8b6e738936d0f04a84c8538
fetch roms/nes/blargg/cpu_interrupts_v2 "$N/cpu_interrupts_v2/rom_singles/1-cli_latency.nes" 1-cli_latency.nes e402d36118f77dcbbe8ddca90c15fc76a46bcb30b25cb028c383e4a621de5fc0
fetch roms/nes/blargg/cpu_interrupts_v2 "$N/cpu_interrupts_v2/rom_singles/2-nmi_and_brk.nes" 2-nmi_and_brk.nes 6e6bf6205930afcfebdc213c583df53986a688a8b36f8856b805ef4c1853e6eb
fetch roms/nes/blargg/cpu_interrupts_v2 "$N/cpu_interrupts_v2/rom_singles/3-nmi_and_irq.nes" 3-nmi_and_irq.nes 3008a9524d174a8aca562ff0361eba81da53e38cf1ebb5125322fe151f14d945
fetch roms/nes/blargg/cpu_interrupts_v2 "$N/cpu_interrupts_v2/rom_singles/4-irq_and_dma.nes" 4-irq_and_dma.nes 6d7b4c1947ada64679af56cf0c227286b2408afe1747dfaa4dc7363d57ff87f6
fetch roms/nes/blargg/cpu_interrupts_v2 "$N/cpu_interrupts_v2/rom_singles/5-branch_delays_irq.nes" 5-branch_delays_irq.nes f9e10b4a24d8f3cd3e51fb7457c72858aab96a6467fdbbd806d0661c2d32fdc7
fetch roms/nes/blargg/cpu_dummy_reads "$N/cpu_dummy_reads/cpu_dummy_reads.nes" cpu_dummy_reads.nes db4f91b80c5fbc123e7dcb420fb7fea9b8a18613edf4de7f3d1e3ed95e3117c9
fetch roms/nes/blargg/cpu_dummy_writes "$N/cpu_dummy_writes/cpu_dummy_writes_oam.nes" cpu_dummy_writes_oam.nes 7c1d71a38b2e873d0874add8b823ff39b99151bb29f50096d8021787020c566c
fetch roms/nes/blargg/cpu_dummy_writes "$N/cpu_dummy_writes/cpu_dummy_writes_ppumem.nes" cpu_dummy_writes_ppumem.nes f59ac329f4872277ccbeff9dd595b901d861af8d53e8a43dcca93bb86752a6b3
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/01-vbl_basics.nes" 01-vbl_basics.nes 06aea5af4edab4e3141c939cd5ac9936f8758203b25dcaf84ae1a09db49e024a
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/02-vbl_set_time.nes" 02-vbl_set_time.nes dd98856130078844e3aa4bd95a9be8ab501ea84c089f1d8ad49a1b20af4b3a80
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/03-vbl_clear_time.nes" 03-vbl_clear_time.nes 787fdaa4dd6c5b6df5f4308fb6d55b57e2c2f69bd5ecdf8ad5c69735db4fcc72
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/04-nmi_control.nes" 04-nmi_control.nes 84722c75b896c47c8642f83220230fe14f0a31e55e26ecb83c400e6a26d91b32
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/05-nmi_timing.nes" 05-nmi_timing.nes 72e515d689d7404ae5779b8c9c4c7b3563a755a94bd44864516f1b03df044482
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/06-suppression.nes" 06-suppression.nes 811dd5997bbf48c2e5687ab06845f17ea76b2be472786596c334137582cc72aa
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/07-nmi_on_timing.nes" 07-nmi_on_timing.nes 1ed154363660b5775b112ae63ce9bb4e400ebde2afef4d0ac12fc433efda3702
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/08-nmi_off_timing.nes" 08-nmi_off_timing.nes 1d2a4093091c8e58a7f99d6a3531bbc6346b52cfc59bcb17ca04c1f2376cf2fc
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/09-even_odd_frames.nes" 09-even_odd_frames.nes 1ac04283021ddd9294cc74ee709c55e20a350dc4815c15a8a93b3654837e858d
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/rom_singles/10-even_odd_timing.nes" 10-even_odd_timing.nes 7217d2d172ce11ad45c4da40c2f22201cf0eb758bc2cd8dd39d2cf0a7d4ca83e
fetch roms/nes/blargg/ppu_vbl_nmi "$N/ppu_vbl_nmi/ppu_vbl_nmi.nes" ppu_vbl_nmi.nes 8dbab1be785585c399cf055ef02147b788ab75fd80e81cf9568a2feafc03fb7d
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/01.basics.nes" 01.basics.nes 51819e8e502bd88fe3b7244198a074dbeef2e848f66c587be04b04f1f0d4bb52
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/02.alignment.nes" 02.alignment.nes 125bbb3ce1e67370f1f4559c2ad3221e52a3e98880b9789400292b5f3a8b39e6
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/03.corners.nes" 03.corners.nes 9dd57776bc6267fe6183c5521d67cbe3fccc6662ae545eb2c419949bf39644d3
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/04.flip.nes" 04.flip.nes 5f7142bddb51b7577f93fa22f9f668efebbeea00346d7255089e1863acb9d46a
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/05.left_clip.nes" 05.left_clip.nes 69b329658c17b953f149c2f0de77eb272089df22c815bd2fd3d6f43206791c13
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/06.right_edge.nes" 06.right_edge.nes 8e6653fcb869e06873e29e5e4423122ea72ba0bf38f3ba9e39f471420db759a4
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/07.screen_bottom.nes" 07.screen_bottom.nes 05849956f80267838c5b6556310266b794078a4300841cbb36339fd141905a0b
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/08.double_height.nes" 08.double_height.nes 127fd966b6b32d6d88a53c5f59d7e938827783c9ad056091f119be1c4ab21c71
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/09.timing_basics.nes" 09.timing_basics.nes 311698c717e50150edd0b5fd0016c41de686463205c20efb5630d6adb90859fd
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/10.timing_order.nes" 10.timing_order.nes 0f36bc07bfe51c416e3cc1a5231053572aa6b15aa60e6d2fd0568be49b6dc2e9
fetch roms/nes/blargg/sprite_hit_tests_2005.10.05 "$N/sprite_hit_tests_2005.10.05/11.edge_timing.nes" 11.edge_timing.nes 5a7c121f6e76617be88a0a7035c0e402293be5c685c95b97190a8d70835736ab
fetch roms/nes/blargg/sprite_overflow_tests "$N/sprite_overflow_tests/1.Basics.nes" 1.Basics.nes 1a6782f63ccb3a3dd1aa6a24272036c9c3aa232c2d1ff0b21e872741a3ee4fe2
fetch roms/nes/blargg/sprite_overflow_tests "$N/sprite_overflow_tests/2.Details.nes" 2.Details.nes 6405a7ff1042fe7a50d9bfe521e43460a3251799425bd3e1863b29b67e7cd587
fetch roms/nes/blargg/sprite_overflow_tests "$N/sprite_overflow_tests/3.Timing.nes" 3.Timing.nes 2252ec8fc35932b408f409ef9b6863edf084aa871fc10ad180b0ac4c2468ef8c
fetch roms/nes/blargg/sprite_overflow_tests "$N/sprite_overflow_tests/4.Obscure.nes" 4.Obscure.nes aebf2199344321465ae0d8dcd81f6c528c7f661f31f1814711365b4e573a8263
fetch roms/nes/blargg/sprite_overflow_tests "$N/sprite_overflow_tests/5.Emulator.nes" 5.Emulator.nes cf994454219696de82794f0b84f2bd63458444d12a1171c85bba8697ab94acb4
fetch roms/nes/blargg/oam_read "$N/oam_read/oam_read.nes" oam_read.nes f298973dabeb61ca35007445f7a615f77e87703c958c870986af83b1aabde926
fetch roms/nes/blargg/apu_test "$N/apu_test/rom_singles/1-len_ctr.nes" 1-len_ctr.nes aacf86c1d773badd11392e54506a43a06a3dd0b67a4c255909d1daf770a4a1e2
fetch roms/nes/blargg/apu_test "$N/apu_test/rom_singles/2-len_table.nes" 2-len_table.nes c002ff1483b4dfb36a6eb004d49739cd58a2dff16e0bac167d5a7c12235caeeb
fetch roms/nes/blargg/apu_test "$N/apu_test/rom_singles/3-irq_flag.nes" 3-irq_flag.nes dd888551665937391a2d691b1f96d1858316dbfce4951306146e9e396367f079
fetch roms/nes/blargg/apu_test "$N/apu_test/rom_singles/4-jitter.nes" 4-jitter.nes bff573d72d0f134fe307f0bb8b968b8d2ffdb85e8aadad9c152839068d6db32a
fetch roms/nes/blargg/apu_test "$N/apu_test/rom_singles/5-len_timing.nes" 5-len_timing.nes 4d88f8cc0b21303dc151af4d0f4169d79284634a73082d7ea1ae5cfafedd1e46
fetch roms/nes/blargg/apu_test "$N/apu_test/rom_singles/6-irq_flag_timing.nes" 6-irq_flag_timing.nes fc1daff82dd1a49c7c1242392ffbf1c6f44fb70156868582117f2a844cc4dffd
fetch roms/nes/blargg/apu_test "$N/apu_test/rom_singles/7-dmc_basics.nes" 7-dmc_basics.nes 547324867ee0ba2aa11401001d8d1288530aa4e0ecaaac1667ce79980a388ec1
fetch roms/nes/blargg/apu_test "$N/apu_test/rom_singles/8-dmc_rates.nes" 8-dmc_rates.nes 5d9a79a505b37fa277cacc95a362f7e2a56e59ace7a698213d78432cc06a8867
fetch roms/nes/blargg/dmc_dma_during_read4 "$N/dmc_dma_during_read4/dma_2007_read.nes" dma_2007_read.nes a2e0fa3f6f155cbe0b8c9517b2f6a57f1fd68f13711c11d6d2fe5676c522d7b2
fetch roms/nes/blargg/dmc_dma_during_read4 "$N/dmc_dma_during_read4/dma_2007_write.nes" dma_2007_write.nes 54c75d491c685fb4cfff281bcf3e199a41e95f6c523e2b0607d67ba039f19f84
fetch roms/nes/blargg/dmc_dma_during_read4 "$N/dmc_dma_during_read4/dma_4016_read.nes" dma_4016_read.nes c6af72e11c197b449129921a9992db2351d9121bb593b3d0ab71895b646b0ebe
fetch roms/nes/blargg/dmc_dma_during_read4 "$N/dmc_dma_during_read4/double_2007_read.nes" double_2007_read.nes 779e6e7db863a7405a3dda8723b8517a23d271e78ce4802970fb0a7d3039ce6b
fetch roms/nes/blargg/dmc_dma_during_read4 "$N/dmc_dma_during_read4/read_write_2007.nes" read_write_2007.nes bc5281ca3f12a6d0ac9fe1a5e727ecc3cac5fc6a47f45ac130d644f0dbd522cf
fetch roms/nes/blargg/mmc3_test_2 "$N/mmc3_test_2/rom_singles/1-clocking.nes" 1-clocking.nes b06d8a97f0ca672be92c841d6af7d1e650696e86e9cc0cf6eeb90d67a6ab499b
fetch roms/nes/blargg/mmc3_test_2 "$N/mmc3_test_2/rom_singles/2-details.nes" 2-details.nes e7af16c764b119e60effb7b1cfeec3dd8e2e657041283693cdbbeedb4081f1e3
fetch roms/nes/blargg/mmc3_test_2 "$N/mmc3_test_2/rom_singles/3-A12_clocking.nes" 3-A12_clocking.nes b375f15b9f9d372c8084b9c50928be9e41a3ac48be831ce82d203c18891433ad
fetch roms/nes/blargg/mmc3_test_2 "$N/mmc3_test_2/rom_singles/4-scanline_timing.nes" 4-scanline_timing.nes 14a220b9d1272acc7a820ab38e9762a7cdf2d54c65e753be87f23dfcaf1bb845
fetch roms/nes/blargg/mmc3_test_2 "$N/mmc3_test_2/rom_singles/5-MMC3.nes" 5-MMC3.nes e0824123d60b83868dac1189b28250f8e10376a01be468a5a74aa59937cb32ca
fetch roms/nes/blargg/mmc3_test_2 "$N/mmc3_test_2/rom_singles/6-MMC3_alt.nes" 6-MMC3_alt.nes 56698b6918453d161a8d4e51f66e363d6966b054939c8176c53c401a6b55269b
# roms/nes/mmc5/     MMC5 (mapper 5) tests (side track D; no licence stated,
#                     as the rest of that collection): Quietust's ExRAM
#                     test (code run from ExRAM), Drag's scanline-counter
#                     test, AWJ's CHR bank test (mmc5test_v2). Judged by eye
#                     (no result protocol); nes-spec keeps their screens.
fetch roms/nes/mmc5 "$N/exram/mmc5exram.nes" mmc5exram.nes 8297cfb986a8393ffbd47549b120d16fac53c1bf96da4f26ceb9ff30ecca50d2
fetch roms/nes/mmc5 "$N/mmc5test/mmc5test.nes" mmc5test.nes 540eaebed9d44067c57de352719fbc3582bbea3946ec3d4d1cfd2a6c0a31c819
fetch roms/nes/mmc5 "$N/mmc5test_v2/mmc5test.nes" mmc5test_v2.nes f18f60a27cae9c00b51782caa3b77cf96a11e1c45e4323a9815474728e5b2980
# (d103cbd since 2026-10-22, side D: 146 tests; in its own folder, so a
# branch still on 673ef55, roms/nes/accuracycoin/, keeps its copy: the
# worktrees share one roms/)
fetch roms/nes/accuracycoin-d103cbd https://raw.githubusercontent.com/100thCoin/AccuracyCoin/d103cbd24182f2cda7f59385f8d8a4d2c58f5228/AccuracyCoin.nes AccuracyCoin.nes cafb7cf687eaee77fe7e11c0d83b428e6e13d236fda60a4317ac16cbcd5f0853

# roms/nes/holy-mapperel/  Holy Mapperel 0.02 (Damian Yerrick, zlib licence;
#                     github.com/pinobatch/holy-mapperel, M3 step 3.10): the
#                     release .7z (p7zip from the dev shell), and from it the
#                     test ROMs of the boards M3 runs (mappers 0, 1, 2, 3, 4,
#                     7 within the BRAM budget: PRG <= 256 KiB, CHR ROM
#                     <= 128 KiB, CHR RAM 8 KiB). Results are on screen
#                     (docs/systems/nes.md, "Holy Mapperel").
H=roms/nes/holy-mapperel
fetch $H https://github.com/pinobatch/holy-mapperel/releases/download/v0.02/holy-mapperel-bin-0.02.7z holy-mapperel-bin-0.02.7z 70f85671e21f293599baebb662faeb06a4c04e9c9ceb283d96d4197f09e4ce7a
while read -r sum name; do
    if [[ ! -f $H/$name ]]; then
        command -v 7z >/dev/null || { echo "fetch-roms: 7z missing (run inside nix develop)" >&2; exit 1; }
        7z e -y -bso0 -bsp0 -o"$H" "$H/holy-mapperel-bin-0.02.7z" "testroms/$name"
    fi
    if ! echo "$sum  $H/$name" | sha256sum --quiet -c -; then
        echo "fetch-roms: $H/$name: SHA-256 mismatch" >&2
        exit 1
    fi
done <<'EOF'
16ad2760079963170df9f4f0ad60b87419402fd89ab832037639fe8b54e6d6fc M0_P32K_C8K_V.nes
1a33ba05af3e7b392e4c320034c618aa903bf9446c49b3d1cba0851f9905d919 M0_P32K_CR8K_V.nes
d02b15ed43c87141f5cbd728ebdddb54332c9c74959ae78667f6012439f00834 M1_P128K.nes
0595f0896d71d6e54ad19b00a2cdfb6d1665e3166167a06935c5971105ed5793 M1_P128K_CR8K.nes
77f77f779720741185bc07442d16dee6b99856ab8ccdb20762cde17570d0e88e M1_P128K_C32K.nes
a3f520601c1cc92842f02b415b803d6b8c95bbf0c34ff6baaca34a79b127765b M1_P128K_C128K.nes
81bd36f2a0aa1ce80e0dfae5a44cc16a1c3ed230b5f2ffdcaf9ac19945687bae M2_P128K_V.nes
c7e83755bd9adbb7c705ea9f29535442af7390444632f9f824fcf4c00632069b M2_P128K_CR8K_V.nes
499891c6d8c7a1e7631bdc601d9d624938842735f92fe1fda7b19ef9fae514b7 M3_P32K_C32K_H.nes
b145558f6d301fd294e3e7641a7a1c414d9f743b1f89aace324c61b72c082e36 M4_P128K.nes
edb301630dae50a8470c1fb914f436434a150c886d9537d072275b751a9b4125 M4_P128K_CR8K.nes
e4a63a3e290e7cf3eb8539d7144a4a26cc448dcc4c06d4e865642f6f10c13afb M7_P128K.nes
4aa0050f36ae17e17701506821e5147df5b843bcdf2827468fa9c66e4b7ac1ba M7_P128K_CR8K.nes
278041824af3a0530d6a1367fa1a1d3bb544d2dd1e97ad8372673f7fe0379b17 M4_P256K_C256K.nes
EOF

# roms/nes/homebrew/  freely licensed NES games for the M3 exit (PLAN.md; picked
#                     2026-10-03, licences in docs/systems/nes.md "Homebrew"):
#                     Thwaite 0.04 (Damian Yerrick, GPLv3, NROM-128) and Double
#                     Action Blaster Guys v2 (NovaSquirrel, zlib, NROM-256 with
#                     CHR RAM, NES 2.0 header).
fetch roms/nes/homebrew https://github.com/pinobatch/thwaite-nes/releases/download/v0.04/thwaite.nes thwaite.nes a2df24d9c9f72e56c2fdc4c703becc47a5700ad0158da8208247635ebeb3779c
fetch roms/nes/homebrew https://github.com/NovaSquirrel/DABG/releases/download/v2/dabg.nes dabg.nes eca79b9d0b546e96c1bc73099e239539fb9c7773ff10a431bb3a9cd6763208ca


# --- The shared Z80 and the Game Gear (milestone 5) -------------------------
# roms/z80/v1/        Tom Harte's SingleStepTests (MIT), github.com/
#                     SingleStepTests/z80 at commit ebe1875: all 1604 opcode
#                     files (1000 tests each, ~1.3 GB, fetched once). SHA-256
#                     per file in tools/roms-z80.sha256 (recorded 2026-10-07).
Z=https://raw.githubusercontent.com/SingleStepTests/z80/ebe1875d48f374bcfd4b505d8eb8ee751568b5f7
while read -r sum path; do
    name=${path#v1/}
    fetch roms/z80/v1 "$Z/v1/${name// /%20}" "$name" "$sum"
done < tools/roms-z80.sha256
# roms/z80/zex/       ZEXDOC and ZEXALL (Frank D. Cringle, 1994, GPL-2.0 or
#                     later; the CP/M .COM originals, run under a BDOS stub
#                     by cpus/z80's z80-zex suite), as kept by github.com/
#                     anotherlin/z80emu at commit 1c418fa (testfiles/).
X=https://raw.githubusercontent.com/anotherlin/z80emu/1c418fa0d719abab9273131113defbe276101d95/testfiles
fetch roms/z80/zex "$X/zexdoc.com" zexdoc.com 34923a7ed82285d3038b2d54bd64899e12173eebb61f9d07b4fc72e78af2ae8f
fetch roms/z80/zex "$X/zexall.com" zexall.com 6e2da55147a04f28d303d5da6a1e6b771557ac244653590a0f24a2d39c8537e8
# roms/gg/games/      the M5 exit game: Waternet v1.0 (joyrider3774, github.com/
#                     joyrider3774/waternet; MIT, the zip's LICENSE; tiles and
#                     music by the author; confirmed as the exit game by the
#                     user 2026-10-07): waternet.gg from the release's Game
#                     Gear zip. Hash recorded 2026-10-07.
G=roms/gg/games
fetch $G https://github.com/joyrider3774/waternet/releases/download/v1.0/Waternet.Sega.Game.Gear.zip Waternet.Sega.Game.Gear.zip 16a06f51168717c252b08687abe6dd5980a4090d5cbe228f5e01d4ed28579fee
if [[ ! -f $G/waternet.gg ]]; then
    command -v 7z >/dev/null || { echo "fetch-roms: 7z missing (run inside nix develop)" >&2; exit 1; }
    7z e -y -bso0 -bsp0 -o"$G" "$G/Waternet.Sega.Game.Gear.zip" waternet.gg LICENSE
fi
# roms/gg/vdptest/    FluBBa's SMS VDP Test 1.31 (2009; smspower.org/Homebrew/
#                     SMSVDPTest-SMS): VDPTEST.sms and its ReadMe from the
#                     release zip. roms/gg/smstestsuite/: sverx's SMS Test
#                     Suite v0.38 (github.com/sverx/SMSTestSuite release).
#                     Neither has a formal licence: fetched, never committed
#                     (the user, 2026-10-07; as Blargg's ROMs and nestest).
#                     Hashes recorded 2026-10-07.
V=roms/gg/vdptest
fetch $V https://www.smspower.org/uploads/Homebrew/SMSVDPTest-SMS-1.31.zip SMSVDPTest-SMS-1.31.zip 6b8f113e54c084d542ae2f87951451724b4dfa85eae95f3b9a5121e6b1bd1179
if [[ ! -f $V/VDPTEST.sms ]]; then
    command -v 7z >/dev/null || { echo "fetch-roms: 7z missing (run inside nix develop)" >&2; exit 1; }
    unpack "$V/SMSVDPTest-SMS-1.31.zip" "$V" VDPTEST.sms VDPTest-ReadMe.txt
fi
fetch roms/gg/smstestsuite https://github.com/sverx/SMSTestSuite/releases/download/v0.38/SMSTestSuite.sms SMSTestSuite.sms edbe62071e94cbd10faa51f9035159f36b1b2fae8e3e4bec2a0f20848d877ebb

# --- The Master System (milestone 6) ----------------------------------------
# The VDP test and sverx's suite are the Game Gear's copies (roms/gg/vdptest,
# roms/gg/smstestsuite, above): both are Master System ROMs.
# roms/sms/games/     the M6 exit game: Raposa do Sol v0.20 (tibonev,
#                     github.com/jflores82/raposadosol; MIT, the repository's
#                     LICENSE at the v0.20 tag, commit edd4bef, pinned beside
#                     it; code, graphics and music by the author, its README;
#                     chosen by the user 2026-10-11): raposadosol_v20.sms from
#                     the release zip. And Waternet's Master System build
#                     (v1.0, as roms/gg/games; MIT, the zip's LICENSE).
#                     Hashes recorded 2026-10-11.
R=roms/sms/games
fetch $R https://github.com/jflores82/raposadosol/releases/download/v0.20/raposadosol_v20.zip raposadosol_v20.zip 56510fc02d944ae0cacbb06957857cbf59423450d5b432ff2faa1d59d7ebfb1c
fetch $R https://raw.githubusercontent.com/jflores82/raposadosol/edd4bef7f20b575c60b1b8a82211a0cdf7b1718c/LICENSE LICENSE.raposadosol 50bd6e85cf41e45117edca7ee49a4cf0bff147a4d04fa8109fe11ee9e4f36773
fetch $R https://github.com/joyrider3774/waternet/releases/download/v1.0/Waternet.Sega.Master.System.zip Waternet.Sega.Master.System.zip b0abd1c7ca992709c4a3fcb4fb3e8a0ffaf42aa2fe216784d00d683347ba2581
if [[ ! -f $R/raposadosol_v20.sms || ! -f $R/waternet.sms ]]; then
    command -v 7z >/dev/null || { echo "fetch-roms: 7z missing (run inside nix develop)" >&2; exit 1; }
    unpack "$R/raposadosol_v20.zip" "$R" raposadosol_v20.sms
    7z e -y -bso0 -bsp0 -o"$R/waternet" "$R/Waternet.Sega.Master.System.zip" waternet.sms LICENSE
    mv "$R/waternet/waternet.sms" "$R/" && mv "$R/waternet/LICENSE" "$R/LICENSE.waternet" && rmdir "$R/waternet"
fi
# roms/sms/nuked-opll/ Nuked-OPLL (nukeykt, github.com/nukeykt/Nuked-OPLL,
#                     GPL-2.0-or-later) at commit 1269cf5 (v1.0.2,
#                     2023-01-19): the YM2413 oracle sms-spec compares
#                     Sms.Spec.Fm with (the user, 2026-10-11: a black box,
#                     fetched and built locally, never committed; nothing of
#                     it is copied into our code). Hashes recorded 2026-10-11.
O=https://raw.githubusercontent.com/nukeykt/Nuked-OPLL/1269cf5a783b65583b50fa2464d08be75830aaa0
fetch roms/sms/nuked-opll "$O/opll.c" opll.c d486915d0238be9650250ac91b0bb4024bfc236acd1893359648e5088144fc45
fetch roms/sms/nuked-opll "$O/opll.h" opll.h 4af73da64dfcb6a19392104c8d37865da9c3a00382ba377617bc1fe1228e82de
fetch roms/sms/nuked-opll "$O/LICENSE" LICENSE 8177f97513213526df2cf6184d8ff986c675afb514d4e68a404010521b880643
# roms/sms/zexall/    ZEXDOC and ZEXALL for the Master System v0.21 (Maxim,
#                     github.com/maxim-zhao/zexall-sms; Frank D. Cringle's
#                     tests, GPL-2.0, the zip's LICENSE): output on the VDP's
#                     screen and the SDSC console. Hash recorded 2026-10-11.
Z=roms/sms/zexall
fetch $Z https://github.com/maxim-zhao/zexall-sms/releases/download/v0.21/ZEXALL-SMS-0.21.zip ZEXALL-SMS-0.21.zip 704e39402516fba832b923395c7579b617896e1f00d2f036696342ab4b5fde7c
if [[ ! -f $Z/zexall.sms ]]; then
    command -v 7z >/dev/null || { echo "fetch-roms: 7z missing (run inside nix develop)" >&2; exit 1; }
    7z e -y -bso0 -bsp0 -o"$Z" "$Z/ZEXALL-SMS-0.21.zip" zexall.sms zexdoc.sms LICENSE README.md
fi

# --- Atari 800XL (milestone 4) ------------------------------------------------
# roms/a800xl/os/     AltirraOS 3.11 (XL/XE kernel, 16 KiB) and Altirra BASIC
#                     (8 KiB; header 1.58, banner 1.59), Avery Lee; licence: "Copying and
#                     distribution of this file, with or without modification,
#                     are permitted in any medium without royalty provided the
#                     copyright notice and this notice are preserved" (FSF
#                     all-permissive; docs/systems/atari800xl.md). Taken from
#                     the C arrays atari800 builds in (github.com/atari800/
#                     atari800, src/roms/, commit d722329): the .c files are
#                     hash-checked, then their bytes written out as .rom.
#                     Atari's own ROMs are never fetched.
# roms/a800xl/acid800/  Acid800 1.2 (Avery Lee, MIT), the standalone tests
#                     (XEX) from the release archive at virtualdub.org; the
#                     tests M4 tracks (A800.Acid800): every hardware test but
#                     cpu_65c816 and pokey_serdirect (they skip themselves
#                     without a 65C816 or a disk drive) and the mod_* helpers.
# roms/a800xl/games/  Scorch v1.56 (Tomasz Pecko, Pawel Kalinowski et al.;
#                     the Unlicense, covering the whole repository: code,
#                     artwork, music; github.com/pkali/scorch_src, tag v1.56 =
#                     commit ea08663), the release's scorch.xex: the M4 exit
#                     game (docs/systems/atari800xl.md, "Exit game").
# Hashes recorded on 2026-10-04.
O=roms/a800xl/os
A8=https://raw.githubusercontent.com/atari800/atari800/d722329d374e1bc5744b5a59d5f0b627138e44e8/src/roms
fetch $O "$A8/altirraos_xl.c"   altirraos_xl.c   6347888c649c9c08abc2c6f3150bc33c7e38c90fee3370a39596eec89ef23258
fetch $O "$A8/altirra_basic.c"  altirra_basic.c  ce5d391d22dcb062e786c8587dacef93060b58777ae493719667b20766f4a438
c_array() { # file.c out.rom sha256: the bytes of the C array's initialiser
    if [[ ! -f $2 ]]; then
        echo "fetch-roms: $2 (from $(basename "$1"))"
        python3 - "$1" "$2.part" <<'PY'
import re, sys
text = open(sys.argv[1]).read()
body = text[text.index("{", text.index("ROM_")):]
body = body[:body.index("}")]
open(sys.argv[2], "wb").write(bytes(int(h, 16) for h in re.findall(r"0x([0-9a-fA-F]{2})", body)))
PY
        mv "$2.part" "$2"
    fi
    if ! echo "$3  $2" | sha256sum --quiet -c -; then
        echo "fetch-roms: $2: SHA-256 mismatch (delete it to re-make it)" >&2
        exit 1
    fi
}
c_array $O/altirraos_xl.c  $O/altirraos_xl.rom  9de5a313fe3946f04fe236a8d3ceacb471fbed4ec5fc5db009732e1169946ccf
c_array $O/altirra_basic.c $O/altirra_basic.rom 19fd64377895eb5d4414319855322dd3f860c21fdc1df9adc0e98ab72c540913
AC=roms/a800xl/acid800
fetch $AC https://www.virtualdub.org/downloads/Acid800-1.2.7z Acid800-1.2.7z 4074e6a40db1833e97603e334ca130f5923bff34a0d4f8a83e2ecf9617de1b20
while read -r sum name; do
    if [[ ! -f $AC/$name ]]; then
        command -v 7z >/dev/null || { echo "fetch-roms: 7z missing (run inside nix develop)" >&2; exit 1; }
        7z e -y -bso0 -bsp0 -o"$AC" "$AC/Acid800-1.2.7z" "out/Release/Acid800/standalone/$name"
    fi
    if ! echo "$sum  $AC/$name" | sha256sum --quiet -c -; then
        echo "fetch-roms: $AC/$name: SHA-256 mismatch" >&2
        exit 1
    fi
done <<'EOF'
18374382d66aa6a608057254f6ad00c7b32474f9db182c696e96135952ed6a9e  antic_addresswrap.xex
cab04637069a50c8ed890f1b05ab82783abac5a1d0e8d15bf9321c592df7f1b6  antic_addrmirror.xex
bbbedebbdef7d240b54313d1ce412a261ee5e20e284d9194ce3e404c60ac22ea  antic_blockednmi.xex
7c5e3c6d1230c0b1259433a476398ded193edd7d368f8a34033042e6fa20500b  antic_charcontrol.xex
d84577082dc185bdfb51e8ce6c3c4c6ac236829ad29b59a0a57d7c357477b064  antic_default.xex
3aaf3e2564ff68c0844407613d7a157ea908c8d40d291ce877f0c1df8dd3d731  antic_dlistwrap.xex
58f2a1933f6935ede19fa3ebcc38b645ba4d099bcb254c2ae88313b029eae7cc  antic_dlitiming.xex
7d725c67470094dfd9de97151d2a0d5f165c4fa97b800413e75eba5ee282f806  antic_dmapattern.xex
380e30264b28e5022997ad7c32f6bb7b1219a68351ae32a5eed52b6cc67f72f6  antic_hiresbug.xex
caadb21cb841b0a1fa843b8ae82e36a8777745891ac4ea2bfd309ef0a3d81753  antic_hscrolbug.xex
8853225aee4f686b37b2c94cbb16f0b39e087897d414157e720528ecb82f5d82  antic_linebuffering.xex
435f96791d4e4a233b429154a0701723ec44532cacbd1cc363b88fb2a0174f95  antic_nmist.xex
0c2b78c7008d2c286bc1f65cc64017c2320e8b295bd2248b7c7a44b4c1967cc8  antic_pfstarttiming.xex
2626671baa380ae04679c0d5cb779a480b25ad3aa64ff728ca5c8941aacac679  antic_pfstoptiming.xex
50e73d720378452f8034a3e48f95aed207125e8ecff5106094ebd5307bd77ec0  antic_pmdma.xex
567e61c97191651793f8729f7931c71c643152f09af79e0d6dc76f0ff3a1e329  antic_vcount.xex
a13fc357193683360c8c0c1fbd0eb2a016800549cd31070ca6a821b29e26354c  antic_virtdma.xex
b90e5c4ff85e0039e2a94d0f8b934f33eb35652aba394f148a87cead23c208d9  antic_vscroldli.xex
b16aca83f914b429a8504c3282f55940308aeb4a747d430ba8a74123694a0a93  antic_vscroll.xex
ef40b822126c72329345c5621dc2bc7c9a2837e41cf56b619a9207ab912df932  antic_wsync.xex
6279dd1f5a74d7e92b6e74941831e442d1ab140ebb63b6afe41700e00979c5ec  cpu_bugs.xex
875a091147eb886a640aeab37f4e807825b0a379495f15dfdb335d82ccd64e5f  cpu_clisei.xex
a17e5288193c1ea8be1e4c8903eaa5b8c68ff452cae3db5d850c69f4b1dc26a2  cpu_decimal.xex
6938fc4378282f1c2684c7a8d25beb06dabeb5b92b00dea6d7af4035c4f9be1c  cpu_flags.xex
392fafc1604db0d1f738a4c9e5f76d86f93757e86cf329cafdaab25ef8bc6a55  cpu_illegal.xex
453d4ad0bfaea73fdfd85b13e4a94a05c57fcaa313ebfd233bf80aa4e207a906  cpu_illtiming.xex
e22353f7117362065b70ad75c9f091ae2c71246c7f5d3859abf3d2d82fed3d87  cpu_insn.xex
42db99d986c4b46f191e83748637e5c0fd665d4238a49247e37a241f2829cc3a  cpu_timing.xex
268251153ce22a432e84c1ca8b95efab98fcee2c0267b10b80cb22f0e0106190  gtia_addrmirror.xex
1249620214e01570a608cc423bf2616c4b6bc249e38042033879205d86752185  gtia_collision2.xex
cf1c9d692c48298094807fd26ea6831adbf3abcb7b5486f553f9ec8ff16672e9  gtia_collision.xex
dcfaca206977ba89c34b9e1f9936fe405b007e6942a451f6331bca3e39523655  gtia_consol.xex
4323246a144e01125b5add1390ffbc58d7d83fd70636505e65470c7a321274be  gtia_default.xex
9e20d36086398ecc03cd708c8dac9751db14b3f4036184893b986f96568751b2  gtia_phantomdma.xex
bbe8c4c08034f0c625e1c357b302a3a289cb5fb9b9fd58442fa95ff86017f23e  gtia_pmoverlap.xex
1aa407b44177510b78167ab4c07fb2339b2f00f4b91ecd2e930e49109a0de0d6  gtia_pmresize.xex
a72e9a5e6c8471df5fc3716d0cb6cf5480f434e5e0a28ed944488276fbcc47a2  gtia_pmretrigger.xex
36f0bb09b7b1946fe627cb6c963ecdef610f335e13f57f75ef682271ea8b30f1  gtia_psuedomodee.xex
c116dff4050b335bc66adef5e3dc982e0ff702c6a5097a9b74bd5a2f6eeaf7ba  gtia_vdelay.xex
a792159eee07aa2ed3c16db73b13207cfe17384303902dc4abcbb0522e7a45ca  mmu_xlbanking.xex
01f21f8afc306af3bb3268dc214c7a39fe143f0368435df1a2da5d61fb930640  pia_basic.xex
d8ced59aac83296fc2fa6fddef63517d654402cd5e49f21bc4c04eb4e7fa7877  pia_irq.xex
378f55e7b2db9f6150cca6fc00a21a9eb5b606b0279807885bce213fb469f679  pokey_addrmirror.xex
1ffd73a922f7f24c03e793419b4096caac2ebd62f5db00214381ab99e704e3ba  pokey_asyncrecv.xex
e935508338ca86cc5aeef43e452463c34a2c33b5fe8392e16b7e95d066c072a4  pokey_default.xex
28311db3707d72a18ceda795d15716f312c3044b8a19853afc544defac1738d6  pokey_inittiming.xex
c25ec478aec17b1f01cf887f7f3874b8e1c3817c10d528519176c07cc4e9a992  pokey_irqtiming.xex
55bfdf2b11d902ebe58035621a8e90b1445b7cbb33e0071ba296eadba3284e5b  pokey_noise.xex
a94e54706b54e277389cc6454b491b9c89a7dc591e98a0394577f32a861374f5  pokey_serclock.xex
856c42150bf90ff6205e41d1ce637453d0c86fcf8dd7938c2ff5c18a2d7a1c04  pokey_seroc.xex
ceab67da1059a72f6345de1449db1a755fec8f201f2d23f9560bcf405086c93c  pokey_sertiming.xex
a636510adf70ca0346cf99a71b7344a3ca6e57a0e193780796859df8b3da6b3e  pokey_skstat.xex
895465deff593816d63e4a97370fce05a2666b86c1554c2c9130bb2a4258e651  pokey_timergranularity.xex
72a1a4008e18e999bbaf10a5e456c0a91801047dd40770e508102e166e868628  pokey_timerirq.xex
899ff8aef476b78144f6087f5ba6d1dfc38705063be5d958dce6e70c9d394bda  pokey_timertiming.xex
a3614357c5e28bdc57c96ef4c82d8f3df32d9f229455e08b76090ca70ff4fe3d  pokey_twotone.xex
EOF
fetch roms/a800xl/games https://github.com/pkali/scorch_src/releases/download/v1.56/scorch.xex scorch.xex 6ff204ca7933229a4af6258f12377701e13b84075aa984ad432bce4c9a09a850


# --- The Super Nintendo, the 65C816 and the SPC700 (milestone 7) ------------
# docs/systems/snes.md, "Test ROMs". Hashes recorded 2026-10-15.
# Files from an archive found by name anywhere in it (7z's -r), through a
# fresh temporary directory as unpack() above; a missing one stops here.
snes_unpack() { # archive dir file...
    local a=$1 d=$2 t f
    shift 2
    command -v 7z >/dev/null || { echo "fetch-roms: 7z missing (run inside nix develop)" >&2; exit 1; }
    mkdir -p "$d"
    t=$(mktemp -d) || exit 1
    7z e -y -bso0 -bsp0 -r -o"$t" "$a" "$@" || { rm -rf "$t"; exit 1; }
    for f in "$@"; do
        [[ -f $t/$f ]] || { echo "fetch-roms: $a: no $f in it" >&2; rm -rf "$t"; exit 1; }
        mv -f "$t/$f" "$d/$f"
    done
    rm -rf "$t"
}
# roms/65816/v1/      Tom Harte's SingleStepTests (MIT), github.com/
#                     SingleStepTests/65816 at commit 9bf6b70: all 512 opcode
#                     files (e = emulation, n = native mode; 10000 tests each,
#                     ~2.8 GB, fetched once). SHA-256 per file in
#                     tools/roms-65816.sha256.
W=https://raw.githubusercontent.com/SingleStepTests/65816/9bf6b708bd8203073fbfbfc54f0aaed8bdda331d
while read -r sum path; do
    fetch roms/65816/v1 "$W/$path" "${path#v1/}" "$sum"
done < tools/roms-65816.sha256
# roms/spc700/v1/     SingleStepTests (MIT), github.com/SingleStepTests/spc700
#                     at commit 67d15f4: all 256 opcode files (1000 tests each,
#                     ~84 MB; the repository's simple_py_serve.py is left out). SHA-256 per file in tools/roms-spc700.sha256.
P7=https://raw.githubusercontent.com/SingleStepTests/spc700/67d15f492b2740964abd4efd1229e0ec9c342228
while read -r sum path; do
    fetch roms/spc700/v1 "$P7/$path" "${path#v1/}" "$sum"
done < tools/roms-spc700.sha256
# roms/snes/gilyon/   gilyon's snes-tests v1.4 (MIT, the zip's LICENSE;
#                     github.com/gilyon/snes-tests): cputest-full/-basic.sfc
#                     (every 65C816 opcode but STP/WAI), spctest.sfc (every
#                     SPC700 opcode but SLEEP/STOP); "Success" on screen. The
#                     tests-*.txt lists name each test's inputs and outputs.
D=roms/snes/gilyon
fetch $D https://github.com/gilyon/snes-tests/releases/download/v1.4/snes-tests.zip snes-tests-v1.4.zip c3a39e15a9f3789fe143e54dfa035c988204576eafacf97df8ff3546a917c5e9
[[ -f $D/spctest.sfc ]] || snes_unpack $D/snes-tests-v1.4.zip $D cputest-full.sfc cputest-basic.sfc spctest.sfc tests-full.txt tests-basic.txt tests.txt LICENSE
# roms/snes/krom/     krom's (Peter Lemon's) SNES tests, github.com/PeterLemon/
#                     SNES at commit 350b394: every CPUTest (65C816 and SPC700
#                     groups, "PASS"/"FAIL" on screen) and a pick of the PPU
#                     demos (goldens by eye). No licence in the repository:
#                     fetched for local tests, never redistributed (as
#                     nestest).
D=roms/snes/krom
P=https://raw.githubusercontent.com/PeterLemon/SNES/350b394e86ec5d62f600b5cbf64cdce3721bb6ef
fetch $D "$P/CPUTest/CPU/ADC/CPUADC.sfc" CPUADC.sfc d6c4f5a284ce7842b8a3944b2bc51c8cde2f5932305f9b561334f4b7f49b5b4c
fetch $D "$P/CPUTest/CPU/AND/CPUAND.sfc" CPUAND.sfc b7438031625af35e86f2c1725a7157358525ffc83ef1f15c02e3654e52ac61c6
fetch $D "$P/CPUTest/CPU/ASL/CPUASL.sfc" CPUASL.sfc 0abb736aaaf683a183355e712ccb98d170bb25e1e13db456b2c21d8be4de480f
fetch $D "$P/CPUTest/CPU/BIT/CPUBIT.sfc" CPUBIT.sfc fdfc621bcd1f9dfcc0800b331d540af69ca70dd7e075632b144b46d9583fa15e
fetch $D "$P/CPUTest/CPU/BRA/CPUBRA.sfc" CPUBRA.sfc 1101903669864e5917f6d1f427ee24ca95c481fd4566125576e0f5a247f0cb25
fetch $D "$P/CPUTest/CPU/CMP/CPUCMP.sfc" CPUCMP.sfc b24c5e15301a0b68806157af642b39d4d38c5dfdc335eb44ddcec0d9e0222672
fetch $D "$P/CPUTest/CPU/DEC/CPUDEC.sfc" CPUDEC.sfc d0d50a863b2b3dc6d5c14aad134c1b47dbd29bdc9af37ce34d68b552f3a3dee9
fetch $D "$P/CPUTest/CPU/EOR/CPUEOR.sfc" CPUEOR.sfc 4db10b39d39125dcb83cddd5f8ad329c95653eb1d3e266db7a58a1ad1c97a2bd
fetch $D "$P/CPUTest/CPU/INC/CPUINC.sfc" CPUINC.sfc d32d4a8128d07970e4d652d907b2fcb51ea2d79d311d5f4a452c357644167b18
fetch $D "$P/CPUTest/CPU/JMP/CPUJMP.sfc" CPUJMP.sfc 8be0c153f166e11d7d384cffffe5ae27ea9c1a57eb4a1b36a85759bab2fe59a3
fetch $D "$P/CPUTest/CPU/LDR/CPULDR.sfc" CPULDR.sfc 4598006dd6ec9ce03fa6622e18178ef25a6d471926ca40abb8748c10006e92da
fetch $D "$P/CPUTest/CPU/LSR/CPULSR.sfc" CPULSR.sfc 01517b0bd134d93fe858d536f76ea36691b23b10462b19caef693c8b67c6b60d
fetch $D "$P/CPUTest/CPU/MOV/CPUMOV.sfc" CPUMOV.sfc 98ae96bdc5035fe9074e5b7c2dc0a8da67cb94f3d9c5668f26f936d9f99b6ab5
fetch $D "$P/CPUTest/CPU/MSC/CPUMSC.sfc" CPUMSC.sfc 00fa9244fe5a086042c4ca8e9f425892dbf115b135d7a14630c88dee3fd18490
fetch $D "$P/CPUTest/CPU/ORA/CPUORA.sfc" CPUORA.sfc e9e4fdd107faa16ec43c13756a465b02f72a576b79e6b00869931535e3922379
fetch $D "$P/CPUTest/CPU/PHL/CPUPHL.sfc" CPUPHL.sfc 27bb4a9e21d1e1021a7287cd48c612c87888ca83b4e30323c22d24ccb0764f76
fetch $D "$P/CPUTest/CPU/PSR/CPUPSR.sfc" CPUPSR.sfc bd85187c7da1006b7c7111a8921b9b13cc73043a89eb7353f3d4fff207273067
fetch $D "$P/CPUTest/CPU/RET/CPURET.sfc" CPURET.sfc 0f65834dae85684202643e648166c37c8ea7364a295992c92e99506d65c7247e
fetch $D "$P/CPUTest/CPU/ROL/CPUROL.sfc" CPUROL.sfc 2b9c67c63befa2d0419c9a9f9b7fe2963fbe3cf84a0663f0bd36107c5aa0b255
fetch $D "$P/CPUTest/CPU/ROR/CPUROR.sfc" CPUROR.sfc ff73f1dd8c2dd8605f8fbc09ff4fda8312741cd382258ec09518763056792fec
fetch $D "$P/CPUTest/CPU/SBC/CPUSBC.sfc" CPUSBC.sfc b9a688dcfcf9316227de98c305a260af35c80d0a0fd48bd227a21051159634a1
fetch $D "$P/CPUTest/CPU/STR/CPUSTR.sfc" CPUSTR.sfc f8bd66164c369d956145c747d71ce59c252693a735cc644a34521a599cf6847a
fetch $D "$P/CPUTest/CPU/TRN/CPUTRN.sfc" CPUTRN.sfc 60776a7bfbdf3f1f0e57885e885c66b9c0ebfa7866f18fece8f8cefd82221ad1
fetch $D "$P/CPUTest/SPC700/ADC/SPC700ADC.sfc" SPC700ADC.sfc 4fa785418a109c0ef052cb9b71f55d37ee56834fca170fe1a0da784ee3df0e2e
fetch $D "$P/CPUTest/SPC700/AND/SPC700AND.sfc" SPC700AND.sfc a4e7a9d6a926f5b575b2c3006b301bf837ed36072c9617776aab527706c3242d
fetch $D "$P/CPUTest/SPC700/DEC/SPC700DEC.sfc" SPC700DEC.sfc fe9254720e62b2aa953a42f50827facc12f7549ad8ca3775c89a5b65f4b55fdf
fetch $D "$P/CPUTest/SPC700/EOR/SPC700EOR.sfc" SPC700EOR.sfc 061572e2ba5599f78c3573be3b538ba2ab67703529f7518eee8e8ce3e41372f1
fetch $D "$P/CPUTest/SPC700/INC/SPC700INC.sfc" SPC700INC.sfc 467376b5ab3cf83197553ec598f91645beef628dadbd43932b6e96d2fa17776b
fetch $D "$P/CPUTest/SPC700/ORA/SPC700ORA.sfc" SPC700ORA.sfc aefb416d0f681eed0975cfbee70fd3048f59154cd5502f550e48e80f789a0ab2
fetch $D "$P/CPUTest/SPC700/SBC/SPC700SBC.sfc" SPC700SBC.sfc fcc6017db0560ce12ae029fa97947eedbd8eaf35dd32200c445281ca94cbae9b
fetch $D "$P/PPU/BGMAP/8x8/2BPP/8x8BG1Map2BPP32x328PAL/8x8BG1Map2BPP32x328PAL.sfc" 8x8BG1Map2BPP32x328PAL.sfc 66083b119131d4cd7b3c41301750342152d2c3a3cfce103de074a0ae53330146
fetch $D "$P/PPU/BGMAP/8x8/2BPP/8x8BG2Map2BPP32x328PAL/8x8BG2Map2BPP32x328PAL.sfc" 8x8BG2Map2BPP32x328PAL.sfc 048fbb0ad29fa6821a127c10a51f8dc3bbc136ef0b678f1bd7b76ea5b6e2c8da
fetch $D "$P/PPU/BGMAP/8x8/2BPP/8x8BG3Map2BPP32x328PAL/8x8BG3Map2BPP32x328PAL.sfc" 8x8BG3Map2BPP32x328PAL.sfc 4fa8e6c48e0a3251088ae810fbd7a7022f52427837660669b05e66daf09c3494
fetch $D "$P/PPU/BGMAP/8x8/2BPP/8x8BG4Map2BPP32x328PAL/8x8BG4Map2BPP32x328PAL.sfc" 8x8BG4Map2BPP32x328PAL.sfc 18e4d47a265aa3b787e57a6ffc2789cd5cace453e963aa5bc0932ee0c674e6cc
fetch $D "$P/PPU/BGMAP/8x8/4BPP/8x8BGMap4BPP32x328PAL/8x8BGMap4BPP32x328PAL.sfc" 8x8BGMap4BPP32x328PAL.sfc 60d4c21e53ee731c15af870eb903fc13220dcbc9dac677ded1355c0b62e428f3
fetch $D "$P/PPU/BGMAP/8x8/8BPP/32x32/8x8BGMap8BPP32x32.sfc" 8x8BGMap8BPP32x32.sfc 238d33f7ec2a24b73e3553dffeb300121c72df48e553eb0988a1e278e3a0af2d
fetch $D "$P/PPU/BGMAP/8x8/8BPP/TileFlip/8x8BGMapTileFlip.sfc" 8x8BGMapTileFlip.sfc 6107053b6b47baf036332897518a8f49ea57470cad3f6492e4ae763e61163b41
fetch $D "$P/PPU/GreenSpace/GreenSpace.sfc" GreenSpace.sfc e3933942eb205f601d96ad9b4cb55d1de4bc44304a038f1ae77686a082ca9196
fetch $D "$P/PPU/HDMA/HiColor64PerTileRow/HiColor64PerTileRow.sfc" HiColor64PerTileRow.sfc 3a45a7ebba613094a0a22a530d71d092a90ad13ea7f6cec92ac71a47ad036bde
fetch $D "$P/PPU/HDMA/HiColor64PerTileRowPseudoHiRes/HiColor64PerTileRowPseudoHiRes.sfc" HiColor64PerTileRowPseudoHiRes.sfc 01dace4535e9352aba93ea688e8d1c4f769482823b0dca0fda6606cbf3e51729
fetch $D "$P/PPU/HDMA/Mode7HDMA/Mode7HDMA.sfc" Mode7HDMA.sfc e60a09ec6e5d57c1adc22b2f84fe24ca9259b60e0d3f7d986bd809151980543c
fetch $D "$P/PPU/HDMA/RedSpace9BitHDMA/RedSpace9BitHDMA.sfc" RedSpace9BitHDMA.sfc ad06e07d45dc82d744d6d9fbe0a9b7897f3386fb475168c163824ee0997c1e44
fetch $D "$P/PPU/HDMA/RedSpaceHDMA/RedSpaceHDMA.sfc" RedSpaceHDMA.sfc f59bc2cc89b22cad7308fe65f5d9fee1025311c84a9f809e14ebf96ab9a4c4de
fetch $D "$P/PPU/HDMA/RedSpaceIndirectHDMA/RedSpaceIndirectHDMA.sfc" RedSpaceIndirectHDMA.sfc bfc85c6c05399512e33c4ec5393e90eba6a4465bdcdb6c3f2624d2f7c2cf90ff
fetch $D "$P/PPU/HDMA/WaveHDMA/WaveHDMA.sfc" WaveHDMA.sfc b9ecef8a41628d74900cbe6334ae24680dcd6e5526292f3e68aa39be34518b31
fetch $D "$P/PPU/Interlace/InterlaceFont/InterlaceFont.sfc" InterlaceFont.sfc 097e4c79fbf351c8e8b74c73ce3d60752ec5f8691185f46737aac786f45dd36c
fetch $D "$P/PPU/Interlace/InterlaceScroll/InterlaceScroll.sfc" InterlaceScroll.sfc 98dcf13d979775afb49b8585845c0cdfd024c6744406978d231d2bd8c865de59
fetch $D "$P/PPU/Mode7/RotZoom/RotZoom.sfc" RotZoom.sfc d5580870e152046ca838fa276568e10c671be63b1db0285030a53dc7864cc4c0
fetch $D "$P/PPU/Mosaic/Mode3/MosaicMode3.sfc" MosaicMode3.sfc 6deebf7ddbe46392d900914dd69e6a060d868e9cc7436a486987c1fb868885db
fetch $D "$P/PPU/Mosaic/Mode5/MosaicMode5.sfc" MosaicMode5.sfc da0197247fa6c19f076351a2110608ad248e10d199310191c69e0a91b6fa339c
fetch $D "$P/PPU/Rings/Rings.sfc" Rings.sfc 9622cef24adb23093c8b66bbcf807ffb00ceb48ebfd02fa62b49130cdbc1e556
fetch $D "$P/PPU/Window/WindowHDMA/WindowHDMA.sfc" WindowHDMA.sfc 27594315a409d11f0182838201901aedeaaf825e986a0c9999111f08cb83ffbf
fetch $D "$P/PPU/Window/WindowMultiHDMA/WindowMultiHDMA.sfc" WindowMultiHDMA.sfc 1bacb0c050a42c8ba3a53268f1bf74cd8889234ca904e195490adf3bf682f363
# roms/snes/opensnes/ OpenSNES v0.50.0's example ROMs (MIT, the zip's LICENSE;
#                     github.com/k0b3n4irb/opensnes; ATTRIBUTION.md: the
#                     shmup's art is Kenney's Pixel Shmup, CC0): shmup_1942
#                     (the exit game, chosen 2026-10-15; no sound), save_game
#                     (8 KiB battery SRAM: the save check), hirom_demo, and
#                     the PPU and DSP feature demos used as goldens.
D=roms/snes/opensnes
# The release's zip was replaced upstream after it was first pinned (found
# 2026-10-18 on a fresh machine): the two differ only in one example's
# README.md and main.c, all 88 ROMs byte for byte the same, so either is
# accepted: a copy already here with the first pin, else a fresh download
# with the second.
Z=$D/opensnes-examples_v0.50.0.zip
if ! { [[ -f $Z ]] && echo "6fff846977c00b53b73b9a10694dbbf1a1ebff12f472cb0338faabf0bd59fed7  $Z" | sha256sum --quiet -c - 2>/dev/null; }; then
    fetch $D https://github.com/k0b3n4irb/opensnes/releases/download/v0.50.0/opensnes-examples_v0.50.0.zip opensnes-examples_v0.50.0.zip a130d63150da6a61a65b27fecf91449948dea8be0e3ede30fbe0c7dd09442dff
fi
[[ -f $D/tetris.sfc ]] || snes_unpack $D/opensnes-examples_v0.50.0.zip $D LICENSE ATTRIBUTION.md \
    shmup_1942.sfc save_game.sfc hirom_demo.sfc breakout.sfc mode7_racing.sfc \
    mode0.sfc mode1.sfc mode2.sfc mode3.sfc mode4.sfc mode5_hires.sfc mode6.sfc \
    mosaic.sfc window.sfc transparency.sfc direct_color.sfc pseudo_hires.sfc extbg.sfc \
    two_players.sfc controller.sfc echo.sfc play_noise.sfc \
    simple_sprite.sfc sprite_sizes.sfc metasprite.sfc sprite_swarm.sfc animated_sprite.sfc \
    transparent_window.sfc window_multi_hdma.sfc hdma_wave.sfc gradient_colors.sfc \
    hdma_indirect_gradient.sfc mode1_bg3_priority.sfc parallax_scroll.sfc mixed_scroll.sfc \
    continuous_scroll.sfc fading.sfc text_glyphs.sfc hicolor_1792.sfc shadow_tint.sfc \
    palette_cycle.sfc perspective.sfc rotate_scale.sfc perspective_rotate.sfc mode7_flying.sfc \
    tetris.sfc print_string.sfc
# the examples' reference screenshots (the OpenSNES authors', 256 x 224),
# for comparing the model's frames by eye and by a match score (step 7.5)
[[ -f $D/ref/shmup_1942.png ]] || python3 tools/snes-refs.py $D/opensnes-examples_v0.50.0.zip $D/ref
# roms/snes/games/    unnamed-snes-game tech demo alpha v3 (Marcus Rowe,
#                     github.com/undisbeliever/unnamed-snes-engine; code MIT,
#                     art and music CC BY-SA 4.0, samples per CREDITS.md):
#                     LoROM with music and sound effects. horizontal-shooter
#                     v1.1 (Marcus Rowe, github.com/undisbeliever/horizontal-
#                     shooter; code MIT, images CC0): the HiROM check.
D=roms/snes/games
fetch $D https://github.com/undisbeliever/unnamed-snes-engine/releases/download/tech-demo-alpha-v3/unnamed-tech-demo-v3.zip unnamed-tech-demo-v3.zip f495993b7b6decbb8675bff05d3ea2004a7ffb95f8c86da81d6620973c3608bd
fetch $D https://github.com/undisbeliever/horizontal-shooter/releases/download/v1.1/horizontal-shooter-v1.1.zip horizontal-shooter-v1.1.zip 43833a8dcd0116a7462d3d1751534ac9301ed7c358b733fdaf406b44ceeb596b
if [[ ! -f $D/unnamed-tech-demo-v3.sfc ]]; then
    snes_unpack $D/unnamed-tech-demo-v3.zip $D/unnamed unnamed-tech-demo-v3.sfc LICENSE CREDITS.md
    mv $D/unnamed/unnamed-tech-demo-v3.sfc $D/ && mv $D/unnamed/LICENSE $D/LICENSE.unnamed-tech-demo && mv $D/unnamed/CREDITS.md $D/CREDITS.unnamed-tech-demo.md && rmdir $D/unnamed
fi
if [[ ! -f $D/horizontal-shooter.sfc ]]; then
    snes_unpack $D/horizontal-shooter-v1.1.zip $D/hs horizontal-shooter.sfc LICENSE
    mv $D/hs/horizontal-shooter.sfc $D/ && mv $D/hs/LICENSE $D/LICENSE.horizontal-shooter && rmdir $D/hs
fi

# --- The shared 68000 (side track P) ------------------------------------------
# roms/m68k/v1/       Tom Harte's SingleStepTests (MIT), github.com/
#                     SingleStepTests/680x0 at commit e0d5ece: the 68000's
#                     124 files (one per instruction and size, 8065 cases
#                     each, gzipped JSON, ~194 MB, kept gzipped: cpus/m68k
#                     reads them through zlib). SHA-256 per file in
#                     tools/roms-m68k.sha256 (recorded 2026-10-18).
P=https://raw.githubusercontent.com/SingleStepTests/680x0/e0d5ece9670205cc84a0101081837deb446f86a3/68000
while read -r sum path; do
    name=${path#v1/}
    fetch roms/m68k/v1 "$P/v1/${name// /%20}" "$name" "$sum"
done < tools/roms-m68k.sha256

echo "fetch-roms: OK"
