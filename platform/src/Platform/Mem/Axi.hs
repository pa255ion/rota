{-|
The read half of AXI3 as the Zynq's high-performance (HP) slave ports
take it (side track C; Zynq-7000 TRM, UG585, "PL AXI interfaces"): 32-bit
addresses, 64-bit data, bursts of 1 to 16 beats. The read address channel
(AR) and the read data channel (R); and (side track O, O.4: screenshots
into DDR) the write channels, address (AW), data (W) and response (B).
A top without a writer has its write channels tied off by the board
wrapper.

A channel's VALID is the 'Just' of a 'Maybe'; READY travels the other way.
The AR fields that never change ('arSize', 'arBurst', 'arCache', ...) are
constants, wired to the PS7's ports in the board wrapper, not carried in
the records.

AXI rules a master has to keep (AMBA AXI3 spec, A3.2 and A3.3), checked
for 'Platform.Mem.Ddr' in its tests: once AR is valid it stays valid with
the same payload until ARREADY; VALID never waits for READY; a burst never
crosses a 4 KiB boundary. The same for AW and W (and a burst's W beats
number AWLEN + 1, the last with WLAST), checked for
'Platform.Video.Capture'.
-}
module Platform.Mem.Axi
  ( AxiAddr
  , AxiData
  , AxiAr (..)
  , AxiR (..)
  , AxiRdM2S (..)
  , AxiRdS2M (..)
  , axiRdM2SIdle
  , axiRdS2MIdle
  , respOkay
    -- * Writing (side track O, O.4)
  , AxiAw (..)
  , AxiW (..)
  , AxiWrM2S (..)
  , AxiWrS2M (..)
  , axiWrM2SIdle
  , axiWrS2MIdle
    -- * The fixed AR fields
  , arSize
  , arBurst
  , arCache
  , arProt
  , arLock
  , arQos
  , arId
  ) where

import Clash.Prelude

-- | A byte address in the PS's memory map: the ZedBoard's 512 MiB of DDR
-- are at 0x00000000 to 0x1FFFFFFF (UG585, "System addresses", which also
-- lists the low 1 MiB's exceptions; our buffers sit above it).
type AxiAddr = Unsigned 32

-- | One beat: 8 bytes, byte 0 in bits 7 to 0 (little-endian, as the PS).
type AxiData = BitVector 64

-- | The read address channel's payload: the first byte's address (aligned
-- to 8) and the beats minus one.
data AxiAr = AxiAr
  { arAddr :: AxiAddr
  , arLen  :: Unsigned 4
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | One beat of read data.
data AxiR = AxiR
  { rData :: AxiData
  , rResp :: BitVector 2   -- ^ 0 OKAY, 2 SLVERR, 3 DECERR
  , rLast :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Master to slave: ARVALID with its payload, and RREADY.
data AxiRdM2S = AxiRdM2S
  { m2sAr     :: Maybe AxiAr
  , m2sRReady :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Slave to master: ARREADY, and RVALID with its payload.
data AxiRdS2M = AxiRdS2M
  { s2mArReady :: Bool
  , s2mR       :: Maybe AxiR
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

axiRdM2SIdle :: AxiRdM2S
axiRdM2SIdle = AxiRdM2S Nothing False

axiRdS2MIdle :: AxiRdS2M
axiRdS2MIdle = AxiRdS2M False Nothing

-- | The write address: AWSIZE, AWBURST and the rest are the read side's
-- constants ('arSize', 'arBurst', ...), wired in the board wrapper.
data AxiAw = AxiAw
  { awAddr :: AxiAddr
  , awLen  :: Unsigned 4
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data AxiW = AxiW
  { wData :: AxiData
  , wStrb :: BitVector 8   -- ^ bit i: byte i of 'wData' is written
  , wLast :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data AxiWrM2S = AxiWrM2S
  { m2sAw     :: Maybe AxiAw
  , m2sW      :: Maybe AxiW
  , m2sBReady :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data AxiWrS2M = AxiWrS2M
  { s2mAwReady :: Bool
  , s2mWReady  :: Bool
  , s2mB       :: Maybe (BitVector 2)  -- ^ BVALID and BRESP
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

axiWrM2SIdle :: AxiWrM2S
axiWrM2SIdle = AxiWrM2S Nothing Nothing True

axiWrS2MIdle :: AxiWrS2M
axiWrS2MIdle = AxiWrS2M False False Nothing

respOkay :: BitVector 2
respOkay = 0

-- | ARSIZE: 8 bytes per beat (the HP port's full width).
arSize :: BitVector 3
arSize = 3

-- | ARBURST: INCR.
arBurst :: BitVector 2
arBurst = 1

-- | ARCACHE: normal, non-cacheable, bufferable (what Xilinx's own masters
-- send; the HP ports aren't coherent with the CPU caches either way).
arCache :: BitVector 4
arCache = 0b0011

-- | ARPROT: data, secure, unprivileged.
arProt :: BitVector 3
arProt = 0

-- | ARLOCK: normal access.
arLock :: BitVector 2
arLock = 0

-- | ARQOS: lowest priority.
arQos :: BitVector 4
arQos = 0

-- | ARID: one ID, so the data comes back in order.
arId :: BitVector 6
arId = 0
