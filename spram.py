# nmigen module to use ice40 SPRAM

from nmigen import *

class SPRAM(Elaboratable):
    def __init__(self):
        self.i_addr = Signal(14)
        self.i_data = Signal(16)
        self.i_we = Signal()
        self.i_re = Signal()
        self.o_data = Signal(16)

    def elaborate(self, platform):
        m = Module()

        # convert re/we to wren/cs
        wren = Signal()
        cs = Signal()
        m.d.comb += [
            wren.eq(self.i_we & ~self.i_re),
            cs.eq(self.i_re | self.i_we),
        ]
        m.submodules.spram = Instance("SB_SPRAM256KA",
            i_ADDRESS=self.i_addr,
            i_DATAIN=self.i_data,
            i_MASKWREN=Const(0b1111),
            i_WREN=wren,
            i_CHIPSELECT=cs,
            o_DATAOUT=self.o_data,

            i_CLOCK=ClockSignal(),
            i_STANDBY=Const(0),
            i_SLEEP=Const(0),
            i_POWEROFF=Const(1),
        )

        return m
