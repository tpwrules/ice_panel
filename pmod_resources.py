# (some of) the PMODs attachable to the icebreaker

from nmigen import *
from nmigen.build import *

# HUB75 breakout. connect to PMOD1A and PMOD1B
hub75_pmod = [
    Resource("hub75", 0,
        # RGB colors for each half of the display. plus a fourth channel which
        # should just be grounded.
        Subsignal("r0", Pins("1", dir="o", conn=("pmod", 0)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("g0", Pins("2", dir="o", conn=("pmod", 0)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("b0", Pins("3", dir="o", conn=("pmod", 0)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("x0", Pins("4", dir="o", conn=("pmod", 0)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("r1", Pins("7", dir="o", conn=("pmod", 0)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("g1", Pins("8", dir="o", conn=("pmod", 0)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("b1", Pins("9", dir="o", conn=("pmod", 0)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("x1", Pins("10", dir="o", conn=("pmod", 0)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        
        # address selection pins (which row of the panel to drive)
        Subsignal("a0", Pins("1", dir="o", conn=("pmod", 1)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("a1", Pins("2", dir="o", conn=("pmod", 1)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("a2", Pins("3", dir="o", conn=("pmod", 1)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("a3", Pins("4", dir="o", conn=("pmod", 1)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        Subsignal("a4", Pins("10", dir="o", conn=("pmod", 1)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),

        # color shift clock
        Subsignal("shift_clock", Pins("9", dir="o", conn=("pmod", 1)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        # latch new colors into row drivers
        Subsignal("latch", Pins("8", dir="o", conn=("pmod", 1)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
        # blank the display (turn off all LEDs)
        Subsignal("blank", Pins("7", dir="o", conn=("pmod", 1)),
            Attrs(IO_STANDARD="SB_LVCMOS33")),
    )
]
