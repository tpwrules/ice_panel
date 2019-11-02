from nmigen import *
from nmigen_boards.icebreaker import *
import pmod_resources
from hub75 import *


class IceBlink(Elaboratable):
    def __init__(self, panel_shape):
        # panel shape: physical (width, height) in LEDs
        # i.e. width is how many pixels to shift out per row
        # and height is 2**(addr_bits)/2 (assuming two rows are driven at once)
        self.panel_shape = panel_shape

        self.ltg = LineTimingGenerator(panel_shape)

        self.line_addr = Signal((panel_shape[1]//2).bit_length())

    def elaborate(self, platform):
        platform.add_resources(pmod_resources.hub75_pmod)

        m = Module()
        m.submodules.ltg = ltg = self.ltg

        pmod = platform.request("hub75", 0)

        # rename the signals from the display into nicer names
        p_rgb0 = Cat(pmod.r0, pmod.g0, pmod.b0)
        p_rgb1 = Cat(pmod.r1, pmod.g1, pmod.b1)

        p_line_addr = Cat(getattr(pmod, "a"+str(n)) for n in range(5))

        p_shift_clock = pmod.shift_clock
        p_latch = pmod.latch
        p_blank = pmod.blank

        # buffer panel outputs by one stage
        b_rgb0 = Signal.like(p_rgb0)
        b_rgb1 = Signal.like(p_rgb1)
        b_line_addr = Signal.like(p_line_addr)
        b_latch = Signal.like(p_latch)
        b_blank = Signal.like(p_blank)
        b_shift_active = Signal()
        p_shift_active = Signal()

        m.d.sync += [
            p_rgb0.eq(b_rgb0),
            p_rgb1.eq(b_rgb1),
            p_line_addr.eq(b_line_addr),
            p_latch.eq(b_latch),
            p_blank.eq(b_blank),
            p_shift_active.eq(b_shift_active)
        ]

        # and give the panel a clock. we invert it so there is time for the
        # stuff on the wires to settle etc. and so that switching shift_active
        # doesn't glitch.
        plat_clk = ClockSignal()
        m.d.comb += [
            p_shift_clock.eq(p_shift_active & plat_clk)
        ]

        # set some initial test defaults for the panel
        m.d.comb += [
            b_rgb0.eq(1), b_rgb1.eq(2),
            self.line_addr.eq(0),
        ]

        # wire up the timing generator
        m.d.comb += [
            ltg.i_idle.eq(0),
            ltg.i_line_addr.eq(self.line_addr),
            b_shift_active.eq(ltg.o_shift_active),
            b_latch.eq(ltg.o_latch),
            b_blank.eq(ltg.o_blank),
            b_line_addr.eq(Cat(0, 0, ltg.o_line_addr))
        ]

        return m

if __name__ == "__main__":
    design = IceBlink(panel_shape=(32, 16))
    ICEBreakerPlatform().build(design, do_program=True)
