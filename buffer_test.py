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

        self.ftg = FrameTimingGenerator(panel_shape)
        self.pbr0 = PixelBuffer(panel_shape)

    def elaborate(self, platform):
        platform.add_resources(pmod_resources.hub75_pmod)

        m = Module()
        m.submodules.ftg = ftg = self.ftg
        m.submodules.pbr0 = pbr0 = self.pbr0

        # shift clock is DDR so we can cleanly gate it
        pmod = platform.request("hub75", 0, xdr={"shift_clock": 2})

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

        m.d.sync += [
            p_rgb0.eq(b_rgb0),
            p_rgb1.eq(b_rgb1),
            p_line_addr.eq(b_line_addr),
            p_latch.eq(b_latch),
            p_blank.eq(b_blank),
        ]

        # and give the panel a clock. we use DDR so we can cleanly gate it.
        # instead of 1 -> 0 it goes shift_active -> 0, so that there's no high
        # period if shift_active is deasserted. note that due to the latch in
        # the pin, we don't use a delayed shift_active. note also that the clock
        # is inverted so that the shift clock falls as the fpga clock rises so
        # there's half a clock for the data to make it to the panel.
        m.d.comb += [
            p_shift_clock.o_clk.eq(ClockSignal()),
            p_shift_clock.o0.eq(0),
            p_shift_clock.o1.eq(b_shift_active),
        ]

        # for simulation, effective shift clock relative to b signals
        b_shift_clk = Signal()
        m.d.comb += [
            b_shift_clk.eq(b_shift_active & ~ClockSignal())
        ]

        f_rgb0 = Signal(3)
        f_rgb1 = Signal(3)

        # attach the pixel buffers to the timing generator
        m.d.comb += [
            pbr0.i_row.eq(ftg.o_row),
            pbr0.i_col.eq(ftg.o_col),
            f_rgb0[0].eq(pbr0.o_pixel),
        ]

        # wire up the timing generator
        m.d.comb += [
            ftg.i_rgb0.eq(f_rgb0),
            ftg.i_rgb1.eq(f_rgb1),
            b_shift_active.eq(ftg.o_shift_active),
            b_latch.eq(ftg.o_latch),
            b_blank.eq(ftg.o_blank),
            b_line_addr.eq(Cat(ftg.o_line_addr, 0)),
            b_rgb0.eq(ftg.o_rgb0),
            b_rgb1.eq(ftg.o_rgb1),
        ]

        return m

if __name__ == "__main__":
    design = IceBlink(panel_shape=(32, 16))
    ICEBreakerPlatform().build(design, do_program=True)

# if __name__ == "__main__":
#     from nmigen.cli import main
#     design = IceBlink(panel_shape=(32, 16))
#     main(design, platform=ICEBreakerPlatform(),
#         ports=[v for k, v in design.__dict__.items() if k.startswith("p_") ])