from nmigen import *
from nmigen_boards.icebreaker import *
import pmod_resources
from hub75 import *

# boneless CPU architecture stuff
from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *

class BonelessLED(Elaboratable):
    def __init__(self, panel_shape):
        # panel shape: physical (width, height) in LEDs
        # i.e. width is how many pixels to shift out per row
        # and height is 2**(addr_bits)/2 (assuming two rows are driven at once)
        self.panel_shape = panel_shape

        self.ftg = FrameTimingGenerator(panel_shape)
        # there's one buffer per panel pixel input, so 6 for 2xRGB
        self.pixel_buffers = {}
        self.pixel_buffer_names = ("r0", "g0", "b0", "r1", "g1", "b1")
        for buffer in self.pixel_buffer_names:
            self.pixel_buffers[buffer] = PixelBuffer(panel_shape)

        self.cpu_rom = Memory(width=16, depth=256,
            init=Instr.assemble(firmware()))
        self.cpu_core = CoreFSM(alsru_cls=ALSRU_4LUT, memory=self.cpu_rom)

    def elaborate(self, platform):
        platform.add_resources(pmod_resources.hub75_pmod)

        m = Module()
        m.submodules.ftg = ftg = self.ftg
        m.submodules.cpu_core = cpu_core = self.cpu_core
        pixel_buffers = self.pixel_buffers
        for buffer_name, buffer in pixel_buffers.items():
            setattr(m.submodules, buffer_name, buffer)

        # use DDR buffer on shift clock so we can cleanly gate it
        pmod = platform.request("hub75", 0, xdr={"shift_clock": 2})

        # rename the signals from the display into nicer names
        p_rgb0 = Cat(pmod.r0, pmod.g0, pmod.b0)
        p_rgb1 = Cat(pmod.r1, pmod.g1, pmod.b1)

        p_line_addr = Cat(getattr(pmod, "a"+str(n)) for n in range(5))

        p_shift_clock = pmod.shift_clock
        p_latch = pmod.latch
        p_blank = pmod.blank

        # buffer panel outputs by one clock
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
        f_allrgb = Cat(f_rgb0, f_rgb1)

        # attach the pixel buffers to the timing generator
        for i, buffer_name in enumerate(self.pixel_buffer_names):
            buffer = pixel_buffers[buffer_name]
            m.d.comb += [
                buffer.i_row.eq(ftg.o_row),
                buffer.i_col.eq(ftg.o_col),
                f_allrgb[i].eq(buffer.o_pixel),
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

        # decode the CPU's external memory bus
        # 0x0000-0x7FFF is the framebuffer
        # low 2 bits select R, G, B or nothing
        # then all the bits to select a column
        # then all the bits to select a row
        # then a bit to select top or bottom half
        # then repeating until the end of the memory

        # add a delay of one clock so the CPU doesn't have to wait for us to
        # decode
        cpu_ext_we = Signal.like(cpu_core.o_ext_we)
        cpu_ext_waddr = Signal.like(cpu_core.o_bus_addr)
        cpu_ext_wdata = Signal.like(cpu_core.o_ext_data)
        m.d.sync += [
            cpu_ext_we.eq(cpu_core.o_ext_we),
            cpu_ext_waddr.eq(cpu_core.o_bus_addr),
            cpu_ext_wdata.eq(cpu_core.o_ext_data),
        ]

        row_bits = (self.panel_shape[1]//2-1).bit_length()
        col_bits = (self.panel_shape[0]-1).bit_length()
        pixel_bits = row_bits + col_bits + 1

        # split apart CPU memory bus as above
        is_fb = Signal()
        fb_color = Signal(2)
        fb_row = Signal(row_bits)
        fb_col = Signal(col_bits)
        fb_half = Signal()
        m.d.comb += [
            is_fb.eq(~cpu_ext_waddr[-1]),
            fb_color.eq(cpu_ext_waddr[:2]),
            fb_col.eq(cpu_ext_waddr[2:2+col_bits]),
            fb_row.eq(cpu_ext_waddr[2+col_bits:2+col_bits+row_bits]),
            fb_half.eq(cpu_ext_waddr[2+col_bits+row_bits]),
        ]

        # then recombine into the pixel buffer addresses
        pb_waddr = Signal(pixel_bits)
        pb_wdata = Signal(8)
        m.d.comb += [
            pb_waddr.eq(Cat(fb_col, fb_row)),
            pb_wdata.eq(cpu_ext_wdata[:8]),
        ]
        # now route the addresses to all the pixel buffers, and determine which
        # should be enabled when
        for name, buffer in self.pixel_buffers.items():
            color_en = Signal() # correct pixel?
            half_en = Signal() # correct display half?
            m.d.comb += [
                color_en.eq(fb_color == "rgb".index(name[0])),
                half_en.eq(fb_half == "01".index(name[1])),

                buffer.i_we.eq(color_en & half_en & cpu_ext_we),
                buffer.i_waddr.eq(pb_waddr),
                buffer.i_wdata.eq(pb_wdata),
            ]

        # hook up the CPU's external bus to our decoder
        m.d.sync += [
            cpu_ext_we.eq(cpu_core.o_ext_we),
            cpu_ext_waddr.eq(cpu_core.o_bus_addr),
            cpu_ext_wdata.eq(cpu_core.o_ext_data)
        ]

        return m

def firmware():
    period = 600000//(4*3) # 10 pixels per second?
    return [
        MOVI(R7, 0),
        MOVI(R6, 1),
    L("poke"),
        STX(R6, R7, 0),
        STX(R6, R7, 1),
        STX(R6, R7, 2),
        ADDI(R7, R7, 4),
        MOVI(R1, period&0xffff),
        MOVI(R2, (period>>16)+1),
    L("loop"),
        SUBI(R1, R1, 1),
        SBBI(R2, R2, 0),
        JNZ ("loop"),
        J   ("poke"),
    ]


if __name__ == "__main__":
    design = BonelessLED(panel_shape=(32, 16))
    ICEBreakerPlatform().build(design, do_program=True)

# if __name__ == "__main__":
#     from nmigen.cli import main
#     design = IceBlink(panel_shape=(32, 16))
#     main(design, platform=ICEBreakerPlatform(),
#         ports=[v for k, v in design.__dict__.items() if k.startswith("p_") ])