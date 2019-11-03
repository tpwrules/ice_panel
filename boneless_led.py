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
    # time between color changes
    # 1 second of clocks / (4 clocks per insn * 3 delay loop insns)
    period = 12000000//(4*3)
    curr_color = R7
    curr_font_data = R6
    curr_msg_ptr = R5
    curr_fb_ptr = R4
    ch_pixels_remaining = R3
    proc_ptr = R2
    temp1 = R0
    temp2 = R1
    fw = [
        MOVI(curr_color, 1),
    L("display_msg"),
        # draw first half of message on top half of screen
        MOVI(curr_msg_ptr, 0),
        MOVI(curr_fb_ptr, 0), # 0 is top left
        # we don't have a free register to keep track of where we are in the
        # message independently of where we are in the half message. we just
        # modify the offset of the message characger load instruction to switch
        # halves.
        MOVR(temp1, "msg_ch_load_insn"),
        LD(temp2, temp1, 0),
        ANDI(temp2, temp2, 0x1f ^ 0xFFFF), # clear offset to 0
        ST(temp2, temp1, 0),
        # then go and display that half
        JAL(proc_ptr, "display_msg_row"),

        # now the bottom row
        MOVI(curr_msg_ptr, 0),
        MOVI(curr_fb_ptr, (32*8*4)),
        # replace load offset with 5 to get second half of message
        MOVR(temp1, "msg_ch_load_insn"),
        LD(temp2, temp1, 0),
        ORI(temp2, temp2, 5), # set offset to 5
        ST(temp2, temp1, 0),
        JAL(proc_ptr, "display_msg_row"),

        # wait some time for the message to show
        MOVI(temp1, period&0xffff),
        MOVI(temp2, (period>>16)+1),
    L("delay"),
        SUBI(temp1, temp1, 1),
        SBBI(temp2, temp2, 0),
        JNZ ("delay"),

        # then switch colors and do it again
        ADDI(curr_color, curr_color, 1),
        J   ("display_msg"),

    L("display_msg_row"),
        # we write to the display one row at a time to save index pointers.
        # row 0 of character 0, row 0 of character 1, ..., row 1 of character 0,
        # row 1 of character 1, ..., row 7 of character 4.

        # in the framebuffer, for a 32x16 panel, there are 4 color words * 32
        # columns * 16 rows. 

        # load the current character of the message
        MOVR(temp1, "message"),
        ADD(temp1, temp1, curr_msg_ptr),
    L("msg_ch_load_insn"),
        LD(temp1, temp1, 0), # offset gets modified!!!
        # then the corresponding row of the font bitmap
        SLLI(temp1, temp1, 3), # low 3 bits are row, remaining are character
        ANDI(temp2, curr_fb_ptr, 7<<7), # pull row out of framebuffer pointer
        SRLI(temp2, temp2, 7),
        OR(temp1, temp1, temp2),
        MOVR(temp2, "fontdata"),
        ADD(temp1, temp1, temp2),
        LD(curr_font_data, temp1, 0),
        # font is six pixels wide
        MOVI(ch_pixels_remaining, 6),
    L("display_ch_row"),
        MOVI(temp2, 0), # assume this pixel is off
        ANDI(temp1, curr_font_data, 0x80), # but maybe it's on?
        JZ("display_ch_row_pix_off"), # nah
        ANDI(temp2, curr_color, curr_color), # oh wait it is
    L("display_ch_row_pix_off"),
        # pull out the colors and write them to the framebuffer
        STX(temp2, curr_fb_ptr, 0), # bit 0 is red
        SRLI(temp2, temp2, 1),
        STX(temp2, curr_fb_ptr, 1), # bit 1 is green
        SRLI(temp2, temp2, 1),
        STX(temp2, curr_fb_ptr, 2), # bit 2 is blue
        # then move to the next pixel
        ADDI(curr_fb_ptr, curr_fb_ptr, 4),
        SLLI(curr_font_data, curr_font_data, 1),
        # done with this character?
        SUBI(ch_pixels_remaining, ch_pixels_remaining, 1),
        JNZ("display_ch_row"),
        # move to next character
        ADDI(curr_msg_ptr, curr_msg_ptr, 1),
        CMPI(curr_msg_ptr, 5), # done with this message half?
        JNZ("display_msg_row"),
        # we only draw 6*5=30 out of 32 pixels, thus skip the last 2 so we start
        # at the next row.
        ADDI(curr_fb_ptr, curr_fb_ptr, (32-(6*5))*4),
        MOVI(curr_msg_ptr, 0), # start from first character on next row
        ANDI(temp1, curr_fb_ptr, 7<<7), # done with all the rows?
        JNZ("display_msg_row"),
        JR(proc_ptr, 0),
    ]
    # add the data as exti instructions since i don't immediately know
    # how to do a .data or something in this mode
    fw.append(L("message"))
    for m in "helloworld":
        fw.append(EXTI(ord(m)-ord('d')))

    fw.append(L("fontdata"))

    from font import tft_font
    # the font bitmap is 6 pixels wide and 8 pixels high. bit 7 is the leftmost
    # bit and bit 2 is the rightmost.

    # we don't have enough free memory to hold a through z, so just store the
    # character range needed for "hello world".
    for ch in range(ord('d'), ord('w')+1):
        for row in range(8):
            fw.append(EXTI(tft_font[ch*8+row]))

    return fw


if __name__ == "__main__":
    design = BonelessLED(panel_shape=(32, 16))
    ICEBreakerPlatform().build(design, do_program=True)

# if __name__ == "__main__":
#     from nmigen.cli import main
#     design = BonelessLED(panel_shape=(32, 16))
#     main(design, platform=ICEBreakerPlatform(),
#         ports=[v for k, v in design.__dict__.items() if k.startswith("p_") ])