from nmigen import *
from nmigen_boards.icebreaker import *
import pmod_resources
from hub75 import BufferedHUB75
from pll import PLL

# boneless CPU architecture stuff
from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *

class BonelessLED(Elaboratable):
    def __init__(self, panel_shape, led_domain="sync", bpp=1):
        # panel shape: physical (width, height) in LEDs
        # i.e. width is how many pixels to shift out per row
        # and height is 2**(addr_bits)/2 (assuming two rows are driven at once)
        self.panel_shape = panel_shape

        self.bpp = bpp # number of bits per color per pixel
        if bpp < 1:
            raise Exception(
                "who has a {}-color display?".format(2**bpp))
        elif bpp > 16:
            raise Exception("{}bpp? now that's just greedy".format(bpp))

        self.panel = BufferedHUB75(panel_shape, led_domain=led_domain, bpp=bpp)

        self.cpu_rom = Memory(width=16, depth=256,
            init=Instr.assemble(firmware(bpp)))
        self.cpu_core = CoreFSM(alsru_cls=ALSRU_4LUT, memory=self.cpu_rom)

    def elaborate(self, platform):
        platform.add_resources(pmod_resources.hub75_pmod)

        m = Module()
        m.submodules.panel = panel = self.panel
        m.submodules.cpu_core = cpu_core = self.cpu_core

        # hook up the CPU's external bus to the panel engine
        m.d.sync += [
            panel.i_we.eq(cpu_core.o_ext_we),
            panel.i_waddr.eq(cpu_core.o_bus_addr[:panel.addr_bits]),
            panel.i_wdata.eq(cpu_core.o_ext_data[:self.bpp])
        ]

        return m

def firmware(bpp):
    # time between color changes
    # 1 second of clocks / (4 clocks per insn * 3 delay loop insns)
    period = 1200//(4*3)
    curr_color = R7
    curr_font_data = R6
    curr_msg_ptr = R5
    curr_fb_ptr = R4
    ch_pixels_remaining = R3
    proc_ptr = R2
    temp1 = R0
    temp2 = R1
    fw = [
        MOVI(curr_color, 0),
    L("display_msg"),
        # draw first half of message on top half of screen
        MOVI(curr_msg_ptr, 0),
        MOVI(curr_fb_ptr, 0), # 0 is top left
        # we don't have a free register to keep track of where we are in the
        # message independently of where we are in the half message. we just
        # modify the offset of the message character load instruction to switch
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
        AND(temp2, curr_color, curr_color), # oh wait it is
    L("display_ch_row_pix_off"),
        # pull out the colors and write them to the framebuffer
        STX(temp2, curr_fb_ptr, 0),
        SRLI(temp2, temp2, bpp),
        STX(temp2, curr_fb_ptr, 1), # bit 1 is green
        SRLI(temp2, temp2, bpp),
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
    # adding data just consists of appending it to the instruction stream
    fw.append(L("message"))
    for m in "helloworld":
        fw.append(ord(m)-ord('d'))

    fw.append(L("fontdata"))

    from font import tft_font
    # the font bitmap is 6 pixels wide and 8 pixels high. bit 7 is the leftmost
    # bit and bit 2 is the rightmost.

    # we don't have enough free memory to hold a through z, so just store the
    # character range needed for "hello world".
    for ch in range(ord('d'), ord('w')+1):
        for row in range(8):
            fw.append(tft_font[ch*8+row])

    return fw

# super top domain to manage clock stuff
class Top(Elaboratable):
    def __init__(self, panel_shape, led_freq_mhz=12, bpp=1):
        self.panel_shape = panel_shape
        self.led_freq_mhz = led_freq_mhz
        self.bpp = bpp

    def elaborate(self, platform):
        # reserve the clock pin before it gets switched to the default domain
        clk_pin = platform.request(platform.default_clk, dir="-")

        m = Module()
        if self.led_freq_mhz != 12:
            # create the PLL to run the LED engine faster than the cpu and stuff
            pll = PLL(12, self.led_freq_mhz, clk_pin,
                orig_domain_name="cpu", # runs at 12MHz
                pll_domain_name="led", # runs at the LED frequency
            )
            m.submodules.pll = pll
            led_domain = "led"
        else:
            # the user doesn't want to run faster and the PLL can't make
            # input = output, so just create the CPU domain using the original
            # clock and run the LEDs in that domain
            cpu = ClockDomain("cpu")
            m.domains += cpu
            m.d.comb += ClockSignal("cpu").eq(clk_pin)
            led_domain = "cpu"

        # create the actual demo and tell it to run in the domain we made
        # for it above
        boneless_led = BonelessLED(self.panel_shape,
            led_domain=led_domain, bpp=self.bpp)

        # remap the default sync domain to the CPU domain, since most logic
        # should run there.
        boneless_led = DomainRenamer("cpu")(boneless_led)

        m.submodules.boneless_led = boneless_led

        return m


if __name__ == "__main__":
    design = Top(panel_shape=(32, 16), led_freq_mhz=40, bpp=11)
    ICEBreakerPlatform().build(design, do_program=True)

# if __name__ == "__main__":
#     from nmigen.cli import main
#     design = Top(panel_shape=(32, 16), led_freq_mhz=12, bpp=9)
#     main(design, platform=ICEBreakerPlatform(),
#         ports=[v for k, v in design.__dict__.items() if k.startswith("p_") ])