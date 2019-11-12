from nmigen import *
from nmigen_boards.icebreaker import *

# boneless CPU architecture stuff
from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *

import pmod_resources
from hub75 import PanelDescription, GammaParameters, FramebufferedHUB75Driver
from pll import PLL
import uart


class BonelessLED(Elaboratable):
    def __init__(self, panel_desc, led_domain="sync"):
        self.pd = panel_desc
        self.gp = GammaParameters(gamma=2.5, bpp=8) 

        self.panel = FramebufferedHUB75Driver(self.pd, led_domain=led_domain,
           gamma_params=self.gp)

        self.cpu_rom = Memory(width=16, depth=512,
            init=Instr.assemble(firmware(self.gp.bpp)))
        self.cpu_core = CoreFSM(alsru_cls=ALSRU_4LUT, memory=self.cpu_rom)

        self.uart = uart.SimpleUART(
            default_divisor=uart.calculate_divisor(12e6, 115200))

    def elaborate(self, platform):
        platform.add_resources(pmod_resources.hub75_pmod)

        uart_pins = platform.request("uart")

        m = Module()
        m.submodules.panel = panel = self.panel
        m.submodules.cpu_core = cpu_core = self.cpu_core
        m.submodules.uart = uart = self.uart

        # split up the external bus
        uart_en = Signal()
        panel_en = Signal()
        m.d.comb += [
            uart_en.eq(cpu_core.o_bus_addr[-1] == 0),
            panel_en.eq(cpu_core.o_bus_addr[-1] == 1),
        ]

        # hook up the CPU's external bus to the panel engine
        m.d.sync += [
            panel.i_we.eq(cpu_core.o_ext_we & panel_en),
            panel.i_waddr.eq(cpu_core.o_bus_addr[:self.pd.chan_bits]),
            panel.i_wdata.eq(cpu_core.o_ext_data[:self.gp.bpp])
        ]

        # then to the UART
        m.d.comb += [
            uart.i_re.eq(cpu_core.o_ext_re & uart_en),
            uart.i_we.eq(cpu_core.o_ext_we & uart_en),
            uart.i_addr.eq(cpu_core.o_bus_addr[:2]),
            uart.i_wdata.eq(cpu_core.o_ext_data),
            cpu_core.i_ext_data.eq(uart.o_rdata),
        ]

        m.d.comb += [
            uart.i_rx.eq(uart_pins.rx),
            uart_pins.tx.eq(uart.o_tx),
        ]

        return m

def firmware(bpp):
    # time between color changes
    # 1 second of clocks / (4 clocks per insn * 3 delay loop insns)
    period = 1#120000//(4*3)
    curr_color = R7
    curr_font_data = R6
    curr_msg_ptr = R5
    curr_fb_ptr = R4
    ch_pixels_remaining = R3
    proc_ptr = R2
    temp1 = R0
    temp2 = R1

    def fw_uart():
        curr_char = R6
        curr_msg_ptr = R5
        temp1 = R4
        temp2 = R3

        return [
            # "helloworld" or "delloworld", who knows?
            NOP(0), NOP(0),
            MOVI(curr_msg_ptr, 0),
        L("sendch"),
            LDR(curr_char, curr_msg_ptr, "message"),
        L("busy"), # loop while the UART doesn't have space for another char
            LDXA(temp1, 2),
            ANDI(temp1, temp1, 0x8000),
            BNZ("busy"),
            ADDI(curr_char, curr_char, ord('d')),
            STXA(curr_char, 2),
            ADDI(curr_msg_ptr, curr_msg_ptr, 1),
            CMPI(curr_msg_ptr, 10),
            BNE("sendch"),
        ]

    fw = [fw_uart(),
        MOVI(curr_color, 0),
    L("display_msg"),
        # draw first half of message on top half of screen
        MOVI(curr_msg_ptr, 0),
        MOVI(curr_fb_ptr, 0x8000), # 0 is top left
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
        MOVI(curr_fb_ptr, 0x8000+(32*8*4)),
        # replace load offset with 5 to get second half of message
        MOVR(temp1, "msg_ch_load_insn"),
        LD(temp2, temp1, 0),
        ORI(temp2, temp2, 5), # set offset to 5
        ST(temp2, temp1, 0),
        JAL(proc_ptr, "display_msg_row"),

    L("busy2"), # loop while the UART doesn't have space for another char
        LDXA(temp1, 2),
        ANDI(temp1, temp1, 0x8000),
        BNZ("busy2"),
        MOVI(temp1, ord("C")),
        STXA(temp1, 2),

        # wait some time for the message to show
        MOVI(temp1, period&0xffff),
        MOVI(temp2, (period>>16)+1),


    L("delay"),
        SUBI(temp1, temp1, 1),
        SBCI(temp2, temp2, 0),
        BNZ ("delay"),

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
        LDR(curr_font_data, temp1, "fontdata"),
        # font is six pixels wide
        MOVI(ch_pixels_remaining, 6),
    L("display_ch_row"),
        MOVI(temp2, 0), # assume this pixel is off
        ANDI(temp1, curr_font_data, 0x80), # but maybe it's on?
        BZ("no_pix"), # skip drawing 0 pixels to avoid filling up FIFO
         # oh wait it is. ramp color up and down all cool-like
        ANDI(temp2, curr_color, 0xFF),
        ANDI(temp1, curr_color, 0x100),
        BZ("display_ch_row_pix_off"),
        SUBI(temp1, temp1, 1),
        SUB(temp2, temp1, temp2),
    L("display_ch_row_pix_off"),
        # write the intensity as white to the frame buffer
        STX(temp2, curr_fb_ptr, 0), # word 0 is red
        STX(temp2, curr_fb_ptr, 1), # word 1 is green
        STX(temp2, curr_fb_ptr, 2), # word 2 is blue
    L("no_pix"),
        # then move to the next pixel
        ADDI(curr_fb_ptr, curr_fb_ptr, 4),
        SLLI(curr_font_data, curr_font_data, 1),
        # done with this character?
        SUBI(ch_pixels_remaining, ch_pixels_remaining, 1),
        BNZ("display_ch_row"),
        # move to next character
        ADDI(curr_msg_ptr, curr_msg_ptr, 1),
        CMPI(curr_msg_ptr, 5), # done with this message half?
        BNZ("display_msg_row"),
        # we only draw 6*5=30 out of 32 pixels, thus skip the last 2 so we start
        # at the next row.
        ADDI(curr_fb_ptr, curr_fb_ptr, (32-(6*5))*4),
        MOVI(curr_msg_ptr, 0), # start from first character on next row
        ANDI(temp1, curr_fb_ptr, 7<<7), # done with all the rows?
        BNZ("display_msg_row"),
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
    def __init__(self, panel_desc, led_freq_mhz=12):
        self.pd = panel_desc
        self.led_freq_mhz = led_freq_mhz

    def elaborate(self, platform):
        m = Module()
        if self.led_freq_mhz != 12:
            # we need a PLL so we can boost the clock. reserve the clock pin
            # before it gets switched to the default domain.
            clk_pin = platform.request(platform.default_clk, dir="-")
            # then create the PLL
            pll = PLL(12, self.led_freq_mhz, clk_pin,
                orig_domain_name="cpu", # runs at 12MHz
                pll_domain_name="led", # runs at the LED frequency
            )
            m.submodules.pll = pll
            led_domain = "led"
        else:
            # the user doesn't want to run faster and the PLL can't make
            # input = output, so just create the CPU domain using the default
            # clock and run the LEDs in that domain too.
            cpu = ClockDomain("cpu")
            m.domains += cpu
            m.d.comb += ClockSignal("cpu").eq(ClockSignal("sync"))
            led_domain = "cpu"

        # create the actual demo and tell it to run in the domain we made
        # for it above
        boneless_led = BonelessLED(self.pd, led_domain=led_domain)

        # remap the default sync domain to the CPU domain, since most logic
        # should run there.
        boneless_led = DomainRenamer("cpu")(boneless_led)

        m.submodules.boneless_led = boneless_led

        return m

panel_desc = PanelDescription(width=32, height=16, bpp=10)

if __name__ == "__main__":
    from cli import main
    def make(simulating):
        design = Top(panel_desc=panel_desc,
            # we can't simulate with different LED and CPU frequencies
            led_freq_mhz=(12 if simulating else 40))
        platform = ICEBreakerPlatform()
        return design, platform

    main(maker=make, build_args={"synth_opts": "-abc9"})
