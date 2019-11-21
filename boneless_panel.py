from nmigen import *
from nmigen_boards.icebreaker import *

# boneless CPU architecture stuff
from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *

import boneload

import pmod_resources
from hub75 import PanelDescription, GammaParameters, FramebufferedHUB75Driver
from pll import PLL
from spram import SPRAM
import uart
import spi
import multiplier


class BonelessLED(Elaboratable):
    def __init__(self, panel_desc, led_domain="sync"):
        self.pd = panel_desc
        self.gp = GammaParameters(gamma=2.5, bpp=8) 

        self.panel = FramebufferedHUB75Driver(self.pd, led_domain=led_domain,
           gamma_params=self.gp)

        self.cpu_rom = Memory(width=16, depth=512,
            init=boneload.boneload_fw(uart_addr=0, spi_addr=16))
        self.cpu_ram = SPRAM()
        self.cpu_core = CoreFSM(alsru_cls=ALSRU_4LUT,
            reset_pc=0xFE00, reset_w=0xFFF8)

        self.uart = uart.SimpleUART(
            default_divisor=uart.calculate_divisor(12e6, 115200))
        self.spi = spi.SimpleSPI(fifo_depth=512)
        self.mul = multiplier.Multiplier(signed=False)

    def elaborate(self, platform):
        uart_pins = platform.request("uart")
        spi_pins = platform.request("spi_flash_1x")

        m = Module()
        m.submodules.panel = panel = self.panel
        m.submodules.cpu_core = cpu_core = self.cpu_core
        m.submodules.cpu_rom_r = cpu_rom_r = self.cpu_rom.read_port(
            transparent=False)
        m.submodules.cpu_rom_w = cpu_rom_w = self.cpu_rom.write_port()
        m.submodules.cpu_ram = cpu_ram = self.cpu_ram
        m.submodules.uart = uart = self.uart
        m.submodules.spi = spi = self.spi
        m.submodules.mul = mul = self.mul

        # split up main bus
        rom_en = Signal()
        ram_en = Signal()
        m.d.comb += [
            rom_en.eq(cpu_core.o_bus_addr[-1] == 1),
            ram_en.eq(cpu_core.o_bus_addr[-1] == 0),
        ]
        # we need to know who was enabled one cycle later so we can route the
        # read result back correctly.
        rom_was_en = Signal()
        ram_was_en = Signal()
        m.d.sync += [
            rom_was_en.eq(rom_en),
            ram_was_en.eq(ram_en),
        ]
        m.d.comb += [
            # address bus to the memories
            cpu_rom_r.addr.eq(cpu_core.o_bus_addr),
            cpu_rom_w.addr.eq(cpu_core.o_bus_addr),
            cpu_ram.i_addr.eq(cpu_core.o_bus_addr),
            # write data to memories
            cpu_rom_w.data.eq(cpu_core.o_mem_data),
            cpu_ram.i_data.eq(cpu_core.o_mem_data),
            # selects to memories
            cpu_rom_r.en.eq(rom_en & cpu_core.o_mem_re),
            cpu_rom_w.en.eq(rom_en & cpu_core.o_mem_we),
            cpu_ram.i_re.eq(ram_en & cpu_core.o_mem_re),
            cpu_ram.i_we.eq(ram_en & cpu_core.o_mem_we),
        ]
        # read results back to cpu
        with m.If(rom_was_en):
            m.d.comb += cpu_core.i_mem_data.eq(cpu_rom_r.data)
        with m.Elif(ram_was_en):
            m.d.comb += cpu_core.i_mem_data.eq(cpu_ram.o_data)


        # split up the external bus into 8 regions of 16 registers. this way,
        # all of them can be addressed absolutely. of course, the panel
        # framebuffer is special and gets the entire top half of the address
        # space to itself.
        periph_en = Signal(8)
        panel_en = Signal()
        ext_addr = cpu_core.o_bus_addr
        ext_old_addr = Signal(16)
        for x in range(8):
            m.d.comb += periph_en[x].eq(
                (ext_addr[-1] == 0) & (ext_addr[4:7] == x))
        m.d.comb += panel_en.eq(ext_addr[-1] == 1)
        m.d.sync += ext_old_addr.eq(ext_addr)

        periph_was_en = Signal(8)
        # remember what was active so we can redirect reads appropriately. the
        # framebuffer can't be read from so we ignore it here.
        m.d.sync += periph_was_en.eq(periph_en)

        # hook up the CPU's external bus to the panel engine
        m.d.sync += [
            panel.i_we.eq(cpu_core.o_ext_we & panel_en),
            panel.i_waddr.eq(cpu_core.o_bus_addr[:self.pd.chan_bits]),
            panel.i_wdata.eq(cpu_core.o_ext_data[:self.gp.bpp])
        ]

        # plus the UART as peripheral 0
        uart_en = periph_en[0]
        uart_was_en = periph_was_en[0]
        m.d.comb += [
            # peripheral to bus
            uart.i_re.eq(cpu_core.o_ext_re & uart_en),
            uart.i_we.eq(cpu_core.o_ext_we & uart_en),
            uart.i_addr.eq(ext_addr[:2]),
            uart.i_wdata.eq(cpu_core.o_ext_data),
            # peripheral to outside world
            uart.i_rx.eq(uart_pins.rx),
            uart_pins.tx.eq(uart.o_tx),
        ]
        with m.If(uart_was_en):
            m.d.comb += cpu_core.i_ext_data.eq(uart.o_rdata)

        # plus the SPI flash as peripheral 1
        spi_en = periph_en[1]
        spi_was_en = periph_was_en[1]
        m.d.comb += [
            # peripheral to bus
            spi.i_re.eq(cpu_core.o_ext_re & spi_en),
            spi.i_we.eq(cpu_core.o_ext_we & spi_en),
            spi.i_addr.eq(ext_addr[:1]),
            spi.i_wdata.eq(cpu_core.o_ext_data),
            # peripheral to outside world
            spi_pins.clk.eq(spi.o_clk),
            spi_pins.cs.eq(~spi.o_cs),
            spi_pins.mosi.eq(spi.o_mosi),
            spi.i_miso.eq(spi_pins.miso),
        ]
        with m.If(spi_was_en):
            m.d.comb += cpu_core.i_ext_data.eq(spi.o_rdata)

        # plus the multiplier as peripheral 2
        mul_en = periph_en[2]
        mul_was_en = periph_was_en[2]
        m.d.comb += [
            # connect bus to both multipler inputs
            mul.i_a.eq(cpu_core.o_ext_data),
            mul.i_b.eq(cpu_core.o_ext_data),
            # load the input corresponding to the address
            mul.i_a_we.eq(mul_en & cpu_core.o_ext_we & (ext_addr[0] == 0)),
            mul.i_b_we.eq(mul_en & cpu_core.o_ext_we & (ext_addr[0] == 1)),
        ]
        # register output. no idea about the latency. but we have minimum four
        # cycles between a read and a write to a peripheral.
        mul_out = Signal(32)
        m.d.sync += mul_out.eq(mul.o_result)
        # allow the user to get the result at any shift amount
        with m.If(mul_was_en):
            with m.Switch(ext_old_addr[:4]):
                for x in range(16):
                    with m.Case(x):
                        m.d.comb += cpu_core.i_ext_data.eq(mul_out>>x)

        return m


# super top domain to manage clock stuff
class Top(Elaboratable):
    def __init__(self, panel_desc, led_freq_mhz=12):
        self.pd = panel_desc
        self.led_freq_mhz = led_freq_mhz

    def elaborate(self, platform):
        platform.add_resources(pmod_resources.hub75_pmod)

        m = Module()
        reset_btn = platform.request("button", 0) # should be the user button
        # the button on the front of the purse
        reset_btn_2 = platform.request("front_button", 0)
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
            m.d.comb += pll.reset.eq(~reset_btn & ~reset_btn_2)
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
