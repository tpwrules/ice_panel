from nmigen import *
from nmigen_boards.icebreaker import *
from hub75 import UpCounter
from pll import PLL

class PLLTest(Elaboratable):
    def __init__(self):
        pass

    def elaborate(self, platform):
        platform.add_resources(platform.break_off_pmod)

        m = Module()

        clk_pin = platform.request(platform.default_clk, dir="-")
        pll = PLL(12, 48, clk_pin,
            pll_domain_name="clk48", orig_domain_name="clk12")
        m.submodules.pll = pll

        led_48_counter = DomainRenamer("clk48")(UpCounter(26))
        led_12_counter = DomainRenamer("clk12")(UpCounter(24))
        led_hf = platform.request("led_g", 1)
        led_lf = platform.request("led_g", 2)
        led_lock = platform.request("led_r", 1)
        m.d.comb += led_hf.eq(led_48_counter.value[-1])
        m.d.comb += led_lf.eq(led_12_counter.value[-1])

        m.d.comb += led_lock.eq(pll.pll_lock)

        m.submodules.led_48_counter = led_48_counter
        m.submodules.led_12_counter = led_12_counter

        return m


if __name__ == "__main__":
    design = PLLTest()
    ICEBreakerPlatform().build(design, do_program=True)
