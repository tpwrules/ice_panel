from nmigen import *
from nmigen_boards.icebreaker import *
from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *


class BonelessDemo(Elaboratable):
    def __init__(self, firmware=[]):
        self.memory = Memory(width=16, depth=256, init=firmware)
        self.core   = CoreFSM(alsru_cls=ALSRU_4LUT, memory=self.memory)

    def elaborate(self, platform):
        platform.add_resources(platform.break_off_pmod)

        m = Module()
        m.submodules.core = core = self.core

        leds = Cat(platform.request("led_g", n+1) for n in range(4))
        with m.If(core.o_ext_we & (core.o_bus_addr == 0x0000)):
            m.d.sync += leds.eq(core.o_ext_data)

        return m


def firmware():
    period = 6000000//(4*3) # 4 CPI, 3 instructions
    return [
        MOVI(R7, 0xa),
    L("blink"),
        XORI(R7, R7, 0xf),
        STXA(R7, 0),
        MOVI(R1, period&0xffff),
        MOVI(R2, period>>16),
    L("loop"),
        SUBI(R1, R1, 1),
        SBBI(R2, R2, 0),
        JNZ ("loop"),
        J   ("blink"),
    ]


if __name__ == "__main__":
    design = BonelessDemo(firmware=Instr.assemble(firmware()))
    ICEBreakerPlatform().build(design, do_program=True)
