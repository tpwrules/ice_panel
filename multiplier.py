# nmigen module to use ice40 MAC block as a multiplier

from nmigen import *

class Multiplier(Elaboratable):
    def __init__(self, signed):
        self.signed = 1 if signed else 0

        self.i_a = Signal(16)
        self.i_a_we = Signal()
        self.i_b = Signal(16)
        self.i_b_we = Signal()

        self.o_result = Signal(32)

    def elaborate(self, platform):
        m = Module()

        curr_a = Signal(16)
        curr_b = Signal(16)
        with m.If(self.i_a_we):
            m.d.sync += curr_a.eq(self.i_a)
        with m.If(self.i_b_we):
            m.d.sync += curr_b.eq(self.i_b)

        m.submodules.multiplier = Instance("SB_MAC16",
            # what the TN says you should do to make a 16x16 multiplier
            p_TOPOUTPUT_SELECT=3,
            p_BOTOUTPUT_SELECT=3,
            p_PIPELINE_16x16_MULT_REG2=1,
            p_A_SIGNED=self.signed,
            p_B_SIGNED=self.signed,
            
            i_A=curr_a,
            i_B=curr_b,
            o_O=self.o_result, # the TN calls this OUTPUT!

            i_CLK=ClockSignal(),
        )

        return m
