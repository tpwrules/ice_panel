#!/usr/bin/env nmigen

from collections import namedtuple
import warnings

from nmigen import *
from nmigen.lib.cdc import ResetSynchronizer
from nmigen.cli import main

# original code https://github.com/kbob/nmigen-examples/blob/master/lib/pll.py

class PLL(Elaboratable):

    """
    Instantiate the iCE40's phase-locked loop (PLL).

    This uses the iCE40's SB_PLL40_2_PAD primitive in simple feedback
    mode.

    The reference clock is directly connected to a package pin. To
    allocate that pin, request the pin with dir='-'; otherwise nMigen
    inserts an SB_IO on the pin.  E.g.,

        clk_pin = platform.request('clk12', dir='-')

    The PLL passes the input clock through to the orig_domain_name domain,
    then outputs the new clock to the pll_domain_name domain.

    This module also has a reset synchronizer -- the domain's reset line
    is not released until a few clocks after the PLL lock signal is
    good.
    """

    def __init__(self, freq_in_mhz, freq_out_mhz, clk_pin,
            pll_domain_name, orig_domain_name="sync"):
        self.freq_in = freq_in_mhz
        self.freq_out = freq_out_mhz
        self.coeff = self._calc_freq_coefficients()

        self.clk_pin = clk_pin
        self.reset = Signal(reset=1)
        self.pll_lock = Signal()

        self.orig_domain_name = orig_domain_name
        self.orig_domain = ClockDomain(orig_domain_name)
        self.pll_domain_name = pll_domain_name
        self.pll_domain = ClockDomain(pll_domain_name)

    def _calc_freq_coefficients(self):
        # cribbed from Icestorm's icepll.
        f_in, f_req = self.freq_in, self.freq_out
        assert 10 <= f_in <= 16
        assert 16 <= f_req <= 275
        coefficients = namedtuple('coefficients', 'divr divf divq')
        divf_range = 128        # see comments in icepll.cc
        best_fout = float('inf')
        for divr in range(16):
            pfd = f_in / (divr + 1)
            if 10 <= pfd <= 133:
                for divf in range(divf_range):
                    vco = pfd * (divf + 1)
                    if 533 <= vco <= 1066:
                        for divq in range(1, 7):
                            fout = vco * 2**-divq
                            if abs(fout - f_req) < abs(best_fout - f_req):
                                best_fout = fout
                                best = coefficients(divr, divf, divq)
        if best_fout != f_req:
            warnings.warn(
                f'PLL: requested {f_req} MHz, got {best_fout} MHz)',
                stacklevel=3)
        return best

    def elaborate(self, platform):
        pll = Instance("SB_PLL40_2_PAD",
            p_FEEDBACK_PATH='SIMPLE',
            p_DIVR=self.coeff.divr,
            p_DIVF=self.coeff.divf,
            p_DIVQ=self.coeff.divq,
            p_FILTER_RANGE=0b001,

            i_PACKAGEPIN=self.clk_pin,
            i_RESETB=self.reset,
            i_BYPASS=Const(0),

            o_PLLOUTGLOBALA=ClockSignal(self.orig_domain_name),
            o_PLLOUTGLOBALB=ClockSignal(self.pll_domain_name),
            o_LOCK=self.pll_lock
        )

        m = Module()
        m.submodules += pll
        m.submodules += ResetSynchronizer(
            ~self.pll_lock, domain=self.pll_domain_name)
        m.submodules += ResetSynchronizer(
            ~self.pll_lock, domain=self.orig_domain_name)

        m.domains += self.orig_domain
        m.domains += self.pll_domain

        return m
