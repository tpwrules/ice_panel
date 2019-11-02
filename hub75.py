# modules related to driving a HUB75 connector display.
# we refer a lot to panel_shape: physical (width, height) in LEDs
# i.e. width is how many pixels to shift out per row
# and height is 2**(addr_bits)/2 (assuming two rows are driven at once)

from nmigen import *

class UpCounter(Elaboratable):
    def __init__(self, width, reset=0):
        # current value of the counter
        self.value = Signal(width, reset=reset)
        self._reset = reset
        # reset counter. counter is set to reset at reset
        self.reset = Signal()

        # is the counter at its maximum value?
        self.at_max = Signal()
        self._max = 2**width-1
        # is the counter at its minimum value? (i.e. zero)
        self.at_min = Signal()

    def elaborate(self, platform):
        m = Module()

        # are we being reset this cycle?
        with m.If(self.reset):
            # yes, load the reset value for next cycle
            m.d.sync += self.value.eq(self._reset)
        with m.Else():
            # no, load the next value for next cycle
            m.d.sync += self.value.eq(self.value+1)

        # check counter position this cycle
        m.d.comb += [
            self.at_max.eq(self.value == self._max),
            self.at_min.eq(self.value == 0),
        ]

        return m


# generate the line timing and signals for the display
class LineTimingGenerator(Elaboratable):
    def __init__(self, panel_shape):
        self.panel_shape = panel_shape

        # idle input. if asserted when the generator finishes a line,
        # the generator will not start on the next line until deasserted. blank
        # will be deasserted during idle.
        self.i_idle = Signal()
        # line addr input. automatically output to display at end of line
        self.i_line_addr = Signal((panel_shape[1]//2-1).bit_length())

        # line sync output. asserted during the first pixel of the line.
        self.o_line_sync = Signal()
        # shift active output. asserted when actively shifting pixels (and thus
        # the panel should receive the shift clock)
        self.o_shift_active = Signal()
        # latch output, direct to panel
        self.o_latch = Signal()
        # blank output, direct to panel
        self.o_blank = Signal()
        # line address bits, direct to panel
        self.o_line_addr = Signal.like(self.i_line_addr)


        # current pixel output
        self.pixel_ctr = UpCounter((panel_shape[0]-1).bit_length(), reset=0)

    def elaborate(self, platform):
        m = Module()
        # register submodules (and make a local reference without self)
        m.submodules.pixel_ctr = pixel_ctr = self.pixel_ctr

        # by default:
        m.d.comb += [
            self.o_shift_active.eq(0), # not outputting pixels
            pixel_ctr.reset.eq(1), # or counting them
            # display is active and we aren't latching
            self.o_blank.eq(0),
            self.o_latch.eq(0),
        ]

        # state machine handles the parts of display generation
        with m.FSM("OUTPUT") as fsm:
            with m.State("OUTPUT"):
                # we are actively shifting pixels into the display.

                # deassert line sync. it gets asserted on transition into
                # OUTPUT so we deassert it the first pixel so it's only on for
                # one pixel.
                m.d.sync += self.o_line_sync.eq(0)
                # keep counting pixels out
                m.d.comb += [
                    self.o_shift_active.eq(1),
                    pixel_ctr.reset.eq(0),
                ]

                # has the counter finished?
                with m.If(pixel_ctr.at_max):
                    # yes, so the line is over
                    m.next = "BLANK"

            with m.State("BLANK"):
                # we blank the display so there's no glitches as we apply
                # the new pixels
                m.d.comb += self.o_blank.eq(1)

                m.next = "ADDR"

            with m.State("ADDR"):
                # address the row we want to apply the pixels to, while blanked
                # to avoid ghosting from the previously addressed row.
                m.d.sync += self.o_line_addr.eq(self.i_line_addr)
                m.d.comb += self.o_blank.eq(1)

                m.next = "LATCH"

            with m.State("LATCH"):
                # latch in the new data to the row selected above, remaining
                # blank so we don't show the old row's data briefly
                m.d.comb += [
                    self.o_latch.eq(1),
                    self.o_blank.eq(1),
                ]

                m.next = "UNBLANK"

            with m.State("UNBLANK"):
                # finally, unblank the display so everyone can see the
                # shiny new line.
                # (it's unblanked now because we didn't specifcally blank it)

                with m.If(self.i_idle == 1):
                    m.next = "IDLE" # idle if requested
                with m.Else():
                    # the next line will start next cycle
                    m.d.sync += self.o_line_sync.eq(1)
                    m.next = "OUTPUT"

            with m.State("IDLE"):
                with m.If(self.i_idle == 0):
                    # the next line will start next cycle
                    m.d.sync += self.o_line_sync.eq(1)
                    m.next = "OUTPUT"

        return m
