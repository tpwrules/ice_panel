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
        # should we count? yes, by default
        self.enable = Signal(reset=1)

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
        with m.Elif(self.enable):
            # no, load the next value for next cycle if we're allowed to count
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
        with m.FSM("IDLE") as fsm:
            with m.State("OUTPUT"):
                # we are actively shifting pixels into the display.

                # keep counting pixels out
                m.d.comb += [
                    self.o_shift_active.eq(1),
                    pixel_ctr.reset.eq(0),
                ]

                # has the counter finished?
                with m.If(pixel_ctr.at_max):
                    # yes, so the line is over
                    m.next = "BLANK"
                    # and we want to emit the sync pulse
                    m.d.sync += self.o_line_sync.eq(1)

            with m.State("BLANK"):
                # we blank the display so there's no glitches as we apply
                # the new pixels
                m.d.comb += self.o_blank.eq(1)
                # and finish the sync pulse
                m.d.sync += self.o_line_sync.eq(0)

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
                    m.next = "OUTPUT"

            with m.State("IDLE"):
                with m.If(self.i_idle == 0):
                    m.next = "OUTPUT"

        return m

# keep track of where we are in the overall display
class FrameTimingGenerator(Elaboratable):
    def __init__(self, panel_shape):
        self.panel_shape = panel_shape

        # what color the next pixels should be. if location is output at cycle
        # n, new color should be supplied at n+1
        self.i_rgb0 = Signal(3)
        self.i_rgb1 = Signal(3)

        self.row_bits = (panel_shape[1]//2-1).bit_length()
        self.col_bits = (panel_shape[0]-1).bit_length()

        # what pixel we want
        self.o_row = Signal(self.row_bits) # select 2 rows
        self.o_col = Signal(self.col_bits)

        # non-buffered color out, synchronous with line timing generator
        self.o_rgb0 = Signal(3)
        self.o_rgb1 = Signal(3)
        
        self.ltg = LineTimingGenerator(panel_shape)
        # and all the line generator outputs
        self.o_line_sync = self.ltg.o_line_sync
        self.o_shift_active = self.ltg.o_shift_active
        self.o_latch = self.ltg.o_latch
        self.o_blank = self.ltg.o_blank
        self.o_line_addr = self.ltg.o_line_addr

        self.pixel_ctr = UpCounter(self.col_bits+self.row_bits, reset=0)
        self.idle_ctr = UpCounter(14, reset=0)

    def elaborate(self, platform):
        m = Module()
        # register submodules (and make a local reference without self)
        m.submodules.ltg = ltg = self.ltg
        m.submodules.pixel_ctr = pixel_ctr = self.pixel_ctr
        m.submodules.idle_ctr = idle_ctr = self.idle_ctr

        # There are always three pixels in motion. The address of the most
        # recent pixel is output to o_row and o_col for whatever is calculating
        # pixels. The next cycle, the pixel at i_rgbX is latched into disp_rgbX.
        # The cycle after that, the pixel in disp_rgbX is output to the panel to
        # be clocked in.

        # thus every cycle there is a pixel being Addressed, a pixel being
        # Calculated, and a pixel being Displayed.

        # hold the calculated pixel for display
        disp_rgb0 = Signal(3)
        disp_rgb1 = Signal(3)

        # latch the calculated pixel into the display buffer
        m.d.sync += [
            disp_rgb0.eq(self.i_rgb0),
            disp_rgb1.eq(self.i_rgb1),
        ]
        # display the pixel in the display buffer
        m.d.comb += [
            self.o_rgb0.eq(disp_rgb0),
            self.o_rgb1.eq(disp_rgb1),
        ]

        # output pixel counter so pixel can be addressed
        m.d.comb += [
            self.o_col.eq(pixel_ctr.value[:self.col_bits]),
            self.o_row.eq(pixel_ctr.value[self.col_bits:]),
        ]

        # simple names for state machine variables
        should_idle = Signal() # should the line generator be idling?
        should_count = Signal() # should the pixel counter be counting?
        m.d.comb += [
            ltg.i_idle.eq(should_idle),
            pixel_ctr.enable.eq(should_count),
        ]
        # by default:
        m.d.comb += [
            should_idle.eq(1),
            should_count.eq(0),
            idle_ctr.reset.eq(1),
        ]

        with m.FSM("STARTLINE") as fsm:
            with m.State("STARTLINE"):
                # starting to output a line.

                # last cycle, a pixel was addressed with whatever was in
                # pixel_ctr. increment the so we have a calculated pixel this
                # cycle for DOLINE. counter so we ha
                m.d.comb += should_count.eq(1)
                m.next = "DOLINE"

            with m.State("DOLINE"):
                # we are now doing the line

                # de-idle the line generator. its first pixel will be next
                # cycle.
                m.d.comb += should_idle.eq(0)
                # keep on counting the pixels throughout the line
                m.d.comb += should_count.eq(1)

                # is this the last pixel?
                with m.If(~pixel_ctr.value[:self.col_bits] == 0):
                    # yes, tell the linereader the row for when it finishes
                    m.d.sync += ltg.i_line_addr.eq(
                        pixel_ctr.value[self.col_bits:])
                    # count one more pixel so the first pixel of the next line
                    # is being addressed.
                    m.d.comb += should_count.eq(1)
                    # and wait for the line to end
                    m.next = "ENDLINE"

            with m.State("ENDLINE"):
                # wait for the line to be latched so we know it's finished
                with m.If(self.ltg.o_latch):
                    # we want to wait a while before doing the next line because
                    # rapidly switching lines causes severe ghosting
                    m.d.comb += idle_ctr.reset.eq(0)
                    m.next = "WAIT"

            with m.State("WAIT"):
                # just wait some time
                m.d.comb += idle_ctr.reset.eq(0)
                with m.If(idle_ctr.at_min):
                    m.next = "STARTLINE"

        return m

# buffer one channel for the whole panel in BRAM at 8bpp
class PixelBuffer(Elaboratable):
    def __init__(self, panel_shape):
        self.panel_shape = panel_shape

        # calculate address widths
        self.row_bits = (panel_shape[1]//2-1).bit_length()
        self.col_bits = (panel_shape[0]-1).bit_length()
        self.pixel_bits = self.row_bits + self.col_bits

        # write input port
        self.i_we = Signal()
        self.i_waddr = Signal(self.pixel_bits)
        self.i_wdata = Signal(8)

        # desired pixel from the frame generator
        self.i_row = Signal(self.row_bits)
        self.i_col = Signal(self.col_bits)

        # what color that pixel is
        self.o_pixel = Signal()

        self.mem = Memory(width=8, depth=2**self.pixel_bits)

    def elaborate(self, platform):
        m = Module()
        # register submodules (and make a local reference without self)
        m.submodules.rdport = rdport = self.mem.read_port()
        m.submodules.wrport = wrport = self.mem.write_port()

        # write port goes straight to the memory
        m.d.comb += [
            wrport.en.eq(self.i_we),
            wrport.addr.eq(self.i_waddr),
            wrport.data.eq(self.i_wdata),
        ]

        # read port goes to the frame generator
        m.d.comb += [
            rdport.addr.eq(Cat(self.i_col, self.i_row)),
            self.o_pixel.eq(rdport.data[0]),
        ]

        return m
