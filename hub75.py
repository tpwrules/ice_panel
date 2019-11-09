from nmigen import *
from nmigen.lib.cdc import FFSynchronizer
from collections import namedtuple

# A HUB75 display (or chain of displays) is driven somewhat strangely.

# Physically, it's 'width' RGB pixels wide by 'height' RGB pixels high.
# Logically, it's still 'width' pixels wide, but is only 'height//2' pixels
# high. The top half and bottom half of the display are driven simultaneously.
# If you had a 16 high display, rows 0 and 8 would both be driven at the same
# time, then 1 and 9, etc.

# The display has no concept of PWM or brightness levels, or even buffering. The
# whole display is drawn by switching rows really fast, and brightness is
# created by refreshing each row really, really fast.

# Fortunately, the display does know about color. The display takes 6 channels:
# R, G, and B on/off for the selected row in the top half, then R, G, B on/off
# for the same row in the bottom half, for 6 bits of information per output
# pixel. Because of this, whenever we refer to a "pixel" when driving, we mean
# the three colors of one physical pixel in the top half plus the three colors
# of the corresponding physical pixel in the bottom half. Each color may have a
# number of bits, depending on the selected 'bpp', but the display only sees one
# bit at a time. Similarly, we say there are 6 channels per pixel. To
# disambiguate, we say "physical pixel/channel/row" when referring to an item
# that's actually on the display panel, and just "pixel/channel/row" when
# referring to a top/bottom pair of items.

# To draw the screen, we start by selecting row 0, then sending out bit 0 of the
# 'width' pixels on that row. We wait for 'n' time for it to display, then send
# out bit 1 of the same pixels, then wait time '2n', then send out bit 2, then
# wait time '4n', etc. In this way, the total amount of light output by each LED
# is proportional to the value of the pixel, and we only have to refresh the row
# 'bpp' times instead of 2**'bpp' times. Once all the bits have been seen, we
# switch to row 1 and do it again. Once all the rows have been displayed, we
# switch back to row 0 and start all over.

# Unfortunately, proportional to the value is a bad metric, and leads to too few
# brightness levels on the low end and too many on the high end. We offer the
# option to gamma correct the pixels before displaying them to alleviate this.

_pd_fields = [
    # USER CONFIGURABLE PARAMETERS
    'width', # width of the display, in physical pixels
    'height', # height of the display, in physical pixels
    'bpp', # number of bits per color per pixel. e.g. 8 would be 24-bit RGB.

    # CALCULATED PARAMETERS
    'col_bits', # no. bits reqd. to select a column, 0 to (width-1)
    'bit_bits', # no. bits reqd. to select a bit in a pixel, 0 to bpp-1
    'row_bits', # no. bits reqd. to select a row, 0 to (height//2-1)
    'pix_bits', # no. bits reqd. to select a pixel, col_bits+row_bits
    'chan_bits', # no. bits reqd. to select a physical channel of a physical
                 # pixel. pix_bits+3, 1 to select display half, 2 to select R,
                 # G, B or (unused). indexes e.g. framebuffer memory. channel 3
                 # is skipped so that pixels start at multiples of 4 in memory.
]

class PanelDescription(namedtuple("PanelDescription", _pd_fields)):
    def __new__(cls, width, height, bpp):
        # make sure BPP sounds legit. we'll overbudget our memories if 
        # it's > 16, and there would never be enough time to refresh it anyway.
        # note also that very low BPP will probably break since the pixel reader
        # won't have enough time to read in new lines and the display would look
        # ghosty as crap since rows are being switched way too fast.
        # TODO: detect above situation and add appropriate delay?
        if bpp < 1:
            raise ValueError(
                "who would even have a {}-level display?".format(2**bpp))
        elif bpp > 16:
            raise ValueError("{}bpp? now that's just greedy".format(bpp))
        # all our counters assume width and height are a power of 2, and AFAIK
        # you can't buy the panels in non power of 2 sizes anyway.
        if width <= 0 or (width & (width-1)) != 0:
            raise ValueError(
                "width must be a positive power of 2,"
                "which doesn't include {}!".format(width))
        # height has to be at least 2 so we can split it into half displays.
        if height <= 1 or (height & (height-1)) != 0:
            raise ValueError(
                "height must be at least 2 and a power of 2,"
                "which doesn't include {}!".format(height))

        # calculate the calculated parameters from the ones the user gave us
        col_bits = (width-1).bit_length()
        bit_bits = (bpp-1).bit_length()
        row_bits = (height//2-1).bit_length()
        pix_bits = col_bits+row_bits
        chan_bits = pix_bits+1+2

        return super(PanelDescription, cls).__new__(cls,
            width, height, bpp,
            col_bits, bit_bits, row_bits,
            pix_bits, chan_bits,
        )

del _pd_fields # not needed in the future


class UpCounter(Elaboratable):
    def __init__(self, width, init=0):
        # current value of the counter
        self.value = Signal(width, reset=init)
        self.init = init
        # reset counter. counter is set to init at reset
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
            m.d.sync += self.value.eq(self.init)
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
    def __init__(self, panel_desc):
        self.pd = panel_desc

        # idle input. if asserted when the generator finishes a line,
        # the generator will not start on the next line until deasserted. blank
        # is not asserted during idle.
        self.i_idle = Signal()
        # which row is being displayed. latched at the end of the line or so.
        self.i_row_sel = Signal(self.pd.row_bits)

        # shift active output. asserted when actively shifting pixels (and thus
        # the panel should receive the shift clock)
        self.o_shift_active = Signal()
        # latch output, direct to panel
        self.o_latch = Signal()
        # blank output, direct to panel
        self.o_blank = Signal()
        # row select bits, direct to panel
        self.o_row_sel = Signal.like(self.i_row_sel)

        # which pixel in the column we are outputting
        self.col_ctr = UpCounter(self.pd.col_bits, init=0)

    def elaborate(self, platform):
        m = Module()
        # register submodules (and make a local reference without self)
        m.submodules.col_ctr = col_ctr = self.col_ctr

        # by default:
        m.d.comb += [
            self.o_shift_active.eq(0), # not outputting pixels
            col_ctr.reset.eq(1), # or counting them
            # display is active and we aren't latching new pixels
            self.o_blank.eq(0),
            self.o_latch.eq(0),
        ]

        with m.FSM("IDLE") as fsm:
            with m.State("OUTPUT"):
                # we are actively shifting pixels into the display.

                # keep counting pixels out
                m.d.comb += [
                    self.o_shift_active.eq(1),
                    col_ctr.reset.eq(0),
                ]

                # has the counter finished?
                with m.If(col_ctr.at_max):
                    # yes, so the line is over
                    m.next = "BLANK"

            with m.State("BLANK"):
                # we blank the display so there's no glitches as we apply the
                # new pixels
                m.d.comb += self.o_blank.eq(1)

                m.next = "ADDR"

            with m.State("ADDR"):
                # select the row we want to apply the pixels to, while blanked,
                # to avoid ghosting from the previously selected row.
                m.d.sync += self.o_row_sel.eq(self.i_row_sel)
                m.d.comb += self.o_blank.eq(1)

                m.next = "LATCH"

            with m.State("LATCH"):
                # latch in the new data to the row selected above, remaining
                # blank so we don't show the old row's data briefly.
                m.d.comb += [
                    self.o_latch.eq(1),
                    self.o_blank.eq(1),
                ]

                m.next = "UNBLANK"

            with m.State("UNBLANK"):
                # finally, unblank the display so everyone can see the
                # shiny new row.
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
    def __init__(self, panel_desc):
        self.pd = panel_desc

        # there are always three pixels in motion. the address of the most
        # recent pixel is output to o_col, o_bit and o_row for whatever is
        # calculating pixels. the next cycle, the calculated pixel data in
        # i_pixel is latched into disp_pixel. the cycle after that, the pixel in
        # disp_pixel is output to the panel, so it can be clocked in.
        # thus, every cycle there is a pixel being Addressed, a pixel being
        # Calculated, and a pixel being Displayed.

        # the calculated pixel data. there's always just one bit per channel
        # here; we tell the buffer what bit of the pixel we'd like when we need
        # it.
        self.i_pixel = Signal(6)

        # what pixel we are addressing
        self.o_col = Signal(self.pd.col_bits) # all the columns in the row
        self.o_bit = Signal(self.pd.bit_bits) # all the bits in the row's pixels
        self.o_row = Signal(self.pd.row_bits) # all the rows on the screen

        # non-buffered pixel data out, synchronous with line timing generator
        self.o_pixel = Signal(6)

        self.ltg = LineTimingGenerator(self.pd)
        # forward all the line generator outputs so they can be attached to the
        # display
        self.o_shift_active = self.ltg.o_shift_active
        self.o_latch = self.ltg.o_latch
        self.o_blank = self.ltg.o_blank
        self.o_row_sel = self.ltg.o_row_sel

        self.col_ctr = UpCounter(self.pd.col_bits)
        self.bit_ctr = UpCounter(self.pd.bit_bits)
        self.row_ctr = UpCounter(self.pd.row_bits)

        # brightness control. split into two counters so we can have the actual
        # line take a non-power-of-two time.
        # count number of cycles in the line. line generator takes 4 cycles
        # between lines to switch.
        self.num_line_cycles = self.pd.width+4
        self.line_cycle_ctr = UpCounter((self.num_line_cycles-1).bit_length())
        # then number of line times so we can keep track of brightness
        self.line_times_ctr = UpCounter(self.pd.bpp)

    def elaborate(self, platform):
        m = Module()
        # register submodules (and make a local reference without self)
        m.submodules.ltg = ltg = self.ltg
        m.submodules.col_ctr = col_ctr = self.col_ctr
        m.submodules.bit_ctr = bit_ctr = self.bit_ctr
        m.submodules.row_ctr = row_ctr = self.row_ctr
        m.submodules.line_cycle_ctr = line_cycle_ctr = self.line_cycle_ctr
        m.submodules.line_times_ctr = line_times_ctr = self.line_times_ctr

        m.d.comb += self.o_bit.eq(0)

        # pixel data currently being sent to the panel
        disp_pixel = Signal(6)
        # latch the calculated pixel for the panel next cycle
        m.d.sync += disp_pixel.eq(self.i_pixel)
        # send out the pixel in the buffer
        m.d.comb += self.o_pixel.eq(disp_pixel),

        # address the pixel pointed to by the position counters
        m.d.comb += [
            self.o_col.eq(col_ctr.value),
            self.o_bit.eq(bit_ctr.value),
            self.o_row.eq(row_ctr.value),
        ]

        # the position counters count columns, then bits, then rows. we do the
        # rows last because switching rows too fast causes ghosting.

        # should the position counters be counting? the state machine says no if
        # we are waiting for the line to be displayed.
        should_count = Signal()
        next_row = Signal()
        m.d.comb += [
            col_ctr.enable.eq(should_count),
            # count bit counter when column counter expires
            bit_ctr.enable.eq(col_ctr.at_max & should_count),
            # count to the next row when bit counter reaches its maximum.
            # bpp might not be a power of 2, so we have to explicitly check
            # instead of waiting for rollover.
            next_row.eq((bit_ctr.value == self.pd.bpp-1) & bit_ctr.enable),
            row_ctr.enable.eq(next_row),
            bit_ctr.reset.eq(next_row),
        ]

        # the line time counter counts every time the line cycle counter
        # finishes a line
        m.d.comb += [
            line_times_ctr.enable.eq(
                line_cycle_ctr.value == self.num_line_cycles-1),
            line_cycle_ctr.reset.eq(
                line_cycle_ctr.value == self.num_line_cycles-1),
        ]

        # we compare it with which bit of the pixel we are processing. since
        # generating that bit from the bit counter is expensive, we instead
        # rotate a bitmask to select the appropriate bit of the line counter. of
        # course, if this mask were to get out of sync with the bit counter,
        # the brightnesses would be all wrong. fortunately, that's not possible.
        line_time_delay = Signal(self.pd.bpp, reset=1)

        should_idle = Signal() # should the line generator be idling?
        m.d.comb += ltg.i_idle.eq(should_idle)

        # by default:
        m.d.comb += [
            should_idle.eq(1),
            should_count.eq(0),
            line_cycle_ctr.reset.eq(0),
            line_times_ctr.reset.eq(0),
        ]

        with m.FSM("STARTLINE") as fsm:
            with m.State("STARTLINE"):
                # starting to output a line.

                # this cycle, we are addressing the first pixel of the line (and
                # have been since the end of the last line). increment the
                # counter so we address the second on the next cycle, and have
                # the first calculating when we transition into DOLINE.
                m.d.comb += should_count.eq(1)

                # reset the line counters. since we do it here, it will
                # be one cycle ahead of the actual line output but that's okay.
                m.d.comb += [
                    line_cycle_ctr.reset.eq(1),
                    line_times_ctr.reset.eq(1),
                ]

                m.next = "DOLINE"

            with m.State("DOLINE"):
                # on the first cycle of DOLINE, we address the second pixel of
                # the line and calculate the first. we stop idling the line
                # generator at that point, meaning it will display the first
                # pixel on the second cycle of DOLINE, exactly when the first
                # pixel is finished calculating and is ready for display.

                # don't idle the line generator
                m.d.comb += should_idle.eq(0)

                # are we addressing the last pixel of the line?
                with m.If(col_ctr.at_max):
                    # yes, tell the line generator what row it should update
                    # whenever it finishes
                    m.d.sync += ltg.i_row_sel.eq(self.row_ctr.value)
                    # and wait for this line to end
                    m.next = "WAIT"
                with m.Else():
                    # keep on counting the pixels throughout the line
                    m.d.comb += should_count.eq(1)
                    # once we're finished, we don't count any more so we remain
                    # on this line until the next one starts.

            with m.State("WAIT"):
                # have we finished with this brightness?
                with m.If(line_time_delay & line_times_ctr.value):
                    # rotate delay time one bit to the left. next line will be
                    # displayed for twice as long, which is appropriate since
                    # its bit in the pixel is twice as significant.
                    m.d.sync += line_time_delay.eq(
                        Cat(line_time_delay[-1], line_time_delay[:-1]))
                    # yes, address the first pixel of the next line
                    m.d.comb += should_count.eq(1)
                    # and go start it
                    m.next = "STARTLINE"

        return m

# read pixels from memory and serve them to the frame generator
class PixelReader(Elaboratable):
    def __init__(self, panel_desc, led_domain="sync"):
        self.pd = panel_desc
        # what domain the frame generator is in. we will still read pixels from
        # sync
        self.led_domain = led_domain

        # so, we have two important requirements.
        # one: we must be able to deliver 6 pixel bits (one from each channel)
        # to the frame generator every cycle. if we fail, the display will get
        # glitchy.
        # two: we must only read each line from memory once. if we read it more
        # than once and it changes between reads, the display starts looking
        # really funky because we are drawing one bit at a time. if a bit gets
        # changed, we might not catch it and then the pixel wouldn't look like
        # it should have been before the change, or after, and would instead
        # flash some random third color.
        # to do this, we have a RAM buffer that can buffer all pixel bits for 6
        # channels for two lines. thankfully, the frame generator doesn't need
        # 6bits/cyc every cycle, so we do have enough time to read one  channel
        # per cycle and fill up one half of the memory while we feed the other
        # to the frame generator.

        # pixel the frame generator wants to read.
        self.i_col = Signal(self.pd.col_bits)
        self.i_bit = Signal(self.pd.bit_bits)
        self.i_row = Signal(self.pd.row_bits)
        self.i_odd_line = Signal()
        # and that pixel's data
        self.o_pixel = Signal(6)

        # read master port to whatever memory has the pixel data. we assert re
        # when we want to read a channel addressed by raddr. on the cycle that
        # rack is asserted, we latch that channel's data in from rdata.
        self.o_re = Signal()
        self.i_rack = Signal()
        self.o_raddr = Signal(self.pd.chan_bits)
        self.i_rdata = Signal(self.pd.bpp)

        # our buffer memory. it needs to be crazy wide so we can read 6 full
        # channels per cycle and select the 6 bits the frame generator wants.
        self.line_buf = Memory(width=6*self.pd.bpp, depth=2*self.pd.width)

        # counter to index the pixels in the column when reading.
        self.col_ctr = UpCounter(self.pd.col_bits, init=0)

    def elaborate(self, platform):
        m = Module()
        # register submodules (and make a local reference without self)
        m.submodules.rdport = rdport = \
            self.line_buf.read_port(domain=self.led_domain, transparent=False)
        # we want each enable to cover one channel so we can write one channel
        # at a time
        m.submodules.wrport = wrport = \
            self.line_buf.write_port(granularity=self.pd.bpp)
        m.submodules.col_ctr = col_ctr = self.col_ctr

        # zeroth, we need to know which row the frame generator is currently
        # working on so we can work on the next one. because it's in the LED
        # domain and we're in sync, we have to synchronize it to us. there will
        # be a few cycles of delay, but it's okay unless the pixel reader is
        # like 99.99% busy.
        fg_row = Signal(self.pd.row_bits)
        fg_row_syncer = FFSynchronizer(i=self.i_row, o=fg_row,
            o_domain="sync", reset_less=False, stages=3)
        m.submodules += fg_row_syncer

        # first, give the frame generator the pixels it wants
        fg_data = Signal(6*self.pd.bpp)
        m.d.comb += [
            # they get read directly from the buffer in the LED domain
            rdport.addr.eq(Cat(self.i_col, self.i_row[0])),
            fg_data.eq(rdport.data),
        ]
        # once the memory is read, we have to select the bits from the pixels
        # that the frame generator actually wants.
        with m.Switch(self.i_bit):
            for bn in range(self.pd.bpp): # which bit from the pixel
                with m.Case(bn):
                    for cn in range(6): # which pixel channel
                        m.d.comb += self.o_pixel[cn].eq(
                            fg_data[cn*self.pd.bpp+bn])

        # second, we need to read pixels to fill the buffer for the next line
        rd_row = Signal(self.pd.row_bits) # which row we're reading
        fg_rchan = Signal(3) # which channel we're reading. goes 0-2, 4-6.
        # combine into output physical address.
        # channel bits, column, row, display half bit
        m.d.comb += self.o_raddr.eq(
            Cat(fg_rchan[:2], col_ctr.value, rd_row, fg_rchan[-1]))
        
        should_count = Signal() # should we count to the next pixel?
        m.d.comb += self.col_ctr.enable.eq(should_count)
        should_read = Signal() # are we intending to read a pixel?
        m.d.comb += self.o_re.eq(should_read)
        done_reading = Signal() # did the pixel read finish?
        m.d.comb += done_reading.eq(self.o_re & self.i_rack)

        # once the pixel read finishes, we want to write it to the buffer.
        # we replicate the read channel data across the 6 memory channels, then
        # only enable the write for the channel that the data is for.
        m.d.comb += wrport.data.eq(Repl(self.i_rdata, 6))
        # and write it at the address of the current pixel, in the appropriate
        # buffer half
        m.d.sync += wrport.addr.eq(Cat(col_ctr.value, rd_row[0]))
        # enable the appropriate channel for writing
        for ch in range(6):
            # we index channels from 0 to 5 here, but fg_rchan goes 0-2, 4-6
            # because we skip 3 to keep addressing nice.
            m.d.sync += wrport.en[ch].eq(
                done_reading & (fg_rchan == (ch if ch < 3 else ch+1)))
        
        # FSM generates the channel addresses and manages starting/ending reads
        with m.FSM("WAIT"):
            with m.State("WAIT"): # wait for next line to begin
                # i.e. the frame generator is reading the line we just wrote to
                with m.If(fg_row[0] == rd_row[0]):
                    # so start working on the next line
                    m.d.sync += rd_row.eq(fg_row+1)
                    # start at the first pixel
                    m.d.comb += col_ctr.reset.eq(1)
                    # and get back into it
                    m.next = "P0R"

            with m.State("P0R"): # top physical pixel's red channel
                m.d.comb += [
                    fg_rchan.eq(0), # which is channel 0
                    should_read.eq(1),
                ]
                with m.If(done_reading): # have we got the data?
                    # go to next pixel. writing is handled outside state machine
                    m.next = "P0G"

            with m.State("P0G"): # top physical pixel's green channel
                m.d.comb += [
                    fg_rchan.eq(1),
                    should_read.eq(1),
                ]
                with m.If(done_reading):
                    m.next = "P0B"

            with m.State("P0B"): # top physical pixel's blue channel
                m.d.comb += [
                    fg_rchan.eq(2),
                    should_read.eq(1),
                ]
                with m.If(done_reading):
                    m.next = "P1R"

            with m.State("P1R"): # bottom physical pixel's red channel
                m.d.comb += [
                    fg_rchan.eq(4),
                    should_read.eq(1),
                ]
                with m.If(done_reading):
                    m.next = "P1G"

            with m.State("P1G"): # bottom physical pixel's green channel
                m.d.comb += [
                    fg_rchan.eq(5),
                    should_read.eq(1),
                ]
                with m.If(done_reading):
                    m.next = "P1B"

            with m.State("P1B"): # bottom physical pixel's blue channel
                m.d.comb += [
                    fg_rchan.eq(6),
                    should_read.eq(1),
                ]
                with m.If(done_reading):
                    # address next pixel in the line
                    m.d.comb += should_count.eq(1)
                    with m.If(col_ctr.at_max): # done with this line?
                        m.next = "WAIT"
                    with m.Else():
                        m.next = "P0R"                

        return m


# a whole screen and buffer
class BufferedHUB75(Elaboratable):
    def __init__(self, panel_desc, led_domain="sync",
            gamma=None, gamma_bpp=None):
        self.pd = panel_desc

        # gamma correction.
        # gamma is the exponent of the gamma curve, or None to disable.
        # if gamma is enabled, gamma_bpp bits look up a pd.bpp wide table.
        # if gamma_bpp is none, gamma_bpp = pd.bpp
        self.gamma = gamma
        if gamma is not None and gamma_bpp is not None:
            self.gamma_bpp = gamma_bpp
        else:
            self.gamma_bpp = self.pd.bpp

        if self.gamma_bpp < 1:
            raise ValueError(
                "who would even have a {}-color display?".format(
                    2**self.gamma_bpp))
        # gamma_bpp can be as high as the user wants, though it would be dumb

        if self.gamma is not None:
            # generate the gamma table from the given parameters
            in_scale = 2**self.gamma_bpp-1
            out_scale = 2**self.pd.bpp-1
            table = []
            for x in range(2**self.gamma_bpp):
                table.append(int(((x/in_scale)**self.gamma)*out_scale))
            self.gamma_table = Memory(
                width=self.pd.bpp, depth=2**self.gamma_bpp, init=table)

        # what domain to run the LED engine in. framebuffers will still be
        # written to from sync.
        self.led_domain = led_domain

        # framebuffer write inputs.
        # the low 2 bits of the address select R, G, B, or nothing.
        # next N bits select 2**N columns (X position).
        # next M bits select 2**M rows (Y position).
        # the last bit selects screen half (RGB0 or RGB1).
        # this means the display is linearly addressed R, G, B color, left to
        # right, top to bottom
        self.i_we = Signal()
        self.i_waddr = Signal(self.pd.chan_bits)
        self.i_wdata = Signal(self.gamma_bpp)

        # our only output is to the display, which we grab direct from the
        # platform during elaboration

        # and have a mechanism to actually generate the display
        # (display generation happens in the LED clock domain)
        self.ftg = DomainRenamer(self.led_domain)(FrameTimingGenerator(self.pd))

        # read pixels and give them to the frame generator. we want it reading
        # from the LED domain too.
        self.pr = DomainRenamer(self.led_domain)(
            PixelReader(self.pd, led_domain=self.led_domain))

        self.buf = Memory(width=self.pd.bpp, depth=2**self.pd.chan_bits)

    def elaborate(self, platform):
        # get the display from the platform.
        # we use DDR buffer on shift clock so we can cleanly gate it.
        hub75 = platform.request("hub75", 0, xdr={"shift_clock": 2})
        
        m = Module()
        m.submodules.ftg = ftg = self.ftg
        m.submodules.pr = pr = self.pr
        m.submodules.rdport = rdport = \
            self.buf.read_port(domain=self.led_domain, transparent=False)
        m.submodules.wrport = wrport = self.buf.write_port()

        # handle the write data input
        # buffer them so we can decode at our leisure
        b_we = Signal.like(self.i_we)
        b_waddr = Signal.like(self.i_waddr)
        b_wdata = Signal.like(self.i_wdata)
        # and once more, so we can gamma correct the data
        g_we = Signal.like(self.i_we)
        g_waddr = Signal.like(self.i_waddr)
        g_wdata = Signal(self.pd.bpp)
        m.d.sync += [ # comes from memory write domain i.e. sync
            b_we.eq(self.i_we),
            b_waddr.eq(self.i_waddr),
            b_wdata.eq(self.i_wdata),
            # gamma correction doesn't modify WE or address
            g_we.eq(b_we),
            g_waddr.eq(b_waddr),
        ]

        if self.gamma is None:
            # just pass data through
            m.d.sync += g_wdata.eq(b_wdata)
        else:
            # wire through gamma lookup table
            g_rdport = self.gamma_table.read_port()
            m.submodules += g_rdport
            m.d.comb += [
                g_rdport.addr.eq(b_wdata),
                g_wdata.eq(g_rdport.data),
            ]

        m.d.comb += [
            wrport.addr.eq(g_waddr),
            wrport.data.eq(g_wdata),
            wrport.en.eq(g_we),

            pr.i_row.eq(ftg.o_row),
            pr.i_col.eq(ftg.o_col),
            pr.i_bit.eq(ftg.o_bit),
            ftg.i_pixel.eq(pr.o_pixel),

            rdport.addr.eq(pr.o_raddr),
            pr.i_rdata.eq(rdport.data),
        ]

        m.d[self.led_domain] += pr.i_rack.eq(pr.o_re)


        # we set the panel outputs synchronously so the panel doesn't see
        # glitches as things settle. this is done in the LED domain.
        m.d[self.led_domain] += [
            hub75.latch.eq(ftg.o_latch),
            hub75.blank.eq(ftg.o_blank),

            Cat(hub75.r0, hub75.g0, hub75.b0,
                hub75.r1, hub75.g1, hub75.b1).eq(ftg.o_pixel),

            # jam together the row selection pins that go to the display
            # and update them with the current row selection.
            Cat(getattr(hub75, "a"+str(n)) for n in range(5)).eq(
                Cat(ftg.o_row_sel, 0))
        ]

        # now we have to generate the panel shift clock.
        # we use DDR so we can cleanly gate it. instead of the panel receiving
        # 1->0, it receives 0->shift active, so that there's no high period if
        # shift_active is deasserted.
        # note that we don't use a buffered version of shift_active because the
        # DDR logic already has a buffer.
        # note also that we output an inverted signal so that the shift clock
        # falls as the FPGA clock rises. this gives half a clock for the data to
        # make it to the panel before being clocked in.
        m.d.comb += [
            hub75.shift_clock.o_clk.eq(ClockSignal(self.led_domain)),
            hub75.shift_clock.o0.eq(0),
            hub75.shift_clock.o1.eq(ftg.o_shift_active),
        ]

        return m