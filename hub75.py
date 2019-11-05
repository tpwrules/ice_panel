# modules related to driving a HUB75 connector display.
# we refer a lot to panel_shape: physical (width, height) in LEDs
# i.e. width is how many pixels to shift out per row
# and height is 2**(addr_bits)/2 (because two rows are driven at once)
#
# we basically always assume one pixel/address/etc is actually two, one for
# RGB0 and the other for RGB1. except for in the buffered addresses!

from nmigen import *
from nmigen.lib.coding import Decoder

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
    def __init__(self, panel_shape):
        self.panel_shape = panel_shape

        # idle input. if asserted when the generator finishes a line,
        # the generator will not start on the next line until deasserted. blank
        # is not asserted during idle.
        self.i_idle = Signal()
        # which line is being displayed. latched at the end of the line or so
        self.i_line_addr = Signal((panel_shape[1]//2-1).bit_length())

        # line sync output. asserted during the last pixel of the line.
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
        self.pixel_ctr = UpCounter((panel_shape[0]-1).bit_length(), init=0)

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
                # we blank the display so there's no glitches as we apply the
                # new pixels
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
    def __init__(self, panel_shape, bpp=1):
        self.panel_shape = panel_shape
        # bits per color per pixel. determines modulation steps
        self.bpp = bpp

        # the calculated pixel color. there's always just one bit per channel
        # here; we tell the buffer what bit of the pixel we want now.
        self.i_rgb0 = Signal(3)
        self.i_rgb1 = Signal(3)

        self.row_bits = (panel_shape[1]//2-1).bit_length()
        # index the bit of the pixel
        self.bit_bits = (bpp-1).bit_length()
        self.col_bits = (panel_shape[0]-1).bit_length()

        # what pixel we are addressing
        self.o_row = Signal(self.row_bits)
        self.o_col = Signal(self.col_bits)
        self.o_bit = Signal(self.bit_bits)

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

        self.col_ctr = UpCounter(self.col_bits)
        self.bit_ctr = UpCounter(self.bit_bits)
        self.row_ctr = UpCounter(self.row_bits)

        # brightness control. split into two counters so we can have the actual
        # line take a non-power-of-two time.
        # count number of cycles in the line. line generator takes 4 cycles
        # between lines to switch.
        self.num_line_cycles = panel_shape[0]+4
        self.line_cycle_ctr = UpCounter((self.num_line_cycles-1).bit_length())
        # then number of line times so we can keep track of brightness
        self.line_times_ctr = UpCounter(self.bpp)

    def elaborate(self, platform):
        m = Module()
        # register submodules (and make a local reference without self)
        m.submodules.ltg = ltg = self.ltg
        m.submodules.col_ctr = col_ctr = self.col_ctr
        m.submodules.bit_ctr = bit_ctr = self.bit_ctr
        m.submodules.row_ctr = row_ctr = self.row_ctr
        m.submodules.line_cycle_ctr = line_cycle_ctr = self.line_cycle_ctr
        m.submodules.line_times_ctr = line_times_ctr = self.line_times_ctr

        # there are always three pixels in motion. the address of the most
        # recent pixel is output to o_row and o_col for whatever is calculating
        # pixels. the next cycle, the pixel at i_rgbX is latched into disp_rgbX.
        # the cycle after that, the pixel in disp_rgbX is output to the panel to
        # be clocked in.

        # thus every cycle there is a pixel being Addressed, a pixel being
        # Calculated, and a pixel being Displayed.

        m.d.comb += self.o_bit.eq(0)

        # hold the currently displayed pixel
        disp_rgb0 = Signal(3)
        disp_rgb1 = Signal(3)

        # latch the calculated pixel for display next cycle
        m.d.sync += [
            disp_rgb0.eq(self.i_rgb0),
            disp_rgb1.eq(self.i_rgb1),
        ]
        # display the pixel in the display buffer
        m.d.comb += [
            self.o_rgb0.eq(disp_rgb0),
            self.o_rgb1.eq(disp_rgb1),
        ]

        # address the pixel pointed to by the position counters
        m.d.comb += [
            self.o_col.eq(col_ctr.value),
            self.o_bit.eq(bit_ctr.value),
            self.o_row.eq(row_ctr.value),
        ]

        # the position counters count columns, bits, then rows. we do the rows
        # last because switching rows too fast causes ghosting.
        # should the position counters be counting? the state machine says no if
        # we are waiting for the line to be displayed.
        should_count = Signal()
        should_count_row = Signal()
        m.d.comb += [
            col_ctr.enable.eq(should_count),
            # count bit when column counter expires
            bit_ctr.enable.eq(col_ctr.at_max & should_count),
            # count row when bit reaches its maximum.
            # bpp might not be a power of 2, so we have to explicitly check
            # instead of waiting for rollover.
            should_count_row.eq((bit_ctr.value == self.bpp-1) & bit_ctr.enable),
            row_ctr.enable.eq(should_count_row),
            bit_ctr.reset.eq(should_count_row),
        ]

        # the line time counter counts every time the line cycle counter
        # finishes a line
        m.d.comb += [
            line_times_ctr.enable.eq(
                line_cycle_ctr.value == self.num_line_cycles-1),
            line_cycle_ctr.reset.eq(
                line_cycle_ctr.value == self.num_line_cycles-1),
        ]
        # so uh the output of this doesn't go to anything anymore, but keeping
        # it improves fmax significantly (by like 20%) and saves LUTs to boot.
        # however, this only applies if this variable name "bitmasker" matches
        # the variable name "bitmasker" in the PixelBuffer module.
        # ?????
        bitmasker = Decoder(self.bpp)
        m.submodules.bitmasker = bitmasker
        m.d.comb += bitmasker.i.eq(self.o_bit)

        # we compare it with which bit of the pixel we are processing. since
        # generating that bit from the bit counter is expensive, we instead
        # rotate a bitmask to select the appropriate bit of the line counter. of
        # course, if this mask were to get out of sync with the bit counter,
        # the brightnesses would be all wrong. fortunately, that's not possible.
        line_time_delay = Signal(self.bpp, reset=1)

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

                # are we addressing the last pixel?
                with m.If(col_ctr.at_max):
                    # yes, tell the line generator what row it should update
                    # whenever it finishes
                    m.d.sync += ltg.i_line_addr.eq(self.row_ctr.value)
                    # and wait for this line to end
                    m.next = "WAIT"
                with m.Else():
                    # keep on counting the pixels throughout the line
                    m.d.comb += should_count.eq(1)
                    # once we're finished, we don't count any more so we remain
                    # on this line.

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

# buffer one channel for the whole panel in BRAM at an arbitrary bpp
class PixelBuffer(Elaboratable):
    def __init__(self, panel_shape, led_domain="sync", bpp=1):
        self.panel_shape = panel_shape
        self.bpp = bpp # number of bits per pixel
        # the memory read happens in the LED domain
        self.led_domain = led_domain

        # calculate address widths
        self.row_bits = (panel_shape[1]//2-1).bit_length()
        self.col_bits = (panel_shape[0]-1).bit_length()
        self.bit_bits = (bpp-1).bit_length()
        self.pixel_bits = self.row_bits + self.col_bits

        # write input port from the CPU or whatever
        self.i_we = Signal()
        self.i_waddr = Signal(self.pixel_bits)
        self.i_wdata = Signal(self.bpp)

        # pixel the frame generator wants to read
        self.i_row = Signal(self.row_bits)
        self.i_col = Signal(self.col_bits)
        self.i_bit = Signal(self.bit_bits)

        # what color that pixel is
        self.o_pixel = Signal()

        self.mem = Memory(width=self.bpp, depth=2**self.pixel_bits)

    def elaborate(self, platform):
        m = Module()
        # register submodules (and make a local reference without self)
        m.submodules.rdport = rdport = self.mem.read_port(
            domain=self.led_domain, transparent=False)
        m.submodules.wrport = wrport = self.mem.write_port()

        # write port goes straight to the memory
        m.d.comb += [
            wrport.en.eq(self.i_we),
            wrport.addr.eq(self.i_waddr),
            wrport.data.eq(self.i_wdata),
        ]

        # read port goes to the frame generator
        bitmasker = Decoder(self.bpp)
        m.submodules.bitmasker = bitmasker
        m.d.comb += [
            bitmasker.i.eq(self.i_bit),

            rdport.addr.eq(Cat(self.i_col, self.i_row)),
            self.o_pixel.eq((rdport.data & bitmasker.o) != 0),
        ]

        return m

# a whole screen and buffer
class BufferedHUB75(Elaboratable):
    def __init__(self, panel_shape, led_domain="sync", bpp=1,
            gamma=None, gamma_bpp=None):
        self.panel_shape = panel_shape
        # gamma correction.
        # gamma is the exponent of the gamma curve, or None to disable.
        # if gamma is enabled, bpp bits look up a gamma_bpp wide table.
        # if gamma_bpp is none, gamma_bpp = bpp
        self.gamma = gamma
        self.bpp = bpp # number of bits per color per pixel
        if gamma is not None and gamma_bpp is not None:
            self.gamma_bpp = gamma_bpp
        else:
            self.gamma_bpp = bpp

        if self.bpp < 1 or self.gamma_bpp < 1:
            raise Exception(
                "who would even have a {}-color display?".format(
                    min(2**self.bpp, 2**self.gamma_bpp)))
        elif self.bpp > 16 or self.gamma_bpp > 16:
            raise Exception("{}bpp? now that's just greedy".format(
                max(self.bpp, self.gamma_bpp)))

        if self.gamma is not None:
            # generate the gamma table from the given parameters
            in_scale = 2**self.bpp-1
            out_scale = 2**self.gamma_bpp-1
            table = []
            for x in range(2**self.bpp):
                table.append(int(((x/in_scale)**self.gamma)*out_scale))
            self.gamma_table = Memory(
                width=self.gamma_bpp, depth=2**self.bpp, init=table)

        # what domain to run the LED engine in. framebuffers will still be
        # written to from sync.
        self.led_domain = led_domain
        # calculate how wide the address components are
        self.row_bits = (self.panel_shape[1]//2-1).bit_length()
        self.col_bits = (self.panel_shape[0]-1).bit_length()
        self.pixel_bits = self.row_bits + self.col_bits
        # + 1 because each address above refers to two indepdendent pixels,
        # + 2 to select pixel color channel
        self.addr_bits = self.pixel_bits + 1 + 2

        # framebuffer write inputs.
        # the low 2 bits of the address select R, G, B, or nothing.
        # next N bits select 2**N columns (X position).
        # next M bits select 2**M rows (Y position).
        # the last bit selects screen half (RGB0 or RGB1).
        # this means the display is linearly addressed R, G, B color, left to
        # right, top to bottom
        self.i_we = Signal()
        self.i_waddr = Signal(self.addr_bits)
        self.i_wdata = Signal(self.bpp)

        # our only output is to the display, which we grab direct from the
        # platform during elaboration

        # buffer each channel of the display independently
        self.pb_names = ("r0", "g0", "b0", "r1", "g1", "b1")
        self.pbs = {}
        for name in self.pb_names:
            self.pbs[name] = PixelBuffer(panel_shape,
                led_domain=self.led_domain, bpp=self.gamma_bpp)

        # and have a mechanism to actually generate the display
        # (display generation happens in the LED clock domain)
        self.ftg = DomainRenamer(self.led_domain)(
            FrameTimingGenerator(panel_shape, bpp=self.gamma_bpp))

    def elaborate(self, platform):
        # get the display from the platform.
        # we use DDR buffer on shift clock so we can cleanly gate it.
        hub75 = platform.request("hub75", 0, xdr={"shift_clock": 2})
        
        m = Module()
        m.submodules.ftg = ftg = self.ftg
        pbs = self.pbs
        for name, buffer in pbs.items():
            # register all the buffers we made
            setattr(m.submodules, name, buffer)

        # handle the write data input
        # buffer them so we can decode at our leisure
        b_we = Signal.like(self.i_we)
        b_waddr = Signal.like(self.i_waddr)
        b_wdata = Signal.like(self.i_wdata)
        # and once more, so we can gamma correct the data
        g_we = Signal.like(self.i_we)
        g_waddr = Signal.like(self.i_waddr)
        g_wdata = Signal(self.gamma_bpp)
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

        # split the write address into the various pixel addresses
        addr_color = Signal(2)
        addr_row = Signal(self.row_bits)
        addr_col = Signal(self.col_bits)
        addr_half = Signal()
        row_bits, col_bits = self.row_bits, self.col_bits
        m.d.comb += [
            addr_color.eq(g_waddr[:2]),
            addr_col.eq(g_waddr[2:2+col_bits]),
            addr_row.eq(g_waddr[2+col_bits:2+col_bits+row_bits]),
            addr_half.eq(g_waddr[2+col_bits+row_bits]),
        ]

        # then wire up those addresses to the pixel buffers
        for name, buffer in pbs.items():
            color_en = Signal() # addressed color matches this buffer?
            half_en = Signal() # addressed display half matches this buffer?
            m.d.comb += [
                color_en.eq(addr_color == "rgb".index(name[0])),
                half_en.eq(addr_half == "01".index(name[1])),

                buffer.i_we.eq(color_en & half_en & g_we),
                buffer.i_waddr.eq(Cat(addr_col, addr_row)),
                buffer.i_wdata.eq(g_wdata),
            ]

        # tell the buffers what pixel the frame generator wants
        for name, buffer in pbs.items():
            m.d.comb += [
                buffer.i_row.eq(ftg.o_row),
                buffer.i_col.eq(ftg.o_col),
                buffer.i_bit.eq(ftg.o_bit),
            ]
        # then wire the retrieved pixel back into the frame generator
        m.d.comb += ftg.i_rgb0.eq(Cat(pbs[n+"0"].o_pixel for n in "rgb"))
        m.d.comb += ftg.i_rgb1.eq(Cat(pbs[n+"1"].o_pixel for n in "rgb"))

        # we set the panel outputs synchronously so the panel doesn't see
        # glitches as things settle. this is done in the LED domain.
        d = getattr(m.d, self.led_domain)
        d += [
            hub75.latch.eq(ftg.o_latch),
            hub75.blank.eq(ftg.o_blank),

            Cat(hub75.r0, hub75.g0, hub75.b0).eq(ftg.o_rgb0),
            Cat(hub75.r1, hub75.g1, hub75.b1).eq(ftg.o_rgb1),

            Cat(getattr(hub75, "a"+str(n)) for n in range(5)).eq(
                Cat(ftg.o_line_addr, 0))
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
