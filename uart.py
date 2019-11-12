# a UART for Boneless

from nmigen import *
from boneless.arch.opcode import *

# Register Map

# 0x0: (W) Baud Rate / (R) Status
#   Write:   15-0: Baud Rate divisor. rate = ((input freq/2)/divisor)-1
#    Read: bit 15: 1 if transmission in progress, 0 otherwise
#          bit  0: 1 if reception in progress, 0 otherwise

# 0x1: (R/W) Error
#     R/W: bit 15: 1 if reception encountered framing error. write 1 to reset.
#          bit  1: 1 if transmit FIFO overflowed. write 1 to reset.
#          bit  0: 1 if receive FIFO overflowed. write 1 to reset.

# 0x2: (R/W) Transmit Data / TX FIFO Status
#    Read: bit 15: 1 if the TX FIFO is full, 0 otherwise.
#   Write:    n-0: queue written character for transmission.

# 0x3: (R) RX FIFO Status and Receive Data
#    Read: bit 15: 1 if the RX FIFO is empty, 0 otherwise
#             n-0: read character, if the RX FIFO is not empty

class SetReset(Elaboratable):
    def __init__(self, module, prefer_set=True):
        # prefer_set: if True, set takes priority over reset
        self.prefer_set = prefer_set

        self.set = Signal()
        self.reset = Signal()
        self.value = Signal()

        # add ourselves. removes the need for the user to add us.
        module.submodules += self

    def elaborate(self, platform):
        m = Module()

        if self.prefer_set:
            with m.If(self.set):
                m.d.sync += self.value.eq(1)
            with m.Elif(self.reset):
                m.d.sync += self.value.eq(0)
        else:
            with m.If(self.reset):
                m.d.sync += self.value.eq(0)
            with m.Elif(self.set):
                m.d.sync += self.value.eq(1)

        return m

def calculate_divisor(freq, baud):
    return int(freq/(2*baud))-1

class SimpleUART(Elaboratable):
    def __init__(self, default_divisor=0, char_bits=8):
        self.default_divisor = default_divisor
        self.char_bits = char_bits
        if char_bits > 15 or char_bits < 1:
            raise ValueError("char width '{}' not in 1-15".format(char_bits))

        # boneless bus inputs. we only have four registers.
        self.i_re = Signal()
        self.i_we = Signal()
        self.i_addr = Signal(2)
        self.o_rdata = Signal(16)
        self.i_wdata = Signal(16)

        # UART signals
        self.i_rx = Signal()
        self.o_tx = Signal(reset=1) # inverted, like usual

    def elaborate(self, platform):
        m = Module()

        # define the signals that make up the registers
        r0_baud_divisor = Signal(16, reset=self.default_divisor)
        r0_tx_active = Signal()
        r0_rx_active = Signal()

        r1_rx_error = SetReset(m, prefer_set=True)
        r1_tx_overflow = SetReset(m, prefer_set=True)
        r1_rx_overflow = SetReset(m, prefer_set=True)

        r2_tx_full = SetReset(m, prefer_set=False)
        r2_tx_data = Signal(self.char_bits)

        r3_rx_empty = SetReset(m, prefer_set=False)
        r3_rx_data = Signal(self.char_bits)


        # handle the boneless bus.
        read_data = Signal(16) # it expects one cycle of read latency
        m.d.sync += self.o_rdata.eq(read_data)

        with m.If(self.i_re):
            with m.Switch(self.i_addr):
                with m.Case(0): # status register
                    m.d.comb += [
                        read_data[15].eq(r0_tx_active),
                        read_data[0].eq(r0_rx_active),
                    ]
                with m.Case(1): # error register
                    m.d.comb += [
                        read_data[15].eq(r1_rx_error.value),
                        read_data[1].eq(r1_tx_overflow.value),
                        read_data[0].eq(r1_rx_overflow.value),
                    ]
                with m.Case(2): # tx fifo status register
                    m.d.comb += read_data[15].eq(r2_tx_full.value)
                with m.Case(3): # rx fifo status + read data register
                    # we don't really have a FIFO, just a buffer register and an
                    # input shift register. so do FIFO-type logic here.
                    m.d.comb += read_data[15].eq(r3_rx_empty.value)
                    # even if the buffer is "empty", there's still bits there.
                    # read them out so we don't have to have another mux.
                    m.d.comb += read_data[:self.char_bits].eq(r3_rx_data)
                    # and since we have read the only thing out of the buffer,
                    # it's now empty
                    m.d.comb += r3_rx_empty.set.eq(1)
        with m.Elif(self.i_we):
            with m.Switch(self.i_addr):
                with m.Case(0): # baud rate register
                    m.d.sync += r0_baud_divisor.eq(self.i_wdata)
                with m.Case(1): # error register
                    m.d.comb += [
                        r1_rx_error.reset.eq(self.i_wdata[15]),
                        r1_tx_overflow.reset.eq(self.i_wdata[1]),
                        r1_rx_overflow.reset.eq(self.i_wdata[0]),
                    ]
                with m.Case(2): # transmit data register
                    # we don't really have a FIFO, just a staging register and
                    # an output shift register. so do FIFO-type logic here.
                    with m.If(~r2_tx_full.value):
                        m.d.sync += r2_tx_data.eq(
                            self.i_wdata[:self.char_bits])
                        # we've put something in the staging register, so we
                        # can't accept anything else.
                        m.d.comb += r2_tx_full.set.eq(1)
                    with m.Else(): # overflowed! drop the write and raise error.
                        m.d.comb += r1_tx_overflow.set.eq(1)


        # transmit data (written in a function to keep locals under control)
        def tx():
            # count out the bits we're sending (including start and stop)
            bit_ctr = Signal(range(self.char_bits+2))
            # shift out the data bits and stop bit
            out_buf = Signal(self.char_bits+1)
            # count cycles per half baud
            baud_ctr = Signal(16)

            # send out a new bit when the state machine requests it
            send_bit = Signal()
            with m.If(send_bit):
                m.d.sync += [
                    self.o_tx.eq(out_buf[0]),
                    out_buf.eq(out_buf >> 1),
                ]

            # automatically count down the time per half baud
            baud_ctr_reset = Signal()
            baud_ctr_done = Signal()
            m.d.comb += baud_ctr_done.eq(baud_ctr == 0)
            with m.If(baud_ctr_reset):
                m.d.sync += baud_ctr.eq(r0_baud_divisor)
            with m.Elif(~baud_ctr_done):
                m.d.sync += baud_ctr.eq(baud_ctr-1)

            with m.FSM("IDLE"):
                with m.State("IDLE"):
                    # once again, no real FIFO. if it's full, then there's
                    # something to transmit.
                    with m.If(r2_tx_full.value):
                        # and once we start transmitting, it's empty.
                        m.d.comb += r2_tx_full.reset.eq(1)
                        m.d.sync += [
                            # load data to send, plus stop bit
                            out_buf.eq(Cat(r2_tx_data, 1)),
                            # start counting down the bits
                            bit_ctr.eq(self.char_bits+2-1),
                            # send the start bit first
                            self.o_tx.eq(0),
                            # finally, let it be known that we are sending
                            r0_tx_active.eq(1),
                        ]
                        # start counting down this baud time
                        m.d.comb += baud_ctr_reset.eq(1)
                        m.next = "BAUD0"

                with m.State("BAUD0"):
                    # nothing to do here, just passing the time...
                    with m.If(baud_ctr_done):
                        m.d.comb += baud_ctr_reset.eq(1)
                        m.next = "BAUD1"

                with m.State("BAUD1"):
                    # but once the half baud timer expires here, we need to send
                    # out a new bit.
                    with m.If(baud_ctr_done):
                        # are we on the last bit?
                        with m.If(bit_ctr == 0):
                            m.d.sync += [
                                # yes, we are done!
                                r0_tx_active.eq(0),
                                # the stop bit leaves the bus idle
                            ]
                            m.next = "IDLE"
                        with m.Else():
                            # nope. shift out the next one and wait for the
                            # appropriate time.
                            m.d.comb += [
                                send_bit.eq(1),
                                baud_ctr_reset.eq(1),
                            ]
                            # one less bit to go
                            m.d.sync += bit_ctr.eq(bit_ctr-1)
                            m.next = "BAUD0"

        tx()

        return m
