# a SPI master for Boneless

from nmigen import *
from nmigen.lib.cdc import FFSynchronizer
from nmigen.lib.fifo import SyncFIFOBuffered
from boneless.arch.opcode import *

# Register Map

# 0x0: (W) Transaction Start / (R) Status
#   Write: writing starts a transaction. it is only legal to start a transaction
#          when one is not already in progress.
#          bit 15: 1 for write transaction, 0 for read transaction
#           14-13: bus mode: 0 = previous, 1 = 1 bit, 2 = 2 bit, 3 = 4 bit
#              12: chip select
#            11-0: transaction length
#    Read: bit 15: 1 if transaction in progress, 0 otherwise. the transaction
#                  FIFO is empty/full and the bus is idle iff this bit is 0
#           14:13: current bus mode
#              12: current chip select

# 0x1: (W) Queue Write / (R) Receive Read/FIFO status
# Write:      7-0: character to write on bus. only legal if in write mode.
#  Read:   bit 15: bit 0 of read character, if RX FIFO is not empty in read mode
#          bit 14: 1 if RX fifo is empty, 0 otherwise (in read mode)
#                  1 if TX fifo is full, 0 otherwise (in write mode)
#             6-0: remaining bits of char, if RX fifo is not empty in read mode

class SetReset(Elaboratable):
    def __init__(self, parent, *, priority, initial=False):
        # if both set and reset are asserted on the same cycle, the value
        # becomes the prioritized state.
        if priority not in ("set", "reset"):
            raise ValueError("Priority must be either 'set' or 'reset', "
                "not '{}'.".format(priority))

        self.priority = priority

        self.set = Signal()
        self.reset = Signal()
        self.value = Signal(reset=initial)

        # avoid the user having to remember to add us
        parent.submodules += self

    def elaborate(self, platform):
        m = Module()

        if self.priority == "set":
            with m.If(self.set):
                m.d.sync += self.value.eq(1)
            with m.Elif(self.reset):
                m.d.sync += self.value.eq(0)
        elif self.priority == "reset":
            with m.If(self.reset):
                m.d.sync += self.value.eq(0)
            with m.Elif(self.set):
                m.d.sync += self.value.eq(1)

        return m

class SimpleSPI(Elaboratable):
    def __init__(self, fifo_depth=16):

        # boneless bus inputs. we only have two registers.
        self.i_re = Signal()
        self.i_we = Signal()
        self.i_addr = Signal(1)
        self.o_rdata = Signal(16)
        self.i_wdata = Signal(16)

        # SPI signals
        self.o_clk = Signal()
        self.o_cs = Signal(reset=1) # inverted, like usual
        self.o_mosi = Signal()
        self.i_miso = Signal()

        self.fifo = SyncFIFOBuffered(width=8, depth=fifo_depth)
    
    def elaborate(self, platform):
        m = Module()
        m.submodules.fifo = fifo = self.fifo

        # define the signals that make up the registers
        r0_txn_active = SetReset(m, priority="set")
        r0_write_txn = Signal()
        r0_bus_mode = Signal(2, reset=1)
        r0_cs = Signal()
        r0_txn_length = Signal(12)

        m.d.sync += self.o_cs.eq(r0_cs)

        # handle the boneless bus.
        read_data = Signal(16) # it expects one cycle of read latency
        read_output = Signal(16)
        # but the FIFO has that too, so we need to incorporate its output if
        # asked to.
        read_from_fifo = Signal()
        with m.If(read_from_fifo):
            m.d.comb += self.o_rdata.eq(
                Cat(fifo.r_data[1:8], read_output[8:15], fifo.r_data[0]))
            m.d.sync += read_from_fifo.eq(0)
        with m.Else():
            m.d.comb += self.o_rdata.eq(read_output)
        m.d.sync += read_output.eq(read_data)

        with m.If(self.i_re):
            with m.Switch(self.i_addr):
                with m.Case(0): # status register
                    m.d.comb += [
                        read_data[15].eq(r0_txn_active.value),
                        read_data[13:15].eq(r0_bus_mode),
                        read_data[12].eq(r0_cs),
                    ]
                with m.Case(1): # receive read/FIFO status register
                    with m.If(r0_write_txn):
                        # interested in if the write side of the fifo is full
                        m.d.comb += read_data[14].eq(~fifo.w_rdy)
                    with m.Else():
                        # interested in if the read side of the fifo is empty
                        m.d.comb += read_data[14].eq(~fifo.r_rdy)
                        with m.If(fifo.r_rdy):
                            # if it is, read it
                            m.d.comb += fifo.r_en.eq(1)
                            # and note that next cycle we need to mux in the
                            # output
                            m.d.sync += read_from_fifo.eq(1)
        with m.Elif(self.i_we):
            with m.Switch(self.i_addr):
                with m.Case(0): # transaction start register
                    with m.If(~r0_txn_active.value):
                        m.d.comb += r0_txn_active.set.eq(1)
                        m.d.sync += [
                            r0_write_txn.eq(self.i_wdata[15]),
                            r0_bus_mode.eq(Mux(self.i_wdata[13:15] == 0,
                                r0_bus_mode, self.i_wdata[13:15])),
                            r0_cs.eq(self.i_wdata[13]),
                            r0_txn_length.eq(self.i_wdata[:12]),
                        ]
                with m.Case(1): # tx queue register
                    with m.If(r0_write_txn & fifo.w_rdy):
                        m.d.comb += [
                            fifo.w_data.eq(self.i_wdata[:8]),
                            fifo.w_en.eq(1),
                        ]

        # this is a crappy state machine but at this point i just want something
        # that does actually work

        bit_ctr = Signal(range(7))
        curr_buf = Signal(8)

        with m.FSM("STOP"):
            with m.State("STOP"): # no transaction happening
                with m.If(r0_txn_active.value):
                    with m.If(r0_write_txn):
                        m.next = "WIDLE"
                    with m.Else():
                        m.next = "RIDLE"

            with m.State("WIDLE"): # write transaction, waiting for data
                with m.If(fifo.r_rdy): # have we got some?
                    # yes, go read it
                    m.d.comb += fifo.r_en.eq(1)
                    m.next = "WGET"

            with m.State("WGET"):
                # copy the FIFO byte into our buffer and start up the counters
                m.d.sync += [
                    curr_buf.eq(fifo.r_data),
                    bit_ctr.eq(7),
                ]
                m.next = "WOUTA"

            with m.State("WOUTA"):
                # the flash latches in the new value on the rising edge.
                # so output the current data bit along with a low clock
                m.d.comb += [
                    self.o_mosi.eq(curr_buf[-1]),
                    self.o_clk.eq(0),
                ]
                m.next = "WOUTB"

            with m.State("WOUTB"):
                # now that the value is output, we can latch it
                m.d.comb += [
                    self.o_mosi.eq(curr_buf[-1]),
                    self.o_clk.eq(1),
                ]
                with m.If(bit_ctr == 0):
                    with m.If(r0_txn_length == 0):
                        m.d.comb += r0_txn_active.reset.eq(1)
                        m.next = "STOP"
                    with m.Else():
                        m.d.sync += r0_txn_length.eq(r0_txn_length-1)
                        m.next = "WIDLE"
                with m.Else():
                    m.d.sync += bit_ctr.eq(bit_ctr-1)
                    m.d.sync += curr_buf.eq(curr_buf<<1)
                    m.next = "WOUTA"

            with m.State("RIDLE"): # read transaction, waiting for space
                with m.If(fifo.w_rdy): # have we got some?
                    # yes, go read from the device
                    m.d.sync += bit_ctr.eq(7)
                    m.next = "RINA"

            with m.State("RINA"):
                # the device clocks out data on the falling edge. since the edge
                # fell when the last read or write ended, start by latching in
                # the current bit.
                m.d.sync += curr_buf.eq(Cat(self.i_miso, curr_buf[:-1]))
                m.d.comb += self.o_clk.eq(0)
                m.next = "RINB"

            with m.State("RINB"):
                m.d.comb += self.o_clk.eq(1)
                with m.If(bit_ctr == 0):
                    m.d.comb += [
                        fifo.w_data.eq(curr_buf),
                        fifo.w_en.eq(1),
                    ]
                    with m.If(r0_txn_length == 0):
                        m.d.comb += r0_txn_active.reset.eq(1)
                        m.next = "STOP"
                    with m.Else():
                        m.d.sync += r0_txn_length.eq(r0_txn_length-1)
                        m.next = "RIDLE"
                with m.Else():
                    m.d.sync += bit_ctr.eq(bit_ctr-1)
                    m.next = "RINA"

        return m