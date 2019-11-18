# boneless CPU architecture stuff
from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *
from bonetools import *

import boneload

def firmware():
    fw = []
    r = RegisterManager()
    fw.append(L("main"))

    r += "R7:lr R6:status R5:data R4:rd0 R3:rd1 R2:rd2 R1:lr2 R0:wait_status"
    fw.append([
        # wait for character so we can be sure the host is listening
    L("host_listen_wait"),
        LDXA(r.status, 3),
        ROLI(r.status, r.status, 1),
        BS1("host_listen_wait"),

        # indicate that we are identifying the chip
        MOVI(r.data, ord("I")),
        JAL(r.lr, "uart_tx"),

        # start transaction to turn on the chip
        MOVI(r.data, (1<<15)+(1<<12)+1), # write txn, length 1, deassert CS
        STXA(r.data, 4),
        MOVI(r.data, 0xAB),
        STXA(r.data, 5),
        JAL(r.lr, "txn_wait"),

        # wait a bunch of time for the chip to get back up
        MOVI(r.data, 255),
    L("powerup_wait"),
        SUBI(r.data, r.data, 1),
        BNZ("powerup_wait"),

        # start transaction asking for the JEDEC ID
        # this is command 0x9F of length 1
        MOVI(r.data, (1<<15) + 1), # write txn, length 1
        STXA(r.data, 4),
        MOVI(r.data, 0x9F),
        STXA(r.data, 5),
        JAL(r.lr, "txn_wait"),

        # it responds with three bytes, which we forward to the UART
        MOVI(r.data, (1<<12)+3), # read txn, length 3, deassert CS
        STXA(r.data, 4),
        JAL(r.lr, "read_fifo"),
        MOV(r.rd0, r.data),
        JAL(r.lr, "read_fifo"),
        MOV(r.rd1, r.data),
        JAL(r.lr, "read_fifo"),
        MOV(r.rd2, r.data),

        MOV(r.data, r.rd0),
        JAL(r.lr, "uart_tx"),
        MOV(r.data, r.rd1),
        JAL(r.lr, "uart_tx"),
        MOV(r.data, r.rd2),
        JAL(r.lr, "uart_tx"),

        # indicate that we are erasing a part of the chip
        MOVI(r.data, ord("E")),
        JAL(r.lr, "uart_tx"),

        # the bitstream takes the first 128K flash = 32 sectors.
        # we test dealing with sector #32, address 0x20000 (128K in)

        JAL(r.lr, "write_enable"),

        MOVI(r.data, (1<<15)+(1<<12)+4), # write txn, length 4, deassert CS
        STXA(r.data, 4),
        MOVI(r.data, 0x20),
        STXA(r.data, 5),
        MOVI(r.data, 2),
        STXA(r.data, 5),
        MOVI(r.data, 0),
        STXA(r.data, 5),
        STXA(r.data, 5),
        JAL(r.lr, "txn_wait"),

        # indicate that we are waiting for the chip to not be busy
        MOVI(r.data, ord("B")),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "flash_busy_wait"),

        # indicate that we are going to read from the chip
        MOVI(r.data, ord("R")),
        JAL(r.lr, "uart_tx"),

        # start fast read (so we can go MAXIMUM SPEED)
        MOVI(r.data, (1<<15)+5), # write txn, length 5
        STXA(r.data, 4),
        MOVI(r.data, 0x0B),
        STXA(r.data, 5),
        # start at address 0x20000 (128K in)
        MOVI(r.data, 2),
        STXA(r.data, 5),
        MOVI(r.data, 0),
        STXA(r.data, 5),
        STXA(r.data, 5),
        STXA(r.data, 5), # dummy byte
        JAL(r.lr, "txn_wait"),

        # read five bytes
        MOVI(r.data, (1<<12)+5), # read txn, length 5, deassert CS
        STXA(r.data, 4),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),

        # indicate that we are trying to write
        MOVI(r.data, ord("W")),
        JAL(r.lr, "uart_tx"),

        JAL(r.lr, "write_enable"),
        MOVI(r.data, (1<<15)+(1<<12)+9), # write txn, length 9, deassert CS
        STXA(r.data, 4),
        MOVI(r.data, 0x02),
        STXA(r.data, 5),
        MOVI(r.data, 2),
        STXA(r.data, 5),
        MOVI(r.data, 0),
        STXA(r.data, 5),
        STXA(r.data, 5),
        MOVI(r.data, ord('h')),
        STXA(r.data, 5),
        MOVI(r.data, ord('e')),
        STXA(r.data, 5),
        MOVI(r.data, ord('l')),
        STXA(r.data, 5),
        STXA(r.data, 5),
        MOVI(r.data, ord('o')),
        STXA(r.data, 5),
        JAL(r.lr, "txn_wait"),

        # indicate that we are waiting for the chip to not be busy
        MOVI(r.data, ord("B")),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "flash_busy_wait"),

        # indicate that we are going to read from the chip
        MOVI(r.data, ord("R")),
        JAL(r.lr, "uart_tx"),

        # start fast read (so we can go MAXIMUM SPEED)
        MOVI(r.data, (1<<15)+5), # write txn, length 5
        STXA(r.data, 4),
        MOVI(r.data, 0x0B),
        STXA(r.data, 5),
        # start at address 0x20000 (128K in)
        MOVI(r.data, 2),
        STXA(r.data, 5),
        MOVI(r.data, 0),
        STXA(r.data, 5),
        STXA(r.data, 5),
        STXA(r.data, 5), # dummy byte
        JAL(r.lr, "txn_wait"),


        # read five bytes
        MOVI(r.data, (1<<12)+5), # read txn, length 5, deassert CS
        STXA(r.data, 4),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),
        JAL(r.lr, "read_fifo"),
        JAL(r.lr, "uart_tx"),

        J("main"), # do it again

    L("txn_wait"),
        LDXA(r.wait_status, 4),
        ANDI(r.wait_status, r.wait_status, 0x8000),
        BS1("txn_wait"),
        JR(r.lr, 0),

    L("read_fifo"),
        LDXA(r.data, 5),
        ROLI(r.data, r.data, 1),
        BS1("read_fifo"),
        JR(r.lr, 0),

    L("uart_tx"),
        LDXA(r.wait_status, 2),
        ANDI(r.wait_status, r.wait_status, 1),
        BNZ("uart_tx"),
        STXA(r.data, 2),
        JR(r.lr, 0),

    L("flash_busy_wait"),
        # start reading the status register by sending command
        MOV(r.lr2, r.lr),
        MOVI(r.data, (1<<15)+1), # write txn, length 1
        STXA(r.data, 4),
        MOVI(r.data, 0x05),
        STXA(r.data, 5),
        JAL(r.lr, "txn_wait"),
    L("fbw_loop"),
        # read the status register once
        MOVI(r.data, 1),
        STXA(r.data, 4),
        JAL(r.lr, "read_fifo"),
        ANDI(r.data, r.data, 1),
        BZ0("fbw_loop"), # busy bit is 1
        # deassert CS to finish command
        MOVI(r.data, (1<<12)),
        STXA(r.data, 4),
        JR(r.lr2, 0),


    L("write_enable"),
        MOVI(r.data, (1<<15)+(1<<12)+1), # write txn, length 1, deassert CS
        STXA(r.data, 4),
        MOVI(r.data, 0x06),
        STXA(r.data, 5),
        J("txn_wait"), # tail call
    ])

    return fw

if __name__ == "__main__":
    from cli import main
    def make(simulating):
        raise Exception("this is code only!")

    def fw():
        return firmware()

    main(maker=make, fw=fw, build_args={"synth_opts": "-abc9"})
