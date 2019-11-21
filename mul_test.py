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

    r += ("R7:lr R6:status "
        "R5:result R4:num3 R3:num2 R2:num1 R1:data R0:wait_status")
    fw.append([
        # wait for character so we can be sure the host is listening
    L("host_listen_wait"),
        LDXA(r.status, 3),
        ROLI(r.status, r.status, 1),
        BS1("host_listen_wait"),

        # say we're going to do an operation
        MOVI(r.data, ord("A")),
        JAL(r.lr, "uart_tx"),

        # multiply 3 by 2
        MOVI(r.num1, 65521),
        MOVI(r.num2, 7),
        STXA(r.num1, 32),
        STXA(r.num2, 33),
        LDXA(r.result, 32+5), # result should be 0x37FC

        # and then transmit it
        ANDI(r.data, r.result, 0xFF),
        JAL(r.lr, "uart_tx"),
        SRLI(r.data, r.result, 8),
        JAL(r.lr, "uart_tx"),


        # and do it all again
        J("main"),

    L("uart_tx"),
        LDXA(r.wait_status, 2),
        ANDI(r.wait_status, r.wait_status, 1),
        BNZ("uart_tx"),
        STXA(r.data, 2),
        JR(r.lr, 0),
    ])

    return fw

if __name__ == "__main__":
    from cli import main
    def make(simulating):
        raise Exception("this is code only!")

    def fw():
        return firmware()

    main(maker=make, fw=fw, build_args={"synth_opts": "-abc9"})
