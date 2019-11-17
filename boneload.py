# boneload!
# a complete bootloading solution for your boneless.
# please note that this is completely and utterly insecure, and never will be
# anything else.

# notational notes:
# boneless is a word-based architecture, it has no concept of the 8 bit byte.
# but we do, so we have to define what a word means.
# * a "word" is a 16 bit unsigned integer, transmitted and stored in
#   little-endian byte order.
# * an "address" selects one "word".

# UART command packet format:
# first word: command
#   bits 7-0: number of words to follow, excluding CRC word and command word
#   bits 15-8: command number, defined later
# next words: command parameters, as defined in command
# last word: CRC-16/KERMIT result, calculated over all preceding words, one
# word at a time.

# UART response packet format: (in response to command)
# first word: result
#   bits 7-0: number of words to follow, excluding CRC word and result word
#   bits 15-8: result code, defined later
# next words: result information, as defined in result
# last word: CRC-16/KERMIT result, calculated over all preceding words, one
# word at a time.

# "number of words to follow" for both cases, i.e. length, can only go up to a
# dictated by the free RAM in boneload, called "max length". this amount is
# indicated in the identify command so the maximum data per packet can be
# selected and used.

# commands
# command 1: identify
#   length: 0
#   parameter words: none
#   result codes: identify, invalid length
#   purpose: sends information about the system boneload is running on

# command 2: write data
#   length: 1-max length
#   parameter words: destination address, data to write*length-1
#   result codes: success, invalid length
#   purpose: writes words to arbitrary memory address. note that boneload lives
#   from $FF00 to $FFFF, and overwriting it would be bad.

# command 3: read data
#   length: 2
#   parameter words: source address, source length (up to max length)
#   result codes: read result, invalid length
#   purpose: reads words from arbitrary memory address.

# command 4: jump to code
#   length: 2
#   parameter words: destination address, new W address
#   result codes: success, execution complete, invalid length
#   purpose: jump to bootloaded code. W is loaded before the jump. entering into
#       the code, R7 will have return address and R6 will have previous W.
#       succese is before jump, execution complete is sent if jump returns.

# command 5: calculate CRC
#   length 2:
#   parameter words: start address, end address (exclusive)
#   result codes: CRC result, invalid length
#   purpose: calculate CRC-16/KERMIT of arbitrary memory region

# results
# result 1: success
#   length: 0
#   parameter words: none
#   purpose: say that everything went great

# result 2: invalid command
#   length: 1
#   parameter words: reason: 0=unknown cmd, 1=invalid length, 2=bad CRC,
#                            3=timeout
#   purpose: say that the command couldn't be processed for whatever reason.

# result 3: CRC result
#   length: 2
#   parameter words: CRC of desired region
#   purpose: give back the CRC

# result 4: read result
#   length: 0-max length
#   parameter words: the words
#   purpose: give back the read words

# result 5: identify
#   length: 3
#   parameter words: boot version (currently 1), board id, max length
#   purpose: say stuff about ourselves

from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *
from bonetools import *
import serial
import random

# crc calculation in boneless.
# on entry (in caller window):
# R7: return address
# R5: start address
# R4: end address
# on exit (in our window):
# R0: result crc
def _bfw_calc_crc():
    # generate random prefix so that we effectively can make local labels
    lp = "_{}_".format(random.randrange(2**32))
    r = RegisterManager("R6:fp R5:curr_addr R4:end_addr R3:bit_ctr "
        "R2:new_word R1:old_crc R0:crc")
    return [
        # set up register frame and load parameters
        LDW(r.fp, -8),
        LD(r.curr_addr, r.fp, 5),
        LD(r.end_addr, r.fp, 4),
        MOVI(r.crc, 0),
    L(lp+"words"),
        LD(r.new_word, r.curr_addr, 0),
        XOR(r.crc, r.crc, r.new_word), # mix new bits into CRC
        MOVI(r.bit_ctr, 16),
    L(lp+"bits"), # update CRC for every bit in the word
        MOV(r.old_crc, r.crc), # copy so we can query lowest bit
        SRLI(r.crc, r.crc, 1),
        ANDI(r.old_crc, r.old_crc, 1), # was lowest bit set?
        BZ1(lp+"nope"),
        XORI(r.crc, r.crc, 0x8408), # yes, XOR in polynomial
    L(lp+"nope"),
        SUBI(r.bit_ctr, r.bit_ctr, 1),
        BNZ(lp+"bits"), # loop through the remaining bits in this word
        ADDI(r.curr_addr, r.curr_addr, 1),
        CMP(r.curr_addr, r.end_addr),
        BNE(lp+"words"), # loop through the words we were asked to calculate

        # take down register frame and return
        ADJW(8),
        JR(R7, 0), # R7 in caller's window
    ]

# receive a packet
# on entry (in caller window):
# R7: return address
# on exit (in our window):
# R0: issues: 0 = ok, 1 = bad length, 2 = bad CRC, 3 = timeout
def _bfw_rx_packet(uart_addr, max_length, timeout_ms=500):
    # generate random prefix so that we effectively can make local labels
    lp = "_{}_".format(random.randrange(2**32))
    r = RegisterManager(
        "R7:lr R6:command R2:got_byte R1:length R0:issues")
    fw = [
        # create register frame
        ADJW(-8),
        # secretly the command word is two bytes!
        # receive the length byte first
        JAL(r.lr, lp+"rx_or_timeout"),
        MOV(r.length, r.got_byte),
        # then get the command byte
        JAL(r.lr, lp+"rx_or_timeout"),
        # and reform the command word
        SLLI(r.command, r.got_byte, 8),
        OR(r.command, r.command, r.length),
        MOVI(r.issues, 0), # by default there is no issue
        STR(r.command, r.issues, "pb_cmdresp"), # issues = 0
        ADDI(r.length, r.length, 1), # add 1 to length to account for CRC word
        # make sure we can actually fit that many words
        CMPI(r.length, max_length+1),
        BLEU(lp+"goodlength"),
        # it's too long! we would overflow something.
        MOVI(r.issues, 1),
        J(lp+"ret"),
    ]
    r -= "command"
    r += "R4:got_word R3:buf_pos"
    fw.append([
    L(lp+"goodlength"),
        # now we can start receiving the words
        MOVI(r.buf_pos, 0),
    L(lp+"rx_words"),
        # get low half
        JAL(r.lr, lp+"rx_or_timeout"),
        MOV(r.got_word, r.got_byte),
        # then high half
        JAL(r.lr, lp+"rx_or_timeout"),
        SLLI(r.got_byte, r.got_byte, 8),
        OR(r.got_word, r.got_word, r.got_byte),
        # and store to the buffer
        STR(r.got_word, r.buf_pos, "pb_data"),
        ADDI(r.buf_pos, r.buf_pos, 1),
        CMP(r.buf_pos, r.length),
        BNE(lp+"rx_words"),
    ])
    r -= "buf_pos got_word"
    r += "R3:calc_crc R5:crc_start R4:crc_end"
    fw.append([
        # calculate what the buffer's CRC actually is
        MOVR(r.crc_start, "pb_cmdresp"),
        ADD(r.crc_end, r.crc_start, r.length),
        JAL(r.lr, "calc_crc"),
        LDW(r.calc_crc, 0),
        LD(r.calc_crc, r.calc_crc, -8+0)
    ])
    r -= "crc_start crc_end"
    r += "R4:got_crc"
    fw.append([
        # and compare it with what it should be
        LDR(r.got_crc, r.length, "pb_cmdresp"),
        CMP(r.calc_crc, r.got_crc),
        BEQ(lp+"ret"),
        # if they don't match, signal CRC error
        MOVI(r.issues, 2),
    L(lp+"ret"),
        ADJW(8),
        JR(R7, 0), # R7 in caller's window
    ])
    r -= "calc_crc got_crc"
    r += "R5:timeout_ctr_hi R6:timeout_ctr_lo"
    # 6 insns at ~4 cycles per insn
    timeout = int((12e6*(timeout_ms/1e3))//(4*6))
    fw.append([
    L(lp+"rx_or_timeout"),
        # set up time counters with precalculated duration
        MOVI(r.timeout_ctr_hi, (timeout>>16)+1),
        MOVI(r.timeout_ctr_lo, timeout&0xFFFF),
    L(lp+"byte_rx"),
        # get potential byte from the peripheral
        LDXA(r.got_byte, uart_addr+3),
        # make sure we've got a real byte
        ROLI(r.got_byte, r.got_byte, 1),
        BS0(lp+"byte_done"), # and we can return with it
        # otherwise, count down the timeout
        SUBI(r.timeout_ctr_lo, r.timeout_ctr_lo, 1),
        SBCI(r.timeout_ctr_hi, r.timeout_ctr_hi, 0),
        BNZ(lp+"byte_rx"),
        # if the high half reached zero, the timeout is over.
        MOVI(r.issues, 3),
        J(lp+"ret"),
    L(lp+"byte_done"),
        JR(r.lr, 0),
    ])

    return fw

# transmit a packet
# on entry (in caller window):
# R7: return address
# R5: result word
# on exit (in our window):
# nothing of significance
def _bfw_tx_packet(uart_addr):
    # generate random prefix so that we effectively can make local labels
    lp = "_{}_".format(random.randrange(2**32))
    r = RegisterManager("R7:lr R6:fp "
        "R5:crc_start R4:crc_end R3:buf_ptr R2:result_word R1:crc R0:length")
    fw = [
        # get the passed in result word
        LDW(r.fp, -8),
        LD(r.result_word, r.fp, 5),
        # and store it to the buffer
        MOVI(r.buf_ptr, 0),
        STR(r.result_word, r.buf_ptr, "pb_cmdresp"),

        ANDI(r.length, r.result_word, 0xFF), # get length from result
        ADDI(r.length, r.length, 1), # bump length to include response word
        # calculate CRC of the packet
        MOVR(r.crc_start, "pb_cmdresp"),
        ADD(r.crc_end, r.crc_start, r.length),
        JAL(r.lr, "calc_crc"),
        LD(r.crc, r.fp, -16+0),
        # and store it at the end of the buffer
        STR(r.crc, r.length, "pb_cmdresp"),
    ]
    r -= "crc_start crc_end result_word crc"
    r += "R2:tx_space R1:tx_word"
    fw.append([
        # actually transmit the packet
    L(lp+"tx"),
        LDR(r.tx_word, r.buf_ptr, "pb_cmdresp"), # get this word from the buffer
        JAL(r.lr, lp+"tx_byte"), # transmit the low byte
        SRLI(r.tx_word, r.tx_word, 8), # get high byte of word
        JAL(r.lr, lp+"tx_byte"), # then send it
        ADDI(r.buf_ptr, r.buf_ptr, 1),
        CMP(r.buf_ptr, r.length), # done with the buffer?
        BLEU(lp+"tx"), # include case when length = buffer to tx CRC too

        ADJW(8),
        JR(R7, 0), # R7 in caller's window

    L(lp+"tx_byte"),
        # wait until the transmit fifo has space
        LDXA(r.tx_space, uart_addr+2),
        ANDI(r.tx_space, r.tx_space, 1),
        BZ0(lp+"tx_byte"),
        # then send the byte
        ANDI(r.tx_space, r.tx_word, 0xFF),
        STXA(r.tx_space, uart_addr+2),
        JR(R7, 0),
    ])

    return fw

def _bfw_main(uart_addr):
    max_length = 16 # adjust to fill memory
    fw = []

    r = RegisterManager("R7:lr R6:fp "
        "R5:result_code R4:command R1:buf_ptr R0:length")
    fw.append([
    L("sys_packet_rx"),
        LDW(r.fp, 0), # fetch window so we can get return values
        # receive a new packet from whatever's bootloading us
        JAL(r.lr, "rx_packet"),
        LD(r.result_code, r.fp, -8+0),
        # load LR with the top of the rx loop so that subfunctions can just jump
        # to the tx packet routine and have it return correctly.
        MOVR(r.lr, "sys_packet_rx"),
        # and load the pointer to the buffer so the subfunctions can easily
        # access it.
        MOVR(r.buf_ptr, "pb_data"),
        # if the result was nonzero, there was some issue
        AND(r.result_code, r.result_code, r.result_code),
        BNZ("sys_packet_tx_issue"),
        # otherwise, dispatch the command
        # a switch table would be nice, but we can't actually declare one yet
        LDR(r.command, r.result_code, "pb_cmdresp"), # (we know result_code = 0)
        ANDI(r.length, r.command, 0xFF),
        SRLI(r.command, r.command, 8),
        CMPI(r.command, 1),
        BEQ("sys_cmd_identify"),
        CMPI(r.command, 2),
        BEQ("sys_cmd_write_data"),
        CMPI(r.command, 4),
        BEQ("sys_cmd_jump_to_code"),
        # oh no, we don't know what the command is
        # fortunately, a result of 0 also = bad command
        J("sys_packet_tx_issue"),
    L("sys_packet_tx_invalid_length"),
        MOVI(r.result_code, 1),
        # fall through to tx issue packet
    ])
    r -= "fp command"
    r += "R6:zero"
    fw.append([
    L("sys_packet_tx_issue"), # send error packet with problem in result_code
        # store the problem into the packet
        MOVI(r.zero, 0),
        STR(r.result_code, r.zero, "pb_data"),
        # result code 2 with length 1
        MOVI(r.result_code, 0x0201),
        J("tx_packet"), # LR is set to return to sys_packet_rx
    L("sys_packet_tx_success"), # say everything went great
        # result code 1 with length 0
        MOVI(r.result_code, 0x0100),
        J("tx_packet"), # LR is set to return to sys_packet_rx
    ])
    r -= "zero"
    r += "R6:ident_info"
    fw.append([
    L("sys_cmd_identify"),
        # we don't expect any additional information
        CMPI(r.length, 0),
        BNE("sys_packet_tx_invalid_length"),
        # write identification data to buffer
        MOVR(r.buf_ptr, "pb_data"),
        MOVI(r.ident_info, 1), # boot version
        ST(r.ident_info, r.buf_ptr, 0),
        MOVI(r.ident_info, 0x69), # board id
        ST(r.ident_info, r.buf_ptr, 1),
        MOVI(r.ident_info, max_length), # max packet len
        ST(r.ident_info, r.buf_ptr, 2),
        # result code 5 with length 3
        MOVI(r.result_code, 0x0503),
        J("tx_packet"), # LR is set to return to sys_packet_rx
    ])
    r -= "ident_info"
    r += "R6:dest_addr R3:copy_tmp"
    fw.append([
    L("sys_cmd_write_data"),
        # write some data to some address
        CMPI(r.length, 0),
        BEQ("sys_packet_tx_invalid_length"), # we need at least an address
        LD(r.dest_addr, r.buf_ptr, 0), # dest addr
        MOVI(r.buf_ptr, 1), # current pos
    L("_scwd_copy"),
        LDR(r.copy_tmp, r.buf_ptr, "pb_data"),
        ST(r.copy_tmp, r.dest_addr, 0),
        ADDI(r.dest_addr, r.dest_addr, 1),
        ADDI(r.buf_ptr, r.buf_ptr, 1),
        CMP(r.buf_ptr, r.length),
        BNE("_scwd_copy"),
        J("sys_packet_tx_success"),
    ])
    r -= "dest_addr copy_tmp"
    r += "R6:dest_code R4:dest_w"
    fw.append([
    L("sys_cmd_jump_to_code"),
        # jump to some downloaded code
        CMPI(r.length, 2),
        BNE("sys_packet_tx_invalid_length"),
        LD(r.dest_code, r.buf_ptr, 0),
        LD(r.dest_w, r.buf_ptr, 1),
        # tell the host that we successfully got everything before we jump into
        # the app code
        MOVI(r.result_code, 0x0100),
        JAL(r.lr, "tx_packet"),
        XCHW(R7, r.dest_w), # set new W and store current one
        LD(R7, R7, int(r.dest_code)), # get jump destination from current frame
        JR(R7, 0), # and jump to it
    ])

    fw.append([ # declare subroutines
    L("calc_crc"),
        _bfw_calc_crc(),
    L("rx_packet"),
        _bfw_rx_packet(uart_addr, max_length),
    L("tx_packet"),
        _bfw_tx_packet(uart_addr),
    ])

    # set up labels for packet buffer and reserve space so that we ensure we
    # don't overwrite something else while using it.
    fw.append([
    L("pb_cmdresp"), 0,
    L("pb_data"), [0]*(max_length+1), # account for CRC word
    ])

    # reserve 3 register windows so we can call subroutines and be sure we won't
    # hit something below.
    fw.append([0]*(3*8))

    return fw

def boneload_fw(uart_addr=0):
    return Instr.assemble(_bfw_main(uart_addr))

# implementation taken from crcany
def _crc(words):
    crc = 0
    for word in words:
        crc ^= word
        for bi in range(16):
            if crc & 1:
                crc = (crc >> 1) ^ 0x8408
            else:
                crc >>= 1
    return crc

class BLError(Exception): pass

class BadCRC(BLError):
    def __init__(self, expected, received):
        self.expected = expected
        self.received = received

    def __repr__(self):
        return "BadCRC(expected=0x{:4X}, received=0x{:4X})".format(
            self.expected, self.received)

    def __str__(self):
        return "Bad CRC: expected 0x{:04X} but received 0x{:04X}".format(
            self.expected, self.received)

class Timeout(BLError): pass

def ser_read(ser, length):
    read = b""
    while length > 0:
        new = ser.read(length)
        if len(new) == 0:
            raise Timeout("read timeout")
        read += new
        length -= len(new)
    return read

# send the given command words, then receive the response words (and check CRC)
def _bl_transact(ser, command):
    for word in command:
        ser.write(word.to_bytes(2, byteorder="little"))

    response = []
    # secretly the first word is two bytes
    length = ser_read(ser, 1)[0]
    response.append((ser_read(ser, 1)[0]<<8) + length)
    length += 1 # include CRC word
    for wi in range(length):
        response.append(int.from_bytes(ser_read(ser, 2), byteorder="little"))

    crc = _crc(response[:-1])
    if crc != response[-1]:
        raise BadCRC(crc, response[-1])
    return response[:-1]

# send a given command with its parameters, then check for common results
def _bl_command(ser, command, params):
    words = []
    words.append((command<<8) + len(params))
    words.extend(params)
    words.append(_crc(words))
    words = _bl_transact(ser, words)
    response = words[0] >> 8
    response_len = words[0] & 0xFF
    if response_len != len(words)-1:
        raise BLError("was told to expect {} words but got {} words".format(
            response_len, len(words)-1))
    if response == 2:
        problems = {0: "unknown command", 1: "invalid length",
            2: "bad CRC", 3: "timeout"}
        raise BLError("was told: '{}'".format(
            problems.get(words[1], int(words[1]))))
    return response, words[1:]

def _bl_identify(ser):
    return _bl_command(ser, 1, [])[1]

def _bl_write_data(ser, addr, data, max_len):
    written = 0
    while written < len(data):
        to_write = max_len-1 # save room for address
        r, p = _bl_command(ser, 2, (addr+written,
            *data[written:written+to_write]))
        if r != 1:
            raise Exception("huh? {} {}".format(r, p))
        written += to_write

def _bl_jump_to_code(ser, addr, w):
    r, p = _bl_command(ser, 4, [addr, w])
    if r != 1:
        raise Exception("huh? {} {}".format(r, p))

# boneload the given firmware to the given port. firmware should be a list of
# integers (each one is one word) and the port should be a string that can be
# given to pyserial.
def boneload(firmware, port):
    import serial
    print("Connecting...")
    ser = serial.Serial(port, 115200, timeout=0.1)
    print("Identifying (reset board please)...")
    while True:
        try:
            ident = _bl_identify(ser)
            break
        except Timeout:
            pass

    if ident[0] != 1:
        raise Exception("incompatible version {}".format(ident[0]))

    print("Identified! Board ID=0x{:02X}, max length={}".format(*ident[1:]))
    print("Downloading program...")
    _bl_write_data(ser, 0, Instr.assemble(firmware), ident[2])
    print("Beginning execution...")
    _bl_jump_to_code(ser, 0, 0xFFF)
    print("Complete!")

if __name__ == "__main__":
    print(len(boneload_fw()))