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
    r = RegisterManager("R6:frame_ptr R5:curr_addr R4:end_addr R3:bit_ctr "
        "R2:new_word R1:old_crc R0:crc")
    return [
        # set up register frame and load parameters
        LDW(r.frame_ptr, -8),
        LD(r.curr_addr, r.frame_ptr, 5),
        LD(r.end_addr, r.frame_ptr, 4),
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

# receive one byte from the UART with constant timeout (in ms, approx)
# on entry (in caller window):
# R7: return address
# on exit (in our window):
# R0: gotten byte ROR 1 (or 14th bit set + junk if timeout)
def _bfw_rx_byte(uart_addr, timeout=500):
    timeout = int((12e6*(500/1e3))//(4*6)) # 6 insns at ~4 cycles per insn
    # generate random prefix so that we effectively can make local labels
    lp = "_{}_".format(random.randrange(2**32))
    timeout_ctr_hi = R5
    timeout_ctr_lo = R4
    temp1 = R3
    got_byte = R0
    return [
        # create register frame. we don't need a frame pointer since we don't
        # have any parameters.
        ADJW(-8),

        # set up time counters with precalculated duration
        MOVI(timeout_ctr_hi, (timeout>>16)+1),
        MOVI(timeout_ctr_lo, timeout&0xFFFF),
    L(lp+"rx"),
        # get potential byte from the peripheral
        LDXA(got_byte, uart_addr+3),
        # make sure we've got a real byte
        # (but don't return the rotated version; we want our caller to be able
        # to check the same way!)
        ROLI(temp1, got_byte, 1),
        BS0(lp+"done"), # and we can return with it
        # otherwise, count down the timeout
        SUBI(timeout_ctr_lo, timeout_ctr_lo, 1),
        SBCI(timeout_ctr_hi, timeout_ctr_hi, 0),
        # if the high half reached zero, the timeout is over. but the got byte
        # already has the appropriate bit set since we just read the peripheral.
        # so we can just fall through and return.
        BNZ(lp+"rx"),
    L(lp+"done"),
        # byte is already where it needs to be, so take down register frame and
        # return
        ADJW(8),
        JR(R7, 0), # R7 in caller's window
    ]

# receive a packet
# on entry (in caller window):
# R7: return address
# on exit (in our window):
# R0: issues: 0 = ok, 1 = bad length, 2 = bad CRC, 3 = timeout
def _bfw_rx_packet(max_length):
    # generate random prefix so that we effectively can make local labels
    lp = "_{}_".format(random.randrange(2**32))
    return_addr = R7
    frame_ptr = R6
    buf_pos = R5
    got_word = R4
    temp1 = R3
    temp2 = R2
    length = R1
    issues = R0
    return [
        # create register frame (we need the pointer to access return values)
        LDW(frame_ptr, -8),

        MOVI(issues, 0),
        # secretly the command word is two bytes!
        # receive the length byte first
        JAL(temp1, lp+"rx_or_timeout"),
        MOV(length, temp2),
        # then get the command byte
        JAL(temp1, lp+"rx_or_timeout"),
        # and reform the command word
        SLLI(temp2, temp2, 8),
        OR(length, length, temp2),
        STR(length, issues, "pb_cmdresp"), # issues = 0
        # now get the actual length back
        ANDI(length, length, 0xFF),
        ADDI(length, length, 1), # add 1 to account for CRC word
        # make sure we can actually fit the length
        CMPI(length, max_length+1),
        BLEU(lp+"goodlength"),
        # it's too long! we would overflow something.
        MOVI(issues, 1),
        J(lp+"ret"),
    L(lp+"goodlength"),
        # now we can start receiving the words
        MOVI(buf_pos, 0),
    L(lp+"rx_words"),
        # get low half
        JAL(temp1, lp+"rx_or_timeout"),
        MOV(got_word, temp2),
        # then high half
        JAL(temp1, lp+"rx_or_timeout"),
        SLLI(temp2, temp2, 8),
        OR(got_word, got_word, temp2),
        # and store to the buffer
        STR(got_word, buf_pos, "pb_data"),
        ADDI(buf_pos, buf_pos, 1),
        CMP(buf_pos, length),
        BNE(lp+"rx_words"),

        # get CRC out of buffer
        LDR(temp1, length, "pb_cmdresp"),
        # then calculate what the buffer's CRC actually is
        MOVR(R5, "pb_cmdresp"),
        ADD(R4, R5, length),
        JAL(return_addr, "calc_crc"),
        # and compare it with what it should be
        LD(temp2, frame_ptr, -16+0),
        CMP(temp1, temp2),
        BEQ(lp+"ret"),
        # if they don't match, signal CRC error
        MOVI(issues, 2),
    L(lp+"ret"),
        ADJW(8),
        JR(R7, 0), # R7 in caller's window
    L(lp+"rx_or_timeout"),
        JAL(return_addr, "rx_byte"),
        LD(temp2, frame_ptr, -16+0),
        ROLI(temp2, temp2, 1),
        BS1(lp+"timedout"),
        JR(temp1, 0),
    L(lp+"timedout"),
        MOVI(issues, 3),
        J(lp+"ret"),
    ]

# transmit a packet
# on entry (in caller window):
# R7: return address
# R5: packet length (excluding CRC and result word)
# R4: result code
# on exit (in our window):
# nothing of significance
def _bfw_tx_packet(uart_addr):
    # generate random prefix so that we effectively can make local labels
    lp = "_{}_".format(random.randrange(2**32))
    return_addr = R7
    frame_ptr = R6
    result = R4
    length = R3
    crc = R2
    temp1 = R1
    buf_ptr = R0
    return [
        # create register frame and load parameters
        LDW(frame_ptr, -8),
        LD(length, frame_ptr, 5),
        LD(result, frame_ptr, 4),

        # pack up the result word
        SLLI(temp1, result, 8),
        OR(temp1, temp1, length),
        MOVI(crc, 0),
        # and store it to the buffer
        STR(temp1, crc, "pb_cmdresp"),

        ADDI(length, length, 1), # bump length to include response word
        # calculate CRC of the packet
        MOVR(R5, "pb_cmdresp"),
        ADD(R4, R5, length),
        JAL(R7, "calc_crc"),
        LD(crc, frame_ptr, -16+0),
        # and store it at the end of the buffer
        STR(crc, length, "pb_cmdresp"),

        # actually transmit the packet
        MOVR(buf_ptr, "pb_cmdresp"),
        ADDI(length, length, 1), # bump length to include CRC
        ADD(length, length, buf_ptr),
        # use 'crc' as temp variable for what to send
    L(lp+"tx"),
        LD(crc, buf_ptr, 0), # get this word from the buffer
        JAL(R7, lp+"tx_byte"), # transmit the low byte
        SRLI(crc, crc, 8), # get high byte of word
        JAL(R7, lp+"tx_byte"), # and send it
        ADDI(buf_ptr, buf_ptr, 1),
        CMP(buf_ptr, length), # done with the buffer?
        BNE(lp+"tx"),
        ADJW(8),
        JR(R7, 0), # R7 in caller's window

    L(lp+"tx_byte"),
        # wait until the transmit fifo has space
        LDXA(temp1, uart_addr+2),
        ANDI(temp1, temp1, 1),
        BZ0(lp+"tx_byte"),
        # then send the byte
        ANDI(temp1, crc, 0xFF),
        STXA(temp1, uart_addr+2),
        JR(R7, 0),
    ]

def _bfw_main(uart_addr):
    max_length = 16 # adjust to fill memory
    fw = []

    our_window = 0xFFF8 # W when chip is reset

    # PACKET RECEIVE SECTION
    return_addr = R7
    result = R6
    command = R5
    temp1 = R4
    temp2 = R3
    temp3 = R2
    temp4 = R1
    fw.append([
    L("sys_packet_rx"),
        # get a new packet from whatever's bootloading us
        JAL(return_addr, "rx_packet"),
        MOVI(result, our_window),
        LD(result, result, -8+0),
        AND(result, result, result), # if nonzero, there was an issue
        BNZ("sys_packet_tx_issue"),
        # otherwise, dispatch the command
        # a switch table would be nice, but we can't actually declare one yet
        LDR(command, result, "pb_cmdresp"), # (we know result = 0)
        SRLI(command, command, 8),
        CMPI(command, 1),
        BEQ("sys_cmd_identify"),
        CMPI(command, 2),
        BEQ("sys_cmd_write_data"),
        CMPI(command, 4),
        BEQ("sys_cmd_jump_to_code"),
        # oh no, we don't know what the command is
        # fortunately, a result of 0 also = bad command
        # so just fall through to sending a problem packet
    L("sys_packet_tx_issue"), # send error packet with problem in result
        # store the problem in the packet
        MOVI(temp1, 0),
        STR(result, temp1, "pb_data"),
        # result code 2 with length 1
        MOVI(R4, 2),
        MOVI(R5, 1),
        JAL(return_addr, "tx_packet"),
        J("sys_packet_rx"), # do it all again
    L("sys_packet_tx_success"), # say everything went great
        # result code 1 with length 0
        MOVI(R4, 1),
        MOVI(R5, 0),
        JAL(return_addr, "tx_packet"),
        J("sys_packet_rx"), # do it all again

    L("sys_cmd_identify"),
        # write identification data to buffer
        MOVR(temp1, "pb_data"),
        MOVI(temp2, 1), # boot version
        ST(temp2, temp1, 0),
        MOVI(temp2, 0x69), # board id
        ST(temp2, temp1, 1),
        MOVI(temp2, max_length), # max packet len
        ST(temp2, temp1, 2),
        # result code 5 with length 3
        MOVI(R4, 5),
        MOVI(R5, 3),
        JAL(return_addr, "tx_packet"),
        J("sys_packet_rx"),

    L("sys_cmd_write_data"),
        # write some data to some address
        # TODO validate length
        MOVR(temp1, "pb_data"),
        LD(temp2, temp1, 0), # dest addr
        LD(temp3, temp1, -1), # command/length
        ANDI(temp3, temp3, 0xFF),
        MOVI(temp1, 1), # current pos
    L("_scwd_copy"),
        LDR(temp4, temp1, "pb_data"),
        ST(temp4, temp2, 0),
        ADDI(temp2, temp2, 1),
        ADDI(temp1, temp1, 1),
        CMP(temp1, temp3),
        BNE("_scwd_copy"),
        J("sys_packet_tx_success"),

    L("sys_cmd_jump_to_code"),
        # jump to some downloaded code
        # TODO validate length
        MOVR(temp1, "pb_data"),
        LD(R0, temp1, 0), # dest addr
        LD(R1, temp1, 1), # dest W
        # tell the host that we successfully got everything before we jump into
        # the app code
        MOVI(R4, 1),
        MOVI(R5, 0),
        JAL(return_addr, "tx_packet"),
        XCHW(R7, R1), # set new W and store current one
        LD(R7, R7, 0), # get jump destination
        JR(R7, 0), # and jump to it
    ])

    fw.append([ # declare subroutines
    L("calc_crc"),
        _bfw_calc_crc(),
    L("rx_byte"),
        _bfw_rx_byte(uart_addr),
    L("rx_packet"),
        _bfw_rx_packet(max_length),
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