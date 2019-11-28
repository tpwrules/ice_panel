# some code samples to test the register allocator
from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *

from regalloc import RegisterAllocator
from regalloc import R_USE

# example code from various reading material
def test_1():
    alloc = RegisterAllocator()
    r = alloc.tracker
    alloc.add_code([
        MOVI(r.a, 0),
    L("loop"),
        [ADDI(r.b, r.a, 1),
        [ADD(r.c, r.c, r.b)],],
        SLLI(r.a, r.b, 1),
        CMPI(r.a, 9),
        BLTU("loop"),
        R_USE(r.c), # force register to be used, as if we are returning it
    ])
    return alloc.allocate()

# the fire demonstration
def test_2():
    alloc = RegisterAllocator()
    r = alloc.tracker

    panel_w = 32
    panel_h = 16
    extra_lines = 4

    buf_w = panel_w
    buf_h = panel_h+extra_lines

    cool_wrap_row = buf_h-2
    spark_val = 192 # limits effect intensity
    delay_time = 15000 # limits framerate. not in any sensible units

    alloc.add_code([
    L("main"),
        # we index the buffer linearly for speed. we start at the second pixel
        # of the second row so that all of the neighbors will exist.
        MOVI(r.buf_pos, buf_w+1),
        MOVR(r.buf_ptr, "front_buffer"),
        ADD(r.buf_ptr, r.buf_ptr, r.buf_pos),
        # load end conditions so we don't have to keep using EXTIs
        # we stop at the second to last pixel of the second to last line so that
        # all of the neighbors will exist, like above.
        MOVI(r.buf_end, buf_w*(buf_h-1)-1),
        # rows near the bottom of the screen get to spark to keep the flames
        # going
        MOVI(r.spark_pos, buf_w*cool_wrap_row),
    L("thermodynamic_loop"), # spread the heat and cool down
        # average the 8 neighbors of this pixel
        MOVI(r.total_pix, 0),
        LD(r.curr_pix, r.buf_ptr, -1-buf_w), # top left
        ADD(r.total_pix, r.total_pix, r.curr_pix),
        LD(r.curr_pix, r.buf_ptr, -buf_w), # top
        ADD(r.total_pix, r.total_pix, r.curr_pix),
        LD(r.curr_pix, r.buf_ptr, 1-buf_w), # top right
        ADD(r.total_pix, r.total_pix, r.curr_pix),
        LD(r.curr_pix, r.buf_ptr, -1), # left
        ADD(r.total_pix, r.total_pix, r.curr_pix),
        LD(r.curr_pix, r.buf_ptr, 1), # right
        ADD(r.total_pix, r.total_pix, r.curr_pix),
        LD(r.curr_pix, r.buf_ptr, -1+buf_w), # bottom left
        ADD(r.total_pix, r.total_pix, r.curr_pix),
        LD(r.curr_pix, r.buf_ptr, buf_w), # bottom
        ADD(r.total_pix, r.total_pix, r.curr_pix),
        LD(r.curr_pix, r.buf_ptr, 1+buf_w), # bottom right
        ADD(r.total_pix, r.total_pix, r.curr_pix),
        SRLI(r.avg_pix, r.total_pix, 3), # divide by 8

        # we only cool pixels where the low bits of the sum are 0. thus, 25% of
        # pixels will be "randomly" chosen for cooling.
        ANDI(r.total_pix, r.total_pix, 3),
        BNZ("done_cooling"),
        # we cool only if the new pixel is above 0 so that the flames eventually
        # die out.
        CMPI(r.avg_pix, 0),
        BNE("cooling"),
        # but! if we are near the bottom of the screen, we let the pixels go
        # below zero so that we get new bright sparks.
        CMP(r.buf_pos, r.spark_pos),
        BLTU("done_cooling"),
        CMPI(r.avg_pix, 0), # does it, in fact, equal zero?
        BNE("cooling"), # no, cool it normally
        MOVI(r.avg_pix, spark_val+1), # yes, make it spark
    L("cooling"),
        SUBI(r.avg_pix, r.avg_pix, 1),
    L("done_cooling"),
        STR(r.avg_pix, r.buf_pos, "back_buffer"),
        ADDI(r.buf_ptr, r.buf_ptr, 1),
        ADDI(r.buf_pos, r.buf_pos, 1),
        # did we reach the end? 
        CMP(r.buf_pos, r.buf_end),
        BNE("thermodynamic_loop"),

        # enregister buffer end so we don't have to use an EXTI.
        MOVI(r.buf_end, buf_w*(buf_h-2)),
        MOVI(r.buf_pos, 0),
        MOVR(r.buf_addr, "back_buffer"),
        # copy the new buffer to the old buffer, offset one row, so that the
        # flames proceed upwards
    L("copy_buf_loop"),
        LD(r.pixel, r.buf_addr, buf_w),
        STR(r.pixel, r.buf_pos, "front_buffer"),
        ADDI(r.buf_addr, r.buf_addr, 1),
        ADDI(r.buf_pos, r.buf_pos, 1),
        CMP(r.buf_pos, r.buf_end),
        BNE("copy_buf_loop"),

        # copy the buffer to the screen memory as well
        MOVI(r.buf_pos, 0),
        MOVI(r.buf_end, buf_w*panel_h),
        MOVI(r.scr_addr, 0x8000),
        MOVR(r.palette_ptr, "color_palette"),
    L("display_loop"),
        LDR(r.pixel, r.buf_pos, "front_buffer"),
        SLLI(r.pixel, r.pixel, 2),
        ADD(r.pixel, r.pixel, r.palette_ptr),
        LD(r.color, r.pixel, 0),
        STX(r.color, r.scr_addr, 0),
        LD(r.color, r.pixel, 1),
        STX(r.color, r.scr_addr, 1),
        LD(r.color, r.pixel, 2),
        STX(r.color, r.scr_addr, 2),
        ADDI(r.buf_pos, r.buf_pos, 1),
        ADDI(r.scr_addr, r.scr_addr, 4),
        CMP(r.buf_pos, r.buf_end),
        BNE("display_loop"),

        MOVI(r.delay_ctr, delay_time),
    L("delay_loop"),
        SUBI(r.delay_ctr, r.delay_ctr, 1),
        BNE("delay_loop"),
        J("main"),
    ])

    return alloc.allocate()

print(test_2()) # execute the test, who knows what will happen
