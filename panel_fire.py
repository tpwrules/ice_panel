# cool fire effect, from https://www.hanshq.net/fire.html

# boneless CPU architecture stuff
from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *
from bonetools import *

import boneload

def firmware(bpp, 
        # panel shape. extra lines is how many lines to have below the effect
        # so it can rise offscreen a bit.
        panel_w, panel_h, extra_lines):

    buf_w = panel_w
    buf_h = panel_h+extra_lines

    cool_wrap_row = buf_h-2
    spark_val = 192 # limits effect intensity
    delay_time = 15000 # limits framerate. not in any sensible units

    fw = []
    r = RegisterManager()
    fw.append(L("main"))

    r += "R7:buf_ptr R6:buf_pos R5:curr_pix R4:total_pix R3:avg_pix"
    r += "R2:buf_end R1:spark_pos"
    fw.append([
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
    ])
    r -= "!"
    r += "R7:buf_pos R6:buf_end R5:buf_addr R4:pixel"
    fw.append([
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
    ])
    r -= "!"
    r += "R7:buf_pos R6:buf_end R5:scr_addr R4:pixel R3:color R2:palette_ptr"
    fw.append([
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
    ])
    r -= "!"
    r += "R7:delay_ctr"
    fw.append([
        MOVI(r.delay_ctr, delay_time),
    L("delay_loop"),
        SUBI(r.delay_ctr, r.delay_ctr, 1),
        BNE("delay_loop"),
    ])
    r -= "!"
    fw.append(J("main"))

    # extrapolate color table from original demo. it was 6 bit color
    orig_palette = [
          0,   0,   0,   0,   1,   1,   0,   4,   5,   0,   7,   9,
          0,   8,  11,   0,   9,  12,  15,   6,   8,  25,   4,   4,
         33,   3,   3,  40,   2,   2,  48,   2,   2,  55,   1,   1,
         63,   0,   0,  63,   0,   0,  63,   3,   0,  63,   7,   0,
         63,  10,   0,  63,  13,   0,  63,  16,   0,  63,  20,   0,
         63,  23,   0,  63,  26,   0,  63,  29,   0,  63,  33,   0,
         63,  36,   0,  63,  39,   0,  63,  39,   0,  63,  40,   0,
         63,  40,   0,  63,  41,   0,  63,  42,   0,  63,  42,   0,
         63,  43,   0,  63,  44,   0,  63,  44,   0,  63,  45,   0,
         63,  45,   0,  63,  46,   0,  63,  47,   0,  63,  47,   0,
         63,  48,   0,  63,  49,   0,  63,  49,   0,  63,  50,   0,
         63,  51,   0,  63,  51,   0,  63,  52,   0,  63,  53,   0,
         63,  53,   0,  63,  54,   0,  63,  55,   0,  63,  55,   0,
         63,  56,   0,  63,  57,   0,  63,  57,   0,  63,  58,   0,
         63,  58,   0,  63,  59,   0,  63,  60,   0,  63,  60,   0,
         63,  61,   0,  63,  62,   0,  63,  62,   0,  63,  63,   0]

    fw.append(L("color_palette"))
    for ci, c in enumerate(orig_palette):
        new_bits = bpp-6
        # replicate top bits to bottom so we get a more linear scale
        fw.append((c<<new_bits) + (c>>(6-new_bits)))
        # each entry is 4 words to save indexing time (we have plenty of memory)
        if ci % 3 == 2:
            fw.append(0)
    # the rest of the entries are filled with white
    for ci in range(spark_val-64):
        fw.append([255, 255, 255, 0])

    fw.append(L("back_buffer"))
    fw.append([0]*(buf_h*(buf_w-1)))
    fw.append(L("front_buffer"))
    fw.append([0]*(buf_h*buf_w))

    return fw

if __name__ == "__main__":
    from cli import main
    def make(simulating):
        raise Exception("this is code only!")

    def fw():
        return firmware(8, 32, 16, 4)

    main(maker=make, fw=fw, build_args={"synth_opts": "-abc9"})
