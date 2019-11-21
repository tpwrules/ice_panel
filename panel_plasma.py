# extremely cool plasma effect, from my personal collection™.
# please see "panel_plasma.py" for the original math and an explanation of the
# fixed point math.

from math import sin, cos, sqrt, tau, pi

# boneless CPU architecture stuff
from boneless.gateware import ALSRU_4LUT, CoreFSM
from boneless.arch.opcode import Instr
from boneless.arch.opcode import *
from bonetools import *

import boneload

# sine and cosine whose argument is from 0 to 1
def usin(x): return sin(x*tau)
def ucos(x): return cos(x*tau)

# this thing is intensely optimized for the exact shape, so you don't get any
# parameters. sorry
def firmware():
    # how much time passes every frame, in f0.12 format.
    t_inc = int((1/(30*tau))*4096)

    # addresses of the multiplier peripheral
    mulp_in_a = 32
    mulp_in_b = 33
    mulp_res = range(32, 48)

    fw = []
    r = RegisterManager()
    fw.append([
    L("reset_vector"),
        MOVI(R7, 0x3FF8), # ensure W is set up correctly
        STW(R7),
        # zero out several register windows down because we want to init
        # counters that we will keep there.
        MOVI(R7, 0x3F00),
        MOVI(R6, 0x3FF7),
        MOVI(R0, 0),
    L("zero_out_the_register_windows"),
        ST(R0, R6, 0),
        SUBI(R6, R6, 1),
        CMP(R6, R7),
        BNE("zero_out_the_register_windows"),
    ])
    r += ("R7:lr R6:fp R5:curr_x R4:curr_y R3:curr_t "
        "R2:fx_total R1:fx_curr R0:scr_ptr")
    fw.append([
        LDW(r.fp, 0),
        MOVI(r.curr_t, 0),
    L("main_loop"),
        # loop through the screen in both time and space
        MOVR(r.scr_ptr, "screen_buf"),
        MOVI(r.curr_y, -8),
    L("y_loop"),
        MOVI(r.curr_x, -16),
    L("x_loop"),
        # there are three sub-effects, and we sum the sine of each result (plus
        # time) to calculate the index of this pixel.
        JAL(r.lr, "sub_effect_1"),
        LD(r.fx_total, r.fp, -8+0),
        ADD(r.fx_total, r.fx_total, r.curr_t),
        SRLI(r.fx_total, r.fx_total, 2),
        ANDI(r.fx_total, r.fx_total, 0x3FF),
        LDR(r.fx_total, r.fx_total, "tbl_output_sine"),

        JAL(r.lr, "sub_effect_2"),
        LD(r.fx_curr, r.fp, -8+0),
        ADD(r.fx_curr, r.fx_curr, r.curr_t),
        SRLI(r.fx_curr, r.fx_curr, 2),
        ANDI(r.fx_curr, r.fx_curr, 0x3FF),
        LDR(r.fx_curr, r.fx_curr, "tbl_output_sine"),
        ADD(r.fx_total, r.fx_total, r.fx_curr),

        JAL(r.lr, "sub_effect_3"),
        LD(r.fx_curr, r.fp, -8+0),
        ADD(r.fx_curr, r.fx_curr, r.curr_t),
        SRLI(r.fx_curr, r.fx_curr, 2),
        ANDI(r.fx_curr, r.fx_curr, 0x3FF),
        LDR(r.fx_curr, r.fx_curr, "tbl_output_sine"),
        ADD(r.fx_total, r.fx_total, r.fx_curr),


        # convert index to R, G, and B
        ANDI(r.fx_total, r.fx_total, 0xFF),
        LDR(r.fx_curr, r.fx_total, "tbl_output_colormap"),
        ST(r.fx_curr, r.scr_ptr, 0),
        ADDI(r.fx_total, r.fx_total, 85),
        ANDI(r.fx_total, r.fx_total, 0xFF),
        LDR(r.fx_curr, r.fx_total, "tbl_output_colormap"),
        ST(r.fx_curr, r.scr_ptr, 1),
        ADDI(r.fx_total, r.fx_total, 85),
        ANDI(r.fx_total, r.fx_total, 0xFF),
        LDR(r.fx_curr, r.fx_total, "tbl_output_colormap"),
        ST(r.fx_curr, r.scr_ptr, 2),

        ADDI(r.scr_ptr, r.scr_ptr, 4),

        ADDI(r.curr_x, r.curr_x, 1),
        CMPI(r.curr_x, 16),
        BNE("x_loop"),

        ADDI(r.curr_y, r.curr_y, 1),
        CMPI(r.curr_y, 8),
        BNE("y_loop"),
    ])
    r -= "lr fx_total fx_curr"
    r += "R2:fb_ptr R1:curr_pix"
    fw.append([
        # now copy our hard work to the framebuffer
        MOVI(r.fb_ptr, 0),
    L("disp_copy"),
        LDR(r.curr_pix, r.fb_ptr, "screen_buf_r"),
        STX(r.curr_pix, r.fb_ptr, 0x8000),
        LDR(r.curr_pix, r.fb_ptr, "screen_buf_g"),
        STX(r.curr_pix, r.fb_ptr, 0x8001),
        LDR(r.curr_pix, r.fb_ptr, "screen_buf_b"),
        STX(r.curr_pix, r.fb_ptr, 0x8002),
        ADDI(r.fb_ptr, r.fb_ptr, 4),
        CMPI(r.fb_ptr, (32*16*4)),
        BNE("disp_copy"),
    ])
    r -= "fb_ptr curr_pix"
    r += "R7:ct_ptr R2:ct_curr R1:ct_max"
    fw.append([
        # move to the next time
        ADDI(r.curr_t, r.curr_t, t_inc),
        # additionally increment special t values which aren't multiples of 16
        # bits, necessarily.
        MOVR(r.ct_ptr, "cyc_t"),
        LD(r.ct_curr, r.ct_ptr, 0),
        ADDI(r.ct_curr, r.ct_curr, t_inc),
        MOVI(r.ct_max, 2*4096),
        CMP(r.ct_curr, r.ct_max),
        BLTU("cyc_t_not_0"),
        SUB(r.ct_curr, r.ct_curr, r.ct_max),
    L("cyc_t_not_0"),
        ST(r.ct_curr, r.ct_ptr, 0),
        LD(r.ct_curr, r.ct_ptr, 1),
        ADDI(r.ct_curr, r.ct_curr, t_inc),
        MOVI(r.ct_max, 3*4096),
        CMP(r.ct_curr, r.ct_max),
        BLTU("cyc_t_not_1"),
        SUB(r.ct_curr, r.ct_curr, r.ct_max),
    L("cyc_t_not_1"),
        ST(r.ct_curr, r.ct_ptr, 1),
        LD(r.ct_curr, r.ct_ptr, 2),
        ADDI(r.ct_curr, r.ct_curr, t_inc),
        MOVI(r.ct_max, 5*4096),
        CMP(r.ct_curr, r.ct_max),
        BLTU("cyc_t_not_2"),
        SUB(r.ct_curr, r.ct_curr, r.ct_max),
    L("cyc_t_not_2"),
        ST(r.ct_curr, r.ct_ptr, 2),
        J("main_loop"),

    ])
    r -= "!"
    r += "R6:fp R5:curr_x R0:result"
    fw.append([
    L("sub_effect_1"),
        # refreshingly simple! just a table lookup
        LDW(r.fp, -8),
        LD(r.curr_x, r.fp, 5),

        ANDI(r.curr_x, r.curr_x, 0x1F),
        LDR(r.result, r.curr_x, "tbl_fx_1_1"),

        ADJW(8),
        JR(R7, 0),
    ])
    r -= "!"
    r += "R6:fp R5:curr_x R4:curr_y R3:res_x R2:res_y R1:temp R0:result"
    fw.append([
    L("sub_effect_2"),
        # not so simple
        LDW(r.fp, -8),
        LD(r.curr_x, r.fp, 5),
        LD(r.curr_y, r.fp, 4),

        # look up the table entries based on the timer values
        MOVR(r.temp, "cyc_t"),
        LD(r.res_x, r.temp, 0),
        SRLI(r.res_x, r.res_x, 3),
        LDR(r.res_x, r.res_x, "tbl_fx_2_1"),
        LD(r.res_y, r.temp, 1),
        SRLI(r.res_y, r.res_y, 3),
        LDR(r.res_y, r.res_y, "tbl_fx_2_2"),

        # multiply the result by the coordinate
        STXA(r.curr_x, mulp_in_a),
        STXA(r.res_x, mulp_in_b),
        LDXA(r.res_x, mulp_res[5]), # >> 5 to make points line up
        STXA(r.curr_y, mulp_in_a),
        STXA(r.res_y, mulp_in_b),
        LDXA(r.res_y, mulp_res[5]), # >> 5 to make points line up

        # sum coordinate results and multiply by 10 to get final answer
        MOVI(r.result, 10),
        STXA(r.result, mulp_in_a),
        ADD(r.result, r.res_x, r.res_y),
        STXA(r.result, mulp_in_b),
        LDXA(r.result, mulp_res[0]),

        ADJW(8),
        JR(R7, 0),
    ])
    r -= "!"
    div_tau_x = int((1/tau)*4096) # f4.12
    div_tau_y = int((1/tau)*8192) # f3.13
    r += "R6:fp R5:curr_x R4:curr_y R3:res_x R2:res_y R1:temp R0:result"
    fw.append([
    L("sub_effect_3"),
        # not at all simple
        LDW(r.fp, -8),
        LD(r.curr_x, r.fp, 5),
        LD(r.curr_y, r.fp, 4),

        # look up the table entries based on the timer values
        MOVR(r.temp, "cyc_t"),
        LD(r.res_x, r.temp, 2),
        SRLI(r.res_x, r.res_x, 3),
        LDR(r.res_x, r.res_x, "tbl_fx_3_1"),
        LD(r.res_y, r.temp, 1),
        SRLI(r.res_y, r.res_y, 3),
        LDR(r.res_y, r.res_y, "tbl_fx_2_2"),
        SRAI(r.res_y, r.res_y, 2),

        # divide the coordinates by tau
        MOVI(r.temp, div_tau_x),
        STXA(r.curr_x, mulp_in_a),
        STXA(r.temp, mulp_in_b),
        LDXA(r.curr_x, mulp_res[5]), # >> 5 to make points line up
        MOVI(r.temp, div_tau_y),
        STXA(r.curr_y, mulp_in_a),
        STXA(r.temp, mulp_in_b),
        LDXA(r.curr_y, mulp_res[5]), # >> 5 to make points line up

        # add table values to components and square them. note that we take the
        # absolute value because the multiplication is unsigned.
        ADD(r.res_x, r.res_x, r.curr_x),
        STXA(r.res_x, mulp_in_a),
        STXA(r.res_x, mulp_in_b),
        LDXA(r.res_x, mulp_res[9]), # >> 9 to make the components line up
        ADD(r.res_y, r.res_y, r.curr_y),
        STXA(r.res_y, mulp_in_a),
        STXA(r.res_y, mulp_in_b),
        LDXA(r.res_y, mulp_res[9]), # >> 9 to make the components line up

        # sum final result and look up in square root table
        ADD(r.result, r.res_x, r.res_y),
        LDR(r.result, r.result, "tbl_fx_3_2"),

        ADJW(8),
        JR(R7, 0),
    ])
    r -= "!"

    # sub effect 1 tables
    tbl_fx_1_1 = [0]*32 # f0.5 -> f4.12
    for x in range(-16, 16):
        c1 = int(10/tau * 4096)&0xFFFF # f4.12
        v = ((x*c1)>>5)&0xFFFF # f0.5*f4.12 = f15.17 -> f4.12
        tbl_fx_1_1[x&0x1F] = v
    fw.append([L("tbl_fx_1_1"), tbl_fx_1_1])

    # sub effect 2 tables
    tbl_fx_2_1 = [] # fX.9 -> f4.12
    for n in range(512*2): # enough to go through whole sine period
        v = usin((n/512)/2)/tau
        tbl_fx_2_1.append(int(v*4096)&0xFFFF)
    fw.append([L("tbl_fx_2_1"), tbl_fx_2_1])

    tbl_fx_2_2 = [] # fX.9 -> f3.13
    for n in range(512*3): # enough to go through whole cosine period
        v = ucos((n/512)/3)/tau
        tbl_fx_2_2.append(int(v*8192)&0xFFFF)
    fw.append([L("tbl_fx_2_2"), tbl_fx_2_2])

    # sub effect 3 tables
    tbl_fx_3_1 = [] # fX.9 -> f4.12
    for n in range(5*512): # enough to go through whole sine period
        v = .5*usin((n/512)/5)/tau
        tbl_fx_3_1.append(int(v*4096)&0xFFFF)
    fw.append([L("tbl_fx_3_1"), tbl_fx_3_1])

    tbl_fx_3_2 = [] # f0.15 -> f4.12
    for n in range(1632+1): # empirically never goes past 1632
        v = sqrt(100*(n/32768)+(1/(tau**2)))
        tbl_fx_3_2.append(int(v*4096)&0xFFFF)
    fw.append([L("tbl_fx_3_2"), tbl_fx_3_2])

    # output tables.
    # sine of the total for this pixel
    tbl_output_sine = [] # f0.9 -> f9.7
    for n in range(1024):
        v = usin(n/1024)
        tbl_output_sine.append(int(v*128)&0xFFFF)
    fw.append([L("tbl_output_sine"), tbl_output_sine])

    # color lookup from the above number to RGB
    tbl_output_colormap = [] # f0.8 -> f8.0
    for n in range(256):
        tbl_output_colormap.append(int((usin(n/256)+1)*255/2))
    fw.append([L("tbl_output_colormap"), tbl_output_colormap])

    # screen sum buffer (we transfer it all to the screen at the end, in one
    # quick motion). we also make labels 0, 1, and 2 words into the thing so we
    # can address the r, g, and b channels.
    fw.append([
    L("screen_buf"),
    L("screen_buf_r"),
        0,
    L("screen_buf_g"),
        0,
    L("screen_buf_b"),
        [0]*(32*16*4-2)
    ])

    # t values that have specific periods which aren't a multiple of 16 bits
    fw.append([L("cyc_t"), [0]*3])

    return fw

if __name__ == "__main__":
    from cli import main
    def make(simulating):
        raise Exception("this is code only!")

    def fw():
        return firmware()

    main(maker=make, fw=fw, build_args={"synth_opts": "-abc9"})
