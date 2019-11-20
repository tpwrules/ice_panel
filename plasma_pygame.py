from math import sin, cos, pow, sqrt, pi, tau

SCALE = 16
DISP = (32, 16)

ti = int((1/(30*tau))*4096)

def plasma1o(x, y, t):
    return sin(x*10+t)

def plasma2o(x, y, t):
    return sin(10*(x*sin(t/2)+y*cos(t/3))+t)

def plasma3o(x, y, t):
    cx = x+.5*sin(t/5)
    cy = y+.5*cos(t/3)
    return sin(sqrt(100*(cx*cx+cy*cy)+1)+t)

funcso = [plasma1o, plasma2o, plasma3o]

def usin(x): return sin(x*tau)
def ucos(x): return cos(x*tau)

# cn = constant that can be simplified
# fn = function that can be converted into a table

def plasma1(x, y, t, nt):
    # x = f11.5
    c1 = int(10/tau * 4096)&0xFFFF # f4.12
    v = x*c1 # f15.17, must be SIGNED!!!!
    v = ((v>>5)&0xFFFF) + t
    return v&0xFFFF # f4.12

# usin(v/2)/tau
plasma2_table1 = [] # 1.12 -> f4.12
for n in range(8192):
    v = usin((n/4096)/2)/tau
    plasma2_table1.append(int(v*4096))

# ucos(v/3)/tau
plasma2_table2 = [] # f1.12 -> f4.12
for n in range(4096*3):
    v = ucos((n/4096)/3)/tau
    plasma2_table2.append(int(v*4096))

p2t = 0
def plasma2(x, y, t, nt):
    global p2t
    c1 = 10 # f16.0
    f1 = plasma2_table1[t & 0x1FFF] # f4.12
    f2 = plasma2_table2[p2t] # f4.12
    if nt:
        p2t += ti
        if p2t >= 3*4096:
            p2t -= 3*4096
    vf1 = (x*f1)>>5 # f11.5*f4.12 = f15.17 -> f4.12
    vf2 = (y*f2)>>4 # f12.4*f4.12 = f16.16 -> f4.12
    v = c1*(vf1 + vf2) + t
    return v # f4.12

# .5*usin(v/5)/tau
plasma3_table1 = [] # f3.12 -> f4.12
for n in range(5*4096):
    v = usin((n/4096)/5)/tau
    plasma3_table1.append(int(v*4096))

# sqrt(100*v+(1/(tau**2)))
plasma3_table2 = [] # f0.15 -> f4.12
for n in range(32768):
    v = sqrt(100*(n/32768)+(1/(tau**2)))
    plasma3_table2.append(int(v*4096))

p3t1 = 0
p3t2 = 0
def plasma3(x, y, t, nt):
    global p3t1, p3t2
    tau_ = int((1/tau)*4096) # f4.12
    xf = plasma3_table1[p3t1]>>1 # f4.12 divide by 2
    yf = plasma2_table2[p3t2]>>1 # f4.12 divide by 2
    if nt:
        p3t1 += ti
        p3t2 += ti
        if p3t1 >= 5*4096:
            p3t1 -= 5*4096
        if p3t2 >= 3*4096:
            p3t2 -= 3*4096
    xs = (x*tau_)>>5 # f11.5*f4.12 = f15.17 = f4.12
    ys = (y*tau_)>>4 # f12.4*f4.12 = f16.16 = f4.12
    cx = abs(xs + xf) # f4.12
    cy = abs(ys + yf) # f4.12
    cx2 = (cx*cx)>>9 # f4.12*f4.12 = f8.24 -> f1.15
    cy2 = (cy*cy)>>9 # f4.12*f4.12 = f8.24 -> f1.15
    s = (cx2+cy2)&0x7FFF
    v = plasma3_table2[s] + t
    return v # f4.12

funcs = [plasma1, plasma2, plasma3]#, plasma3]#, plasma2, plasma3]

output_table = [] # f0.8 -> f8.0
for n in range(256):
    output_table.append(int((usin(n/256)+1)*255/2))

table_usin_1 = [] # f0.12 -> f9.7
for n in range(4096):
    v = usin(n/4096)
    table_usin_1.append(int(v*128)&0xFFFF)

intt = 0

def draw(screen, frame):
    global intt, p2t, p3t1, p3t2

    if frame == 0:
        p2t = 0
        p3t1 = 0
        p3t2 = 0

    # original version
    t = frame/30
    for y_ in range(DISP[1]):
        y__ = (y_-(DISP[1])/2)/DISP[1]
        for x_ in range(DISP[0]):
            v = sum([f((x_-(DISP[0]/2))/DISP[0], y__, t) for f in funcso])
            output = (int((sin(v*pi)+1)*255/2),
                int((sin(v*pi+2*pi/3)+1)*255/2),
                int((sin(v*pi+4*pi/3)+1)*255/2))
            screen.fill(output,
                (x_*SCALE, y_*SCALE, SCALE-4, SCALE-4))

    # modified, optimized version
    t = intt & 0xFFFF
    intt += ti
    # convert t to f4.12. we need to figure out the period at some point...
    #t = int(t*4096)&0xFFFF
    nt = True
    for y_ in range(DISP[1]):
        y__ = y_-(DISP[1]//2)
        for x_ in range(DISP[0]):
            v = 0
            for f in funcs:
                fv = f(x_-(DISP[0]//2), y__, t, nt)&0xFFFF # f4.12
                # all the functions need to be usined, using only the
                # fractional part of the result.
                v += table_usin_1[fv & 0xFFF]
            nt = False

            # v is returned in f9.7 . since we need to divide v by 2,
            # we just interpret it as f8.8 and the division is done.
            v = v&0xFF # remove integer bits
            output = (output_table[v], # and look up result in table
                output_table[(v+85)&0xFF],
                output_table[(v+170)&0xFF])
            screen.fill(output,
                (x_*SCALE, (y_+DISP[1]+1)*SCALE, SCALE-4, SCALE-4))
