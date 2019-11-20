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

plasma1_table1 = [0]*32 # 0.5 -> 4.12
for x in range(-16, 16):
    c1 = int(10/tau * 4096)&0xFFFF # f4.12
    v = x*c1 # f15.17, must be SIGNED!!!!
    plasma1_table1[x&0x1F] = (v>>5)&0xFFFF

def plasma1(x, y, t, nt):
    # x = f11.5
    return plasma1_table1[x&0x1F]+t # f4.12

# usin(v/2)/tau
plasma2_table1 = [] # 1.12 -> f4.12
for n in range(512*2):
    v = usin((n/512)/2)/tau
    plasma2_table1.append(int(v*4096))

# ucos(v/3)/tau
plasma2_table2 = [] # f2.12 -> f4.12
for n in range(512*3):
    v = ucos((n/512)/3)/tau
    plasma2_table2.append(int(v*8192))

p2t1 = 0
p2t2 = 0
def plasma2(x, y, t, nt):
    global p2t1, p2t2
    c1 = 10 # f16.0
    f1 = plasma2_table1[p2t1>>3] # f4.12
    f2 = plasma2_table2[p2t2>>3] # f3.13
    if nt:
        p2t1 += ti
        p2t2 += ti
        if p2t1 >= 2*4096:
            p2t1 -= 2*4096
        if p2t2 >= 3*4096:
            p2t2 -= 3*4096
    vf1 = (x*f1)>>5 # f11.5*f4.12 = f15.17 -> f4.12
    vf2 = (y*f2)>>5 # f12.4*f3.13 = f15.17 -> f4.12
    v = c1*(vf1 + vf2) + t
    return v # f4.12

# .5*usin(v/5)/tau
plasma3_table1 = [] # f3.12 -> f4.12
for n in range(5*512):
    v = usin((n/512)/5)/tau
    plasma3_table1.append(int(v*4096))

# sqrt(100*v+(1/(tau**2)))
plasma3_table2 = [] # f0.15 -> f4.12
for n in range(1632+1): # never goes past 1632
    v = sqrt(100*(n/32768)+(1/(tau**2)))
    plasma3_table2.append(int(v*4096))

p3t1 = 0
p3t2 = 0
maxs = 0
def plasma3(x, y, t, nt):
    global p3t1, p3t2, maxs
    taux_ = int((1/tau)*4096) # f4.12
    tauy_ = int((1/tau)*8192) # f3.13
    xf = plasma3_table1[p3t1>>3]>>1 # f4.12 divide by 2
    yf = plasma2_table2[p3t2>>3]>>2 # f3.13 -> f4.12 then divide by 2
    if nt:
        p3t1 += ti
        p3t2 += ti
        if p3t1 >= 5*4096:
            p3t1 -= 5*4096
        if p3t2 >= 3*4096:
            p3t2 -= 3*4096
    xs = (x*taux_)>>5 # f11.5*f4.12 = f15.17 = f4.12
    ys = (y*tauy_)>>5 # f12.4*f3.13 = f15.17 = f4.12
    cx = xs + xf # f4.12
    cy = ys + yf # f4.12
    cx2 = (cx*cx)>>9 # f4.12*f4.12 = f8.24 -> f1.15
    cy2 = (cy*cy)>>9 # f4.12*f4.12 = f8.24 -> f1.15
    s = (cx2+cy2)
    if s > maxs:
        print("max s =", s)
        maxs = s
    v = plasma3_table2[s] + t
    return v # f4.12

funcs = [plasma1, plasma2, plasma3]

output_table = [] # f0.8 -> f8.0
for n in range(256):
    output_table.append(int((usin(n/256)+1)*255/2))

table_usin_1 = [] # f0.9 -> f9.7
for n in range(1024):
    v = usin(n/1024)
    table_usin_1.append(int(v*128)&0xFFFF)

intt = 0

def draw(screen, frame):
    global intt, p2t, p3t1, p3t2

    if frame == 0:
        p2t = 0
        p3t1 = 0
        p3t2 = 0

    t = intt & 0xFFFF

    # original version
    t /= 4096/tau
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
    nt = True
    for y_ in range(DISP[1]):
        y__ = y_-(DISP[1]//2)
        for x_ in range(DISP[0]):
            v = 0
            for f in funcs:
                fv = f(x_-(DISP[0]//2), y__, t, nt)&0xFFFF # f4.12
                # all the functions need to be usined, using only the
                # fractional part of the result.
                v += table_usin_1[(fv>>2) & 0x3FF]
            nt = False

            # v is returned in f9.7 . since we need to divide v by 2,
            # we just interpret it as f8.8 and the division is done.
            v = v&0xFF # remove integer bits
            output = (output_table[v], # and look up result in table
                output_table[(v+85)&0xFF],
                output_table[(v+170)&0xFF])
            screen.fill(output,
                (x_*SCALE, (y_+DISP[1]+1)*SCALE, SCALE-4, SCALE-4))
