from math import sin, cos, pow, sqrt, pi, tau

SCALE = 16
DISP = (32, 16)

def plasma1o(x, y, t):
    return sin(x*10+t)

def plasma2o(x, y, t):
    return sin(10*(x*sin(t/2)+y*cos(t/3))+t)

def plasma3o(x, y, t):
    cx = x+.5*sin(t/5)
    cy = y+.5*cos(t/3)
    return sin(sqrt(100*(cx*cx+cy*cy)+1)+t)

funcso = [plasma1o]#, plasma2o, plasma3o]

def usin(x): return sin(x*tau)
def ucos(x): return cos(x*tau)

# cn = constant that can be simplified
# fn = function that can be converted into a table

def plasma1(x, y, t):
    # x = f11.5
    c1 = int(10/tau * 4096)&0xFFFF # f4.12
    v = x*c1 # f15.17, must be SIGNED!!!!
    v = ((v>>5)&0xFFFF) + t
    return v&0xFFFF # f4.12

# usin(v/2)/tau
plasma2_table1 = [] # 1.12 -> f6.10
for n in range(8192):
    v = usin((n/4096)/2)/tau
    plasma2_table1.append(int(v*1024))

# ucos(v/3)/tau
plasma2_table2 = [] # f1.12 -> f5.11
for n in range(8192):
    v = ucos((n/4096)/3)/tau
    plasma2_table2.append(int(v*2048))

def plasma2(x, y, t):
    c1 = 10 # f16.0
    f1 = plasma2_table1[t & 0x1FFF] # f8.7
    f2 = plasma2_table2[t & 0x1FFF] # f9.6
    vf1 = (x*f1)>>3 # f11.5*f6.10 = f17.15 -> f4.12
    vf2 = (y*f2)>>3 # f12.4*f5.11 = f17.15 -> f4.12
    v = c1*(vf1 + vf2) + t
    return v # f4.12

def plasma3(x, y, t):
    f1 = lambda v: .5*usin(v/5)
    f2 = lambda v: .5*ucos(v/3)
    f3 = lambda v: sqrt(100/(tau**2)*v+(1/(tau**2)))
    f4 = lambda v: v*v
    cx = x+f1(t)
    cy = y+f2(t)
    v = f3(f4(cx) + f4(cy)) + t
    return int(v*1024)&0xFFFF # f6.10

funcs = [plasma1]#, plasma2, plasma3]

output_table = [] # f0.8 -> f8.0
for n in range(256):
    output_table.append(int((usin(n/256)+1)*255/2))

table_usin_1 = [] # f0.12 -> f9.7
for n in range(4096):
    v = usin(n/4096)
    table_usin_1.append(int(v*128)&0xFFFF)

def draw(screen, frame):
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
    t = frame/(30*tau)
    # convert t to f4.12. we need to figure out the period at some point...
    t = int(t*4096)&0xFFFF
    for y_ in range(DISP[1]):
        y__ = y_-(DISP[1]//2)
        for x_ in range(DISP[0]):
            v = 0
            for f in funcs:
                fv = f(x_-(DISP[0]//2), y__, t)&0xFFFF # f4.12
                # all the functions need to be usined, using only the
                # fractional part of the result.
                v += table_usin_1[fv & 0xFFF]

            # v is returned in f9.7 . since we need to divide v by 2,
            # we just interpret it as f8.8 and the division is done.
            v = v&0xFF # remove integer bits
            output = (output_table[v], # and look up result in table
                output_table[(v+85)&0xFF],
                output_table[(v+170)&0xFF])
            screen.fill(output,
                (x_*SCALE, (y_+DISP[1]+1)*SCALE, SCALE-4, SCALE-4))

