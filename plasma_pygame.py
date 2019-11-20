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

funcso = [plasma1o, plasma2o, plasma3o]

def usin(x): return sin(x*tau)
def ucos(x): return cos(x*tau)

# cn = constant that can be simplified
# fn = function that can be converted into a table

def plasma1(x, y, t):
    c1 = 10/tau
    f1 = lambda v: usin(v)
    return f1(x*c1 + t)

def plasma2(x, y, t):
    c1 = 10
    # / tau could be moved to c1 as well, depending on precision
    f1 = lambda v: usin(v/2)/tau
    f2 = lambda v: ucos(v/3)/tau
    f3 = lambda v: usin(v)
    return f3(c1*( x*f1(t) + y*f2(t) ) + t)

def plasma3(x, y, t):
    f1 = lambda v: .5*usin(v/5)
    f2 = lambda v: .5*ucos(v/3)
    f3 = lambda v: sqrt(100/(tau**2)*v+(1/(tau**2)))
    f4 = lambda v: v*v
    f5 = lambda v: usin(v)
    cx = x+f1(t)
    cy = y+f2(t)
    return f5(f3(f4(cx) + f4(cy)) + t)

funcs = [plasma1, plasma2, plasma3]

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
    for y_ in range(DISP[1]):
        y__ = ((y_-(DISP[1])/2)/DISP[1])
        for x_ in range(DISP[0]):
            v = sum([f(((x_-(DISP[0]/2))/DISP[0]), y__, t) for f in funcs])
            output = (int((usin(v*0.5)+1)*255/2),
                int((usin(v*0.5+0.333333)+1)*255/2),
                int((usin(v*0.5+0.666667)+1)*255/2))
            screen.fill(output,
                (x_*SCALE, (y_+DISP[1]+1)*SCALE, SCALE-4, SCALE-4))

