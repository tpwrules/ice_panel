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

def plasma1(x, y, t):
    return usin(x*10+t)

def plasma2(x, y, t):
    return usin(10*(x*usin(t/2)+y*ucos(t/3))+t)

def plasma3(x, y, t):
    cx = x*tau+.5*usin(t/5)
    cy = y*tau+.5*ucos(t/3)
    return usin(sqrt(100/(tau**2)*(cx*cx+cy*cy)+(1/(tau**2)))+t)

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
        y__ = ((y_-(DISP[1])/2)/DISP[1]) / tau
        for x_ in range(DISP[0]):
            v = sum([f(((x_-(DISP[0]/2))/DISP[0]) / tau, y__, t) for f in funcs])
            output = (int((usin(v*0.5)+1)*255/2),
                int((usin(v*0.5+0.333333)+1)*255/2),
                int((usin(v*0.5+0.666667)+1)*255/2))
            screen.fill(output,
                (x_*SCALE, (y_+DISP[1]+1)*SCALE, SCALE-4, SCALE-4))

