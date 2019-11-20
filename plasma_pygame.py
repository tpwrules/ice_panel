import pygame
import os
import itertools
from math import sin, cos, pow, sqrt, pi
import struct
import sys

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

def plasma1(x, y, t):
    return sin(x*10+t)

def plasma2(x, y, t):
    return sin(10*(x*sin(t/2)+y*cos(t/3))+t)

def plasma3(x, y, t):
    cx = x+.5*sin(t/5)
    cy = y+.5*cos(t/3)
    return sin(sqrt(100*(cx*cx+cy*cy)+1)+t)

funcs = [plasma1, plasma2, plasma3]


output = []
for x in range(DISP[1]):
    output.append([(0, 0, 0)]*DISP[0])

pygame.init()

# open display
screen = pygame.display.set_mode((SCALE*DISP[0], 2*SCALE*DISP[1]+SCALE))

# now enter pygame event loop
frame_clock = pygame.time.Clock()
frame = 0
while True:
    button_changed = False
    should_quit = True
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            break
        elif event.type == pygame.KEYDOWN:
            if event.key == "q":
                break
    else:
        should_quit = False
    if should_quit:
        break

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
    for y_ in range(DISP[1]):
        y__ = (y_-(DISP[1])/2)/DISP[1]
        for x_ in range(DISP[0]):
            v = sum([f((x_-(DISP[0]/2))/DISP[0], y__, t) for f in funcs])
            output = (int((sin(v*pi)+1)*255/2),
                int((sin(v*pi+2*pi/3)+1)*255/2),
                int((sin(v*pi+4*pi/3)+1)*255/2))
            screen.fill(output,
                (x_*SCALE, (y_+DISP[1]+1)*SCALE, SCALE-4, SCALE-4))

    pygame.display.flip()
    frame_clock.tick(30)

    frame += 1
