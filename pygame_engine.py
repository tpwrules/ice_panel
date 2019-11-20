import pygame
import sys
import importlib
import traceback

# import the display module the user wishes to use
disp_mod = importlib.import_module(sys.argv[1].replace(".py", ""))
SCALE = disp_mod.SCALE
DISP = disp_mod.DISP

pygame.init()

# open display
screen = pygame.display.set_mode((SCALE*DISP[0], 2*SCALE*DISP[1]+SCALE),
    display=1)

# now enter pygame event loop
frame_clock = pygame.time.Clock()
frame = 0
excepted = False
while True:
    button_changed = False
    should_quit = True
    should_reload = False
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            break
        elif event.type == pygame.KEYDOWN:
            if event.unicode == "q":
                break
            elif event.unicode == "r":
                should_reload = True
    else:
        should_quit = False
    if should_quit:
        break

    if should_reload:
        # reload the display module cause presumably the user changed it
        try:
            importlib.reload(disp_mod)
            # and start the whole deal over
            frame = 0
            excepted = False
        except:
            traceback.print_exc()

    if not excepted:
        try:
            disp_mod.draw(screen, frame)
        except:
            traceback.print_exc()
            excepted = True

    pygame.display.flip()
    frame_clock.tick(30)

    frame += 1
