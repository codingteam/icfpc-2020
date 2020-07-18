import termios
import sys

class Interactor:
    def __init__(self, data):
        self.data = data
        self.px = self.py = 0

    def draw(self):
        print(end = "xy=%4d %4d    " % (self.px, self.py))
        print("\x1b[2mwasd hjkl - move cursor; q - exit; enter - click\x1b[m")

        x0 = x1 = self.px
        y0 = y1 = self.py

        for layer in self.data:
            if len(layer) == 0:
                continue
            x0 = min(x0, min(map(lambda a: a[0], layer)))
            x1 = max(x1, max(map(lambda a: a[0], layer)))
            y0 = min(y0, min(map(lambda a: a[1], layer)))
            y1 = max(y1, max(map(lambda a: a[1], layer)))

        res = [[". "] * (x1 - x0 + 1) for y in range(y0, y1+1)]

        for layer in self.data:
            for x, y in layer:
                res[y - y0][x - x0] = "##"

        res[self.py-y0][self.px-x0] = "\x1b[41;30m" + res[self.py-y0][self.px-x0] + "\x1b[m"
        res[-y0][-x0] = "\x1b[42;30m" + res[-y0][-x0] + "\x1b[m"

        print("\n".join(map("".join, res)))

    def run(self):
        fd = sys.stdin.fileno()
        old = termios.tcgetattr(fd)
        new = termios.tcgetattr(fd)
        new[3] = new[3] & ~termios.ECHO & ~termios.ICANON
        try:
            print(end="\x1b[?1047h") # Enable alternate display
            termios.tcsetattr(fd, termios.TCSADRAIN, new)
            print(end="\x1b7") # save cursor
            #print(end="\x1b[?1000h", flush=True) # Enable mouse tracking

            while True:
                print(end="\x1b[2J") # Erase All
                print(end="\x1b[H") # Cursor at 0, 0
                self.draw()

                a = sys.stdin.read(1)
                if a in "kw":
                    self.py -= 1
                if a in "js":
                    self.py += 1
                if a in "ha":
                    self.px -= 1
                if a in "ld":
                    self.px += 1
                if a in "\n\r":
                    return (self.px, self.py)
                if a in "q":
                    return None
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old)
            #print(end="\x1b[?1000l") # Disable mouse tracking
            print(end="\x1b[?1047l") # Disable alternate display
            print(end="\x1b8") # restore cursor


def draw(what):
    if len(what) == 0:
        x0 = y0 = x1 = y1 = 0
    else:
        x0 = min(map(lambda a: a[0], what)); x0 = min(x0, 0)
        x1 = max(map(lambda a: a[0], what)); x1 = max(x1, 0)
        y0 = min(map(lambda a: a[1], what)); y0 = min(y0, 0)
        y1 = max(map(lambda a: a[1], what)); y1 = max(y1, 0)

    res = [["."] * (x1 - x0 + 1) for y in range(y0, y1+1)]

    for x, y in what:
        res[y - y0][x - x0] = "#"

    res[-y0][-x0] = "\x1b[42;30m" + res[-y0][-x0] + "\x1b[m"

    print("\n".join(map("".join, res)))
    print()
