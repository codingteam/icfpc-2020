#!/usr/bin/env python3

import acceleration_controls as ac
from state_parsing import Ship
import math


def main():
    pos, vel = (48, -10), (0, 0)

    accel = [0,0]

    ticks = 59
    positions = []
    traces = []

    for tick in range(ticks):
        ship = Ship()
        ship.is_defender = True
        ship.ship_id = 0
        ship.xy_coordinates = pos
        ship.xy_velocity = vel

        moon_radius = math.sqrt(512/2)


        pos, vel = simulate1(pos, vel)

        cmd = ac.calculate_acceleration_corner(ship, moon_radius)
        accel = handle_command(cmd)

        s = simulate(pos, vel, 15)

        vel = (vel[0] - accel[0], vel[1] - accel[1])

        print(f"qq {tick} \x1b[34m{pos}\x1b[m \x1b[31m{vel}\x1b[m {accel} {s}")
        print(f"qw [{pos[0]},{pos[1]}]")

        positions.append(pos)
        traces.append(s)

    for tick in range(ticks):
        draw_svg(tick, moon_radius, positions, traces[tick])

def handle_command(cmd):
    if cmd is None:
        return (0, 0)
    if len(cmd) == 3 and cmd[0] == 0 and len(cmd[2]) == 2:
        return tuple(cmd[2])

def simulate1(pos, vel):
    pos = (pos[0] + vel[0], pos[1] + vel[1])
    dx, dy = abs(pos[0]), abs(pos[1])
    maxD = max(dx, dy)

    newV = list(vel)
    if dx == maxD:
        newV[0] -= sign(pos[0])
    if dy == maxD:
        newV[1] -= sign(pos[1])
    return pos, tuple(newV)

def simulate(pos, vel, ticks):
    points = []
    for i in range(ticks):
        pos, vel = simulate1(pos, vel)
        points.append(pos)
    return points

def draw_svg(tick, moon_radius, *traces):
    f = open("out/%03d.svg" % tick, "w")
    vbox = 128 + 64
    print(f'<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="512" height="512" viewBox="-{vbox} -{vbox} {vbox*2} {vbox*2}">', file=f)
    print(f'<rect x="-200%" y="-200%" width="400%" height="400%" style="fill:#131313"/>', file=f)
    print(f'<rect x="-128" y="-128" width="256" height="256" style="fill:#000000"/>', file=f)

    print(f'<rect x="{-moon_radius}" y="{-moon_radius}" width="{moon_radius*2}" height="{moon_radius*2}" style="fill:white"/>', file=f)
    print(f'<line x1="-200%" y1="-200%" x2="200%" y2="200%" style="stroke:#444" />',file=f)
    print(f'<line x1="-200%" y1="200%" x2="200%" y2="-200%" style="stroke:#444" />',file=f)

    colors = ["teal", "orange"]
    for n, trace in enumerate(traces):
        sx, sy = trace[0]
        for px, py in trace[1:]:
            print(f'<line x1="{sx}" y1="{sy}" x2="{px}" y2="{py}" style="stroke:{colors[n]};stroke-width:2" />',file=f)
            sx, sy = px, py

    print('</svg>', file=f)

def sign(a):
    if a < 0:
        return -1
    if a > 0:
        return 1
    return 0

main()
