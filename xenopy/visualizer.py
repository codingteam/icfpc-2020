#!/usr/bin/env python3

import acceleration_controls as ac
from state_parsing import Ship
import math


def main():
    pos, vel = (48, 18), (0, 0)

    accel = [0,0]

    fuel_used = 0
    ticks = 200
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

        cmd = ac.calculate_acceleration_v2(ship, moon_radius)
        accel = handle_command(cmd)
        if accel != (0, 0):
            fuel_used += 1

        s = simulate(pos, vel, 80)

        vel = (vel[0] - accel[0], vel[1] - accel[1])

        print(f"qq {tick} {fuel_used} \x1b[34m{pos}\x1b[m \x1b[31m{vel}\x1b[m {accel} {s}")
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

def simulate_score(pos, vel, moon_radius, acc, limit):
    speed_sum = 0
    for i in range(limit):
        pos, vel = simulate1(pos, vel)
        moon_dist = max(abs(pos[0]), abs(pos[1])) - moon_radius*2
        space_dist = 128 - max(abs(pos[0]), abs(pos[1]))
        if moon_dist <= 2 or space_dist <= 2:
            return (0, i)
        if i == 0:
            min_moon_dist = max_moon_dist = moon_dist
        else:
            min_moon_dist = min(min_moon_dist, moon_dist)
            max_moon_dist = max(max_moon_dist, moon_dist)
        speed_sum += abs(vel[0]) + abs(vel[1])

    if min_moon_dist > 10:
        min_moon_dist = 10

    return (1, +min_moon_dist)

def norm_1_1(a):
    while a < -math.pi: a += 2 * math.pi
    while a >  math.pi: a -= 2 * math.pi
    return a

def simulate_score(pos, vel, moon_radius, limit):
    speed_sum = 0

    a_s = math.atan2(*pos)
    total_rots = 0

    for i in range(limit):
        a_1 = norm_1_1(math.atan2(*pos) - a_s)
        pos, vel = simulate1(pos, vel)
        a_2 = norm_1_1(math.atan2(*pos) - a_s)

        if i != 0 and a_1 < 0 and a_2 > 0:
            total_rots += 2*math.pi
        if i != 0 and a_1 > 0 and a_2 < 0:
            total_rots -= 2*math.pi
        if a_2 < 0:
            a_2 += 2 * math.pi

        moon_dist = max(abs(pos[0]), abs(pos[1])) - moon_radius*2
        space_dist = 128 - max(abs(pos[0]), abs(pos[1]))
        if moon_dist <= 2 or space_dist <= 2:
            return (0, -(a_2 + total_rots))
        if i == 0:
            min_moon_dist = max_moon_dist = moon_dist
        else:
            min_moon_dist = min(min_moon_dist, moon_dist)
            max_moon_dist = max(max_moon_dist, moon_dist)
        speed_sum += abs(vel[0]) + abs(vel[1])

    if min_moon_dist > 20:
        min_moon_dist = 20

    return (1, +min_moon_dist)

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

if __name__ == "__main__":
    main()
