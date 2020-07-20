from typing import List
from state_parsing import *

def next_position(current_position, velocity):
    return (
            current_position[0] + velocity[0],
            current_position[1] + velocity[1]
            )

def distance(x, y):
    dx = x[0] - y[0]
    dy = x[1] - y[1]
    return max(abs(dx), abs(dy))

def find_nearest_enemy(us_pos: (int, int), enemies: List[Ship]):
    (distance, enemy) = (100500, None)

    for candidate in enemies:
        d = distance(us_pos, candidate.xy_coordinates)
        if d < distance:
            distance = d
            enemy = candidate

    if distance < 15:
        return enemy
    else:
        return None

def suggest_shooting_commands(us: List[Ship], enemies: List[Ship]):
    print("[SHOOTING MODULE]")
    commands = []

    ready_to_shoot = filter(lambda ship: ship.x4[1] != 0, us)
    for us in ready_to_shoot:
        them = find_nearest_enemy(us.xy_coordinates, enemies)
        if not them:
            continue

        target = next_position(them.xy_coordinates, them.xy_velocity)
        commands.append([
            2, # shoot
            us.ship_id,
            target,
            us.x4[1] / us.x4[3]
            ])
        print("Ship {} shooting at enemy {} at {} with power {}"
                .format(
                    us.ship_id,
                    them.ship_id,
                    target,
                    us.x4[1]
                    ))

    print(" -"*15)
    return commands
