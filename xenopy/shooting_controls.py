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
    (best_distance, enemy) = (100500, None)

    for candidate in enemies:
        d = distance(us_pos, candidate.xy_coordinates)
        if d < best_distance:
            best_distance = d
            enemy = candidate

    if distance < 15:
        return enemy
    else:
        return None

def suggest_shooting_commands(us: List[Ship], enemies: List[Ship]):
    print("[SHOOTING MODULE]")
    commands = []

    for s in us:
        print("Ship {} has energy {}".format(s.ship_id, s.ship_params[1]))

    ready_to_shoot = filter(lambda ship: ship.ship_params[1] != 0, us)
    for us in ready_to_shoot:
        them = find_nearest_enemy(us.xy_coordinates, enemies)
        if not them:
            continue

        target = next_position(them.xy_coordinates, them.xy_velocity)
        commands.append([
            2, # shoot
            us.ship_id,
            target,
            us.ship_params[1] / us.ship_params[3]
            ])
        print("Ship {} shooting at enemy {} at {} with power {}"
                .format(
                    us.ship_id,
                    them.ship_id,
                    target,
                    us.ship_params[1]
                    ))

    print(" -"*15)
    return commands
