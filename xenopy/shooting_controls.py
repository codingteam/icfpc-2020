from typing import List
from state_parsing import *
from acceleration_controls import get_vector_magnitude

def next_position(current_position, velocity):
    return (
            current_position[0] + velocity[0],
            current_position[1] + velocity[1]
            )

def find_nearest_enemy(us: (int, int), enemies: List[Ship]):
    distance_limit = 32 # generated totally at random :)
    (x, y) = us

    (distance, nearest) = (distance_limit + 1, None)

    for enemy in enemies:
        (ex, ey) = enemy.xy_coordinates
        line_of_sight = (x - ex, y - ey)
        d = get_vector_magnitude(line_of_sight)
        if d < distance:
            distance = d
            nearest = enemy

    if distance < distance_limit:
        return nearest

    return None

def suggest_shooting_commands(us: List[Ship], enemies: List[Ship]):
    print("[SHOOTING MODULE]")
    commands = []

    ready_to_shoot = filter(lambda ship: ship.x4[1] != 0, us)
    for us in ready_to_shoot:
        enemy = find_nearest_enemy(us.xy_coordinates, enemies)

        if enemy:
            target = next_position(enemy.xy_coordinates, enemy.xy_velocity)
            commands.append([
                2, # shoot
                us.ship_id,
                target,
                us.x4[1]
                ])
            print("Ship {} shooting at enemy {} at {} with power {}"
                    .format(
                        us.ship_id,
                        enemy.ship_id,
                        target,
                        us.x4[1]
                        ))

    print(" -"*15)
    return commands
