from typing import List
from state_parsing import *

def next_position(current_position, velocity):
    return (
            current_position[0] + velocity[0],
            current_position[1] + velocity[1]
            )

def suggest_shooting_commands(us: List[Ship], enemies: List[Ship]):
    print("[SHOOTING MODULE]")
    commands = []

    ready_to_shoot = filter(lambda ship: ship.x4[1] != 0, us)
    for (us, them) in zip(ready_to_shoot, enemies):
        target = next_position(them.xy_coordinates, them.xy_velocity)
        commands.append([
            2, # shoot
            us.ship_id,
            target,
            us.x4[1]
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
