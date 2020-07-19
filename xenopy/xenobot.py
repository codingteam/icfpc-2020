import sys
import traceback

import requests

from demodulator import demodulate_list
from modulator import modulate
from state_parsing import *
from acceleration_controls import *

url = sys.argv[1]
player_key = int(sys.argv[2])

if len(sys.argv) > 3:
    api_key = "?apiKey=" + sys.argv[3]
else:
    api_key = ""


def send_request(data):
    """
    see https://message-from-space.readthedocs.io/en/latest/game.html
    """
    print("request:", data)
    mod_data = modulate(data)
    #print("MOD request:", mod_data)

    x = requests.post(url + "/aliens/send" + api_key, mod_data)

    #print("response", x.text)
    demod_response = demodulate_list(x.text)
    print("DEMOD response", demod_response)
    return demod_response


init_data = send_request([2, player_key, []])
is_running = True

try:
    print("-" * 30)
    game_data = send_request([3, player_key,
                              [156, # fuel?
                               4, # guns?
                               10,
                               1]
                              ])
    parsed_data = parse_game_data(game_data)
    print(parse_game_data(game_data))

except Exception:
    print(traceback.format_exc())


def next_position(current_position, velocity):
    return list(map(lambda x: x[0] + x[1], zip(current_position, velocity)))

def play_a_turn():
    global parsed_data
    global is_running

    sent_successfully = False

    targets = None

    while not sent_successfully:
        print("-" * 30)
        commands = []
        for ship in parsed_data.our_fleet:
            # try to orbit
            acceleration_vector = calculate_acceleration(ship, parsed_data.moon_radius)
            if acceleration_vector != [0, 0]:
                commands.append([
                    0,  # acceleration command
                    ship.ship_id,
                    acceleration_vector
                ])

        # Shooting
        us = parsed_data.our_fleet[0]
        them = parsed_data.enemy_fleet[0]

        if targets is None:
            pos = next_position(them.xy_coordinates, them.xy_velocity)
            targets = []
            for dx in range(-2, 3):
                for dy in range(-2, 3):
                    targets.append([pos[0]+dx, pos[1]+dy])

        if not (targets is None) and not (targets == []):
                target = targets.pop()
                # Shooting parameters. No idea what they mean or if they're correct
                params = (us.x4[1]-1, 0, 4)
                commands.append([
                    2, # shoot
                    us.ship_id,
                    target,
                    *params
                    ])
                print("Ship {} shooting at enemy {} at {} with params {}"
                        .format(
                            us.ship_id,
                            them.ship_id,
                            target,
                            params
                            ))

        game_data = send_request([4, player_key, commands])
        if len(game_data) > 1 and game_data[1] == 2:
            is_running = False
        if is_running and game_data[0] == 1:
            parsed_data = parse_game_data(game_data)
            print(parsed_data)
        else:
            print("is running:", is_running)
            print("server error:", game_data[0])

        sent_successfully = (game_data != [0])

while is_running:
    try:
        play_a_turn()
    except Exception:
        print(traceback.print_exc())
