import random
import sys
import traceback

import requests

from demodulator import demodulate_list
from modulator import modulate
from state_parsing import *

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
    print("MOD request:", mod_data)

    x = requests.post(url + "/aliens/send" + api_key, mod_data)

    print("response", x.text)
    demod_response = demodulate_list(x.text)
    print("DEMOD response", demod_response)
    return demod_response


init_data = send_request([2, player_key, []])
prev_velocities = {}

try:
    print("-" * 30)
    game_data = send_request([3, player_key,
                              [150, # fuel?
                               0,
                               16,
                               1]
                              ])
    parsed_data = parse_game_data(game_data)
    print(parse_game_data(game_data))
    for ship in parsed_data.our_fleet:
        prev_velocities[ship.ship_id] = ship.xy_velocity

except Exception:
    print(traceback.format_exc())


def get_rotated_vector(x, y):
    return -y, x


while True:
    try:
        print("-" * 30)
        commands = []
        for ship in parsed_data.our_fleet:
            if (parsed_data.we_defend):
                # stay in place, if defender
                new_x = ship.xy_velocity[0]
                new_y = ship.xy_velocity[1]
            else:
                # try to orbit, if attacker
                new_x, new_y = get_rotated_vector(
                    -ship.xy_coordintes[0],
                    -ship.xy_coordintes[1]
                )
            commands.append([
                0,  # acceleration command
                ship.ship_id,
                [
                    new_x,
                    new_y
                ]
            ])
            prev_velocities[ship.ship_id] = ship.xy_velocity
        game_data = send_request([4, player_key, commands])
        parsed_data = parse_game_data(game_data)
    except Exception:
        print(traceback.print_exc())
