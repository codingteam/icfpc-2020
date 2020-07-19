import math
import sys
import traceback
import random

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
is_running = True
prev_velocities = {}
throttle = 0
max_throttle = 1

try:
    print("-" * 30)
    game_data = send_request([3, player_key,
                              [150, # fuel?
                               0,
                               10,
                               1]
                              ])
    parsed_data = parse_game_data(game_data)
    print(parse_game_data(game_data))
    for ship in parsed_data.our_fleet:
        prev_velocities[ship.ship_id] = ship.xy_velocity

except Exception:
    print(traceback.format_exc())


def get_rotated_vector(vector):
    return [-vector[1], vector[0]]

def get_vector_magnitude(vector):
    return math.sqrt(vector[0]**2 + vector[1]**2)

def normalize_vector(vector):
    magnitude = get_vector_magnitude(vector)
    if magnitude == 0:
        return [0 ,0]
    return [
        round(vector[0]/magnitude),
        round(vector[1]/magnitude)
    ]

while is_running:
    try:
        print("-" * 30)
        commands = []
        for ship in parsed_data.our_fleet:
            if (parsed_data.we_defend):
                # stay in place, if defender
                new_vector = ship.xy_velocity
            else:
                # try to orbit, if attacker
                new_vector = get_rotated_vector(ship.xy_coordinates)

                if get_vector_magnitude(ship.xy_coordinates) <= parsed_data.moon_radius:
                    throttle = max_throttle
                else:
                    throttle = max(0, throttle - 1)
                print("Throttle:", throttle, "| distance to the moon", get_vector_magnitude(ship.xy_coordinates), "| moon radius", parsed_data.moon_radius)
                if throttle == 0 or random.randint(0, max_throttle) > throttle:
                    new_vector = [0, 0]

            acceleration_vector = normalize_vector(new_vector)
            if acceleration_vector != [0, 0]:
                commands.append([
                    0,  # acceleration command
                    ship.ship_id,
                    acceleration_vector
                ])
            prev_velocities[ship.ship_id] = ship.xy_velocity
        game_data = send_request([4, player_key, commands])
        if len(game_data) > 1 and game_data[1] == 2:
            is_running = False
        if is_running and game_data[0] == 1:
            parsed_data = parse_game_data(game_data)
            print(parsed_data)
        else:
            print("is running:", is_running)
            print("server error:", game_data[0])
    except Exception:
        print(traceback.print_exc())
