import sys
import traceback

import requests

from demodulator import demodulate_list
from modulator import modulate
from state_parsing import *
from acceleration_controls import *
from shooting_controls import *

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


init_data = send_request([2, player_key, [1,2,3,4]])
static_game_info = init_data[2]
role = static_game_info[1]
is_attacker = role == 0

is_running = True

try:
    print("-" * 30)
    print("We're attacker?", is_attacker)
    fuel = 256 if is_attacker else 150
    game_data = send_request([3, player_key,
                              [fuel, # fuel?
                               16, # guns? Max 44 for 150 fuel
                               4,
                               8]
                              ])
    parsed_data = parse_game_data(game_data)
    print(parse_game_data(game_data))

except Exception:
    print(traceback.format_exc())


def next_position(current_position, velocity):
    return (
            current_position[0] + velocity[0],
            current_position[1] + velocity[1]
            )

def play_a_turn():
    global parsed_data
    global is_running

    print("-" * 30)
    commands = []

    for ship in parsed_data.our_fleet:
        if ship.is_defender and False: # disabled
            acceleration_command = calculate_acceleration_corner(ship, parsed_data.moon_radius)
        else:
            # try to orbit
            acceleration_command = calculate_circular_acceleration(ship, parsed_data.moon_radius)
        if acceleration_command is not None:
            commands.append(acceleration_command)

    if parsed_data.our_fleet[0].x4[3] > 1:
        commands.append([3, ship.ship_id, [20, 2, 0, 1]])

    commands.extend(
        suggest_shooting_commands(
            parsed_data.our_fleet,
            parsed_data.enemy_fleet))

    game_data = send_request([4, player_key, commands])
    if len(game_data) > 1 and game_data[1] == 2:
        is_running = False
    if is_running and game_data[0] == 1:
        parsed_data = parse_game_data(game_data)
        print(parsed_data)
    else:
        print("is running:", is_running)
        print("server error:", game_data[0])

while is_running:
    try:
        play_a_turn()
    except Exception:
        print(traceback.print_exc())
