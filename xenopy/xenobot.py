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
is_running = True
zero_bot_num = 20

try:
    print("-" * 30)
    game_data = send_request([3, player_key,
                              [2100//(zero_bot_num*5+1),  # fuel?
                               0,  # guns? Max 44 for 150 fuel, >= 0 for shooter, == 0 for replication
                               24, # type 24 is replicator, 0 is shooter
                               zero_bot_num # replication number, >= 0 for replication , == 0 for shooter
                               ]
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
    global current_type

    print("-" * 30)
    commands = []

    for ship in parsed_data.our_fleet:
        if ship.ship_params[3] == 24 and ship.ship_params[0] > 10:
            # try to go into the corner if enough fuel is left
            acceleration_command = calculate_acceleration_corner(ship, parsed_data.moon_radius)
        else:
            # try to orbit
            acceleration_command = calculate_circular_acceleration(ship, parsed_data.moon_radius, desired_orbit_over_moon_surface=25+ship.ship_id*2)
        if acceleration_command is not None:
            commands.append(acceleration_command)

    if parsed_data.turn == 10: # stable enough!
        for ship in parsed_data.our_fleet:
            if ship.ship_params[3] == 24: #spawner
                new_ship_params = [ship.ship_params[0] // (ship.ship_params[3] + 1), 0, 0, 1]

                commands.append([3, ship.ship_id, new_ship_params])
                print("Ship {} spawns a new ship with parameters {}".format(ship.ship_id, new_ship_params))
        current_type += 1

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
