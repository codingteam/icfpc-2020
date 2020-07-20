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
turn = 0

try:
    print("-" * 30)
    game_data = send_request([3, player_key,
                              [140, # fuel?
                               0, # guns? Max 44 for 150 fuel
                               24,
                               10]
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

new_ships = []

def get_new_ships(old, new):
    old_ids = set(ship.ship_id for ship in old)
    new = filter(lambda ship: ship.ship_id not in old_ids, new)
    return new

def play_a_turn():
    global parsed_data
    global is_running

    print("-" * 30)
    commands = []

    for ship in parsed_data.our_fleet:
        is_new = ship.ship_id in new_ships
        print("Ship {} is new? {}".format(ship.ship_id, is_new))
        if ship.is_defender and not is_new:
            acceleration_command = calculate_acceleration_corner(ship, parsed_data.moon_radius)
        else:
            # try to orbit
            acceleration_command = calculate_circular_acceleration(ship, parsed_data.moon_radius)
        if acceleration_command is not None:
            commands.append(acceleration_command)

    if turn > 10:
        for ship in parsed_data.our_fleet:
            if ship.x4[3] > 1:
                commands.append([3, ship.ship_id, [20, 0, 0, 1]])
                print("Ship {} spawned a new ship".format(ship.ship_id))

    commands.extend(
        suggest_shooting_commands(
            parsed_data.our_fleet,
            parsed_data.enemy_fleet))

    game_data = send_request([4, player_key, commands])
    if len(game_data) > 1 and game_data[1] == 2:
        is_running = False
    if is_running and game_data[0] == 1:
        old_parsed_data = parsed_data
        parsed_data = parse_game_data(game_data)
        spawned = get_new_ships(old_parsed_data.out_fleet, parsed_data.out_fleet)
        print("New ships are:", [ship.ship_id for ship in spawned])
        new_ships.append(spawned)
        print(parsed_data)
    else:
        print("is running:", is_running)
        print("server error:", game_data[0])

while is_running:
    try:
        play_a_turn()
        turn += 1
    except Exception:
        print(traceback.print_exc())
