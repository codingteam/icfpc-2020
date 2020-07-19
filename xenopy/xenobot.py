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
    #print("MOD request:", mod_data)

    x = requests.post(url + "/aliens/send" + api_key, mod_data)

    #print("response", x.text)
    demod_response = demodulate_list(x.text)
    print("DEMOD response", demod_response)
    return demod_response


init_data = send_request([2, player_key, []])
is_running = True
gravity_constant = 0.004
desired_orbit_over_moon = 25

try:
    print("-" * 30)
    game_data = send_request([3, player_key,
                              [255, # fuel?
                               0,
                               10,
                               1]
                              ])
    parsed_data = parse_game_data(game_data)
    print(parse_game_data(game_data))

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

distances = []
while is_running:
    try:
        print("-" * 30)
        commands = []
        for ship in parsed_data.our_fleet:
            # try to orbit

            desired_orbit_height = parsed_data.moon_radius * math.sqrt(2) + desired_orbit_over_moon
            desired_orbital_velocity = math.sqrt(
                parsed_data.moon_radius ** 2 * gravity_constant / desired_orbit_height
            ) * desired_orbit_height # convert to linear speed
            current_velocity = get_vector_magnitude(ship.xy_velocity)

            distances.append(get_vector_magnitude(ship.xy_coordinates))
            print(
                "desired orbital velocity {:.1f}, current velocity {:.1f}".format(desired_orbital_velocity, current_velocity),
                " | desired distance {:.1f}, avg distance {:.1f} ".format(desired_orbit_height, sum(distances[:10])/len(distances[:10]))
            )

            if current_velocity - desired_orbital_velocity < -2.5: # too slow
                print("fixing too slow speed {:.1f}".format(current_velocity - desired_orbital_velocity))
                new_vector = get_rotated_vector(ship.xy_coordinates) # rotate
            elif current_velocity - desired_orbital_velocity > 2.5: # too fast
                print("fixing too fast speed {:.1f}".format(current_velocity - desired_orbital_velocity))
                new_vector = ship.xy_velocity # slow down
            else:
                print("speed in boundaries")
                new_vector = [0, 0]

            acceleration_vector = normalize_vector(new_vector)
            print("new_vector:", new_vector, "acceleration_vector", acceleration_vector)
            if acceleration_vector != [0, 0]:
                commands.append([
                    0,  # acceleration command
                    ship.ship_id,
                    acceleration_vector
                ])
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
