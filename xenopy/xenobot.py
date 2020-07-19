import sys
from time import sleep
import random
import traceback

import requests

from demodulator import demodulate_list
from modulator import modulate
from state_parsing import *

url = sys.argv[1]
player_key = int(sys.argv[2])

if len(sys.argv) > 3:
    api_key = "?apiKey="+sys.argv[3]
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

try:
    print("-"*30)
    game_data = send_request([3, player_key, [random.randint(5, 25), random.randint(5, 25), random.randint(5, 25), random.randint(5, 25)]])
    print(parse_game_data(game_data))
except Exception:
    print(traceback.format_exc())

while True:
    try:
        print("-"*30)
        game_data = send_request([4, player_key, []])
        print(parse_game_data(game_data))
        sleep(0.2)
    except Exception:
        print(traceback.print_exc())