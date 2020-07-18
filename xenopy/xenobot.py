import sys
from time import sleep

import requests

from demodulator import demodulate_list
from modulator import modulate

url = sys.argv[1]
player_key = int(sys.argv[2])

def send_request(data):
    """
    see https://message-from-space.readthedocs.io/en/latest/game.html#join
    first value in data is command: 2 is to JOIN, 3 is to START, 4 is COMMANDS
    second value in data is player key
    third value is unknown
    forth value is unknown

    first value in response is 1 (always success?)
    second value in response: 0 -- game hasn't started yet; 1 -- game already started; 2 -- game has finished
    third value is static list
    fourth value is dynamic list
    """
    print("request:", data)
    mod_data = modulate(data)
    print("MOD request:", mod_data)

    x = requests.post(url + "/aliens/send", mod_data)

    print("response", x.text)
    demod_response = demodulate_list(x.text)
    print("DEMOD response", demod_response)
    return demod_response

init_data = send_request([2, player_key, None, None])

print("-"*30)
send_request([3, player_key, init_data, None])

while True:
    print("-"*30)
    send_request([4, player_key, None, None])
    sleep(0.5)
