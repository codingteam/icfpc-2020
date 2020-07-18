import sys
from time import sleep

import requests

from demodulator import demodulate_list
from modulator import modulate

url = sys.argv[1]
player_key = int(sys.argv[2])

def send_request(data):
    print("request:", data)
    mod_data = modulate(data)
    print("MOD request:", mod_data)

    x = requests.post(url + "/aliens/send", mod_data)

    print("response", x.text)
    print("DEMOD response", demodulate_list(x.text))

send_request([2, player_key, []])

print("-"*30)
send_request([3, player_key, [5, 10, 20, 40]])

while True:
    print("-"*30)
    send_request([4, player_key, []])
    sleep(0.5)
