from demodulator import demodulate_list
from modulator import modulate

# demodulator for numbers
assert demodulate_list("010") == 0 # 0
assert demodulate_list("01100001") == 1 # 1
assert demodulate_list("10100001") == -1 # -1
assert demodulate_list("01100010") == 2 # 2
assert demodulate_list("10100010") == -2 # -2

assert demodulate_list("0111000010000") == 16 # 16
assert demodulate_list("1011000010000") == -16 # -16

assert demodulate_list("0111011111111") == 255 # 255
assert demodulate_list("1011011111111") == -255 # -255

assert demodulate_list("011110000100000000") == 256 # 256
assert demodulate_list("101110000100000000") == -256 # -256

# demodulator for lists
assert demodulate_list("11 00 00".replace(" ", "")) == [None, None]
assert demodulate_list("11 010 00".replace(" ", "")) == [0, None]

assert demodulate_list("11 01100001 01100010".replace(" ", "")) == [1, 2]
assert demodulate_list("11 01100001 11 01100010 00".replace(" ", "")) == [1, 2, None]

assert demodulate_list("11 01100001 11 01100010 11 00 11 00 00".replace(" ", "")) == [1, 2, None, None, None]

assert demodulate_list("11 01100001 11 11 01100010 11 01100011 00 11 01100100 00".replace(" ", "")) == [1, [2, 3, None], 4, None]

# modulator + demodulator for lists give the same result
assert modulate(demodulate_list("11 00 00".replace(" ", ""))) == "11 00 00".replace(" ", "")
assert modulate(demodulate_list("11 010 00".replace(" ", ""))) == "11 010 00".replace(" ", "")

assert modulate(demodulate_list("11 01100001 01100010".replace(" ", ""))) == "11 01100001 01100010".replace(" ", "")
assert modulate(demodulate_list("11 01100001 11 01100010 00".replace(" ", ""))) == "11 01100001 11 01100010 00".replace(" ", "")

assert modulate(demodulate_list("11 01100001 11 01100010 11 00 11 00 00".replace(" ", ""))) == "11 01100001 11 01100010 11 00 11 00 00".replace(" ", "")

assert modulate(demodulate_list("11 01100001 11 11 01100010 11 01100011 00 11 01100100 00".replace(" ", ""))) == "11 01100001 11 11 01100010 11 01100011 00 11 01100100 00".replace(" ", "")
