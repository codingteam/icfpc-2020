from demodulator import demodulate_list
from modulator import modulate

assert demodulate_list("11 00 00".replace(" ", "")) == [None, None]
assert demodulate_list("11 010 00".replace(" ", "")) == [0, None]

assert demodulate_list("11 01100001 01100010".replace(" ", "")) == [1, 2]
assert demodulate_list("11 01100001 11 01100010 00".replace(" ", "")) == [1, 2, None]

assert demodulate_list("11 01100001 11 01100010 11 00 11 00 00".replace(" ", "")) == [1, 2, None, None, None]

assert demodulate_list("11 01100001 11 11 01100010 11 01100011 00 11 01100100 00".replace(" ", "")) == [1, [2, 3, None], 4, None]


assert modulate(demodulate_list("11 00 00".replace(" ", ""))) == "11 00 00".replace(" ", "")
assert modulate(demodulate_list("11 010 00".replace(" ", ""))) == "11 010 00".replace(" ", "")

assert modulate(demodulate_list("11 01100001 01100010".replace(" ", ""))) == "11 01100001 01100010".replace(" ", "")
assert modulate(demodulate_list("11 01100001 11 01100010 00".replace(" ", ""))) == "11 01100001 11 01100010 00".replace(" ", "")

assert modulate(demodulate_list("11 01100001 11 01100010 11 00 11 00 00".replace(" ", ""))) == "11 01100001 11 01100010 11 00 11 00 00".replace(" ", "")

assert modulate(demodulate_list("11 01100001 11 11 01100010 11 01100011 00 11 01100100 00".replace(" ", ""))) == "11 01100001 11 11 01100010 11 01100011 00 11 01100100 00".replace(" ", "")
