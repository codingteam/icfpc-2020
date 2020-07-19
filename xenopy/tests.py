from demodulator import demodulate_list
from modulator import modulate
from state_parsing import *

# modulator for numbers

assert modulate(0) == "010"
assert modulate(1) == "011 0000 1".replace(" ", "")
assert modulate(-1) == "1 0 1 0000 1".replace(" ", "")
assert modulate(2) == "0 11 000 1 0".replace(" ", "")
assert modulate(-2) == "1 0 1 000 1 0".replace(" ", "")

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
assert demodulate_list("11 00 00".replace(" ", "")) == []
assert demodulate_list("11 010 00".replace(" ", "")) == [0]

assert demodulate_list("11 01100001 01100010".replace(" ", "")) == [1, 2]
assert demodulate_list("11 01100001 11 01100010 00".replace(" ", "")) == [1, 2]

assert demodulate_list("11 01100001 11 01100010 11 00 11 00 00".replace(" ", "")) == [1, 2]

assert demodulate_list("11 01100001 11 11 01100010 11 01100011 00 11 01100100 00".replace(" ", "")) == [1, [2, 3], 4]

# modulator + demodulator for lists give the same result
assert modulate(demodulate_list("11 00 00".replace(" ", ""))) == "00".replace(" ", "")
assert modulate(demodulate_list("11 010 00".replace(" ", ""))) == "11 010 00".replace(" ", "")

assert modulate(demodulate_list("11 01100001 01100010".replace(" ", ""))) == "11 01100001 01100010".replace(" ", "")
assert modulate(demodulate_list("11 01100001 01100010".replace(" ", ""))) == "110110000101100010".replace(" ", "")

assert modulate(demodulate_list("11 01100001 01100010".replace(" ", ""))) == "11 01100001 01100010".replace(" ", "")

assert modulate(demodulate_list("11 0110000 11 11 101100010 01100011 11 01100100 00".replace(" ", ""))) == "11 0110000 11 11 101100010 01100011 11 01100100 00".replace(" ", "")

# state parsing tests
state = [1, 1, [256, 1, [512, 1, 64], [16, 128], [5, 15, 20, 25]], [0, [16, 128], [[[1, 0, [48, -2], [-5, -5], [5, 15, 20, 25], 0, 64, 1]], [[0, 1, [-48, 2], [0, 0], [5, 15, 20, 25], 0, 64, 1]]]]]
game_state = parse_game_data(state)

assert game_state.we_defend is True

assert len(game_state.our_fleet) == 1
assert game_state.our_fleet[0].is_defender is True
assert game_state.our_fleet[0].ship_id == 0
assert game_state.our_fleet[0].xy_coordinates == [48, -2]
assert game_state.our_fleet[0].xy_velocity == [-5, -5]

assert len(game_state.enemy_fleet) == 1
assert game_state.enemy_fleet[0].is_defender is False
assert game_state.enemy_fleet[0].ship_id == 1
assert game_state.enemy_fleet[0].xy_coordinates == [-48, 2]
assert game_state.enemy_fleet[0].xy_velocity == [0, 0]

assert game_state.moon_radius == 16
assert game_state.turn == 0

state = [1, 1, [256, 0, [448, 1, 64], [16, 128], [250, 0, 16, 1]], [6, [16, 128], [[[1, 0, [42, -34], [-1, 0], [245, 0, 16, 1], 0, 64, 1], [[0, [-1, 0]]]], [[0, 1, [-28, 33], [5, -1], [250, 0, 16, 1], 0, 64, 1]]]]]
game_state = parse_game_data(state)

assert game_state.we_defend is False

assert len(game_state.our_fleet) == 1
assert game_state.our_fleet[0].is_defender is False
assert game_state.our_fleet[0].ship_id == 1
assert game_state.our_fleet[0].xy_coordinates == [-28, 33]
assert game_state.our_fleet[0].xy_velocity == [5, -1]

assert len(game_state.enemy_fleet) == 1
assert game_state.enemy_fleet[0].is_defender is True
assert game_state.enemy_fleet[0].ship_id == 0
assert game_state.enemy_fleet[0].xy_coordinates == [42, -34]
assert game_state.enemy_fleet[0].xy_velocity == [-1, 0]

assert game_state.moon_radius == 15
assert game_state.turn == 6
