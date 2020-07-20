from typing import List
import math

class Ship:
    is_defender: bool = None
    ship_id: int = None
    xy_coordinates: (int, int) = None
    xy_velocity: (int, int) = None
    x4: List[int] = None
    x5: int = None
    x6: int = None
    x7: int = None

    def __str__(self) -> str:
        return "[{:02d}{}; {:03d}{:+03d} {:03d}{:+03d}; x4: {}; x5: {}; x6: {}; x7: {}]".format(
            self.ship_id,
            "D" if self.is_defender else "A",
            self.xy_coordinates[0],
            self.xy_velocity[0],
            self.xy_coordinates[1],
            self.xy_velocity[1],
            self.x4,
            self.x5,
            self.x6,
            self.x7,
        )

class GameState:
    our_fleet: List[Ship] = None
    enemy_fleet: List[Ship] = None
    we_defend: bool = None
    moon_radius: int = None
    turn: int = None

    def __str__(self) -> str:
        return "We {}, moon size is {}, it's {} turn\n Frndl fleet: {}\n Enemy fleet: {}".format(
            "Defend" if self.we_defend else "Attack",
            self.moon_radius,
            self.turn,
            "".join([str(x) for x in self.our_fleet]),
            "".join([str(x) for x in self.enemy_fleet])
        )

def parse_ship(ship):
    parsed_ship = Ship()
    parsed_ship.is_defender = ship[0] == 1
    parsed_ship.ship_id = ship[1]
    parsed_ship.xy_coordinates = (ship[2][0], ship[2][1])
    parsed_ship.xy_velocity = (ship[3][0], ship[3][1])
    parsed_ship.x4 = ship[4]
    parsed_ship.x5 = ship[5]
    parsed_ship.x6 = ship[6]
    parsed_ship.x7 = ship[7]
    return parsed_ship

def parse_game_data(game_data):
    game_state = GameState()
    game_state.we_defend = game_data[2][1] == 1
    game_state.moon_radius = round(math.sqrt(game_data[2][2][0]/2))
    game_state.turn = game_data[3][0]
    sides = []
    for ship_and_history_by_side in game_data[3][2]:
        sides.append([])
        for ship in ship_and_history_by_side[:1]:
            sides[-1].append(parse_ship(ship))

    if (len(sides) != 2):
        print("+"*15, "Totally", len(sides), "sides")

    if sides[0][0].is_defender and game_state.we_defend:
        game_state.our_fleet = sides[0]
        game_state.enemy_fleet = sides[1]
    else:
        game_state.our_fleet = sides[1]
        game_state.enemy_fleet = sides[0]
    return game_state
