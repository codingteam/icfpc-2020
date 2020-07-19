from typing import List

class Ship:
    is_defender: bool = None
    ship_id: int = None
    xy_coordintes: List[int] = None
    xy_velocity: List[int] = None

    def __str__(self) -> str:
        return "[{:02d}{}; {:03d}{:+03d} {:03d}{:+03d}]".format(
            self.ship_id,
            "D" if self.is_defender else "A",
            self.xy_coordintes[0],
            self.xy_velocity[0],
            self.xy_coordintes[1],
            self.xy_velocity[1]
        )

class GameState:
    our_fleet: List[Ship] = None
    enemy_fleet: List[Ship] = None
    we_defend: bool = None

    def __str__(self) -> str:
        return "We {}\n Frndl fleet: {}\n Enemy fleet: {}".format(
            "Defend" if self.we_defend else "Attack",
            "".join([str(x) for x in self.our_fleet]),
            "".join([str(x) for x in self.enemy_fleet])
        )

def parse_ship(ship):
    print(ship)
    parsed_ship = Ship()
    parsed_ship.is_defender = ship[0] == 1
    parsed_ship.ship_id = ship[1]
    parsed_ship.xy_coordintes = ship[2]
    parsed_ship.xy_velocity = ship[3]
    return parsed_ship

def parse_game_data(game_data):
    game_state = GameState()
    game_state.we_defend = game_data[2][1] == 1
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
