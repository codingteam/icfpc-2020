class Ship:
    is_defender = None
    ship_id = None
    xy_coordintes = None
    xy_velocity = None

    def __repr__(self) -> str:
        return "Ship {} ({}); xy {} xy'{}".format(
            self.ship_id,
            "D" if self.is_defender else "A",
            self.xy_coordintes,
            self.xy_velocity
        )

class GameState:
    our_fleet = None
    enemy_fleet = None
    we_defend = None

    def __repr__(self) -> str:
        return "We {}\n\tOur fleet:{}\n\tEnemy fleet: {}".format(
            "Defend" if self.we_defend else "Attack",
            " ".join([str(x) for x in self.our_fleet]),
            " ".join([str(x) for x in self.enemy_fleet])
        )

def parse_ship(ship):
    parsed_ship = Ship()
    parsed_ship.is_defender = ship[0] == 1
    parsed_ship.ship_id = ship[1]
    parsed_ship.xy_coordintes = ship[2]
    parsed_ship.xy_velocity = ship[3]
    return ship

def parse_game_data(game_data):
    game_state = GameState
    game_state.we_defend = game_data[2][1] == 1
    sides = []
    for side in game_data[3][2]:
        sides.append([])
        for ship in side:
            sides[-1].append(parse_ship(ship))

    print("Totally", len(sides), "sides")
    if sides[0][0].is_defender and game_state.we_defend:
        game_state.our_fleet = sides[0]
        game_state.enemy_fleet = sides[1]
    else:
        game_state.our_fleet = sides[1]
        game_state.enemy_fleet = sides[0]
    return game_state
