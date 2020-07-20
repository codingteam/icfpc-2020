from state_parsing import *

gravity_constant = 1
distances = []

def get_rotated_vector(vector, ccw: bool):
    if (ccw):
        return (-vector[1], vector[0])
    else:
        return (vector[1], -vector[0])

def get_opposite_vector(vec):
    return (-vec[0], -vec[1])

def average_vectors(vec1, vec2):
    return (
        (vec1[0] + vec2[0]) / 2,
        (vec1[1] + vec2[1]) / 2
    )

def get_vector_magnitude(vector):
    return max(abs(vector[0]), abs(vector[1]))

def normalize_vector(vector):
    magnitude = get_vector_magnitude(vector)
    if magnitude == 0:
        return (0 ,0)
    return (
        round(vector[0]/magnitude),
        round(vector[1]/magnitude)
    )

def calculate_circular_acceleration(ship: Ship, moon_radius: int, desired_orbit_over_moon_surface = 25, ccw_direction=True):
    print("[ACCELERATION MODULE]")
    desired_orbit_from_center = moon_radius + desired_orbit_over_moon_surface
    desired_orbital_velocity = math.sqrt(
        gravity_constant / desired_orbit_from_center
    ) * desired_orbit_from_center  # convert to linear speed
    current_velocity = get_vector_magnitude(ship.xy_velocity)

    current_distance = get_vector_magnitude(ship.xy_coordinates)
    distances.append(current_distance)
    print(
        " desired orbital velocity {:.1f}, current velocity {:.1f}".format(
            desired_orbital_velocity, current_velocity
        ),
        " | desired distance {:.1f}, current distance {:.1f}, avg distance {:.1f} ".format(
            desired_orbit_from_center, current_distance, sum(distances[:10]) / len(distances[:10])
        )
    )

    close_proximity = False
    if current_distance < desired_orbit_over_moon_surface:
        velocity_error_boundary = 1 * current_distance / desired_orbit_over_moon_surface
        print(" use close proximity for", end=" ")
        close_proximity = True
    else:
        velocity_error_boundary = max(1, (15 - abs(current_distance - desired_orbit_over_moon_surface)) / 10)
        print(" use far proximity for", end=" ")
    print("velocity boundary {:.1f}".format(velocity_error_boundary))

    if current_velocity - desired_orbital_velocity < -velocity_error_boundary:  # too slow
        print(" fixing too slow speed {:.1f}".format(
            current_velocity - desired_orbital_velocity))
        new_vector = get_rotated_vector(ship.xy_coordinates, ccw_direction)  # rotate
    elif current_velocity - desired_orbital_velocity > velocity_error_boundary:  # too fast
        print(" fixing too fast speed {:.1f}".format(
            current_velocity - desired_orbital_velocity))
        new_vector = ship.xy_velocity  # slow down
    else:
        print(" speed in boundaries")
        new_vector = (0, 0)

    if close_proximity: # try to yeet from the planet harder
        new_vector = average_vectors(new_vector, ship.xy_coordinates)

    acceleration_vector = normalize_vector(new_vector)
    print(" new_vector:", new_vector, "acceleration_vector", acceleration_vector)
    print(" -"*15)

    if acceleration_vector != (0, 0):
        return [
                    0,  # acceleration command
                    ship.ship_id,
                    acceleration_vector
                ]
    else:
        return None
