from state_parsing import *

gravity_constant = 0.004
distances = []

def get_rotated_vector(vector, ccw: bool):
    if (ccw):
        return [-vector[1], vector[0]]
    else:
        return [vector[1], -vector[0]]

def get_vector_magnitude(vector):
    return math.sqrt(vector[0]**2 + vector[1]**2)

def normalize_vector(vector):
    magnitude = get_vector_magnitude(vector)
    if magnitude == 0:
        return [0 ,0]
    return [
        round(vector[0]/magnitude),
        round(vector[1]/magnitude)
    ]

def calculate_acceleration(ship: Ship, moon_radius: int, desired_orbit_over_moon = 30, ccw_direction=True):
    desired_orbit_height = moon_radius * math.sqrt(2) + desired_orbit_over_moon
    desired_orbital_velocity = math.sqrt(
        moon_radius ** 2 * gravity_constant / desired_orbit_height
    ) * desired_orbit_height  # convert to linear speed
    current_velocity = get_vector_magnitude(ship.xy_velocity)

    current_distance = get_vector_magnitude(ship.xy_coordinates)
    distances.append(current_distance)
    print(
        "desired orbital velocity {:.1f}, current velocity {:.1f}".format(
            desired_orbital_velocity, current_velocity
        ),
        " | desired distance {:.1f}, current distance {:.1f}, avg distance {:.1f} ".format(
            desired_orbit_height, current_distance, sum(distances[:10]) / len(distances[:10])
        )
    )

    if current_distance < desired_orbit_over_moon:
        velocity_error_boundary = 1 * current_distance / desired_orbit_over_moon
    else:
        velocity_error_boundary = max(1, (15 - abs(current_distance - desired_orbit_over_moon))/10)

    print("velocity boundary {:.1f}".format(velocity_error_boundary))
    if current_velocity - desired_orbital_velocity < -velocity_error_boundary:  # too slow
        print("fixing too slow speed {:.1f}".format(
            current_velocity - desired_orbital_velocity))
        new_vector = get_rotated_vector(ship.xy_coordinates, ccw_direction)  # rotate
    elif current_velocity - desired_orbital_velocity > velocity_error_boundary:  # too fast
        print("fixing too fast speed {:.1f}".format(
            current_velocity - desired_orbital_velocity))
        new_vector = ship.xy_velocity  # slow down
    else:
        print("speed in boundaries")
        new_vector = [0, 0]

    acceleration_vector = normalize_vector(new_vector)
    print("new_vector:", new_vector, "acceleration_vector", acceleration_vector)

    return acceleration_vector
