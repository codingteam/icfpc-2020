import logging
logging.basicConfig(level=logging.INFO)

def demodulate_list(string):
    logging.debug("+"*25)
    assert len(string) >= 2

    result = []
    args = [0]

    shift = 0
    while shift < len(string):
        logging.debug("-" * 5)
        logging.debug(string[shift:])

        mode = string[shift : shift+2]

        if mode == "00": # just ignore nils
            logging.debug("nil")
            args[-1] += 1
            shift = shift + 2

        if mode == "11": # a definition of pair/list
            logging.debug("pair")
            # see https://message-from-space.readthedocs.io/en/latest/message35.html
            result.append("[")
            args.append(0)
            shift = shift + 2
            pass

        if mode == "01" or mode == "10": # number
            logging.debug("number")
            # see https://message-from-space.readthedocs.io/en/latest/message13.html
            n = 0
            while True:
                char = string[shift + 2 + n]
                if char == "1":
                    n += 1
                else:
                    break

            number = 0
            number_bits = string[shift + 3 + n : shift + 3 + n + 4 * n]

            for index, number_bit in enumerate(reversed(number_bits)):
                if number_bit == "1":
                    number += 2 ** index

            if mode == "10":
                number *= -1

            result.append(str(number) + ", ")
            args[-1] = args[-1] + 1

            shift = shift + 3 + n + 4 * n

        while args[-1] == 2:
            result.append("],")
            args.pop(len(args)-1)
            args[-1] = args[-1] + 1

        logging.debug("".join(result))

    nested_list = eval("".join(result))[0]

    return nested_list

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

assert demodulate_list("110000") == [] # pair
assert demodulate_list("1101000") == [0]  # pair

assert demodulate_list("110110000101100010") == [1, 2] # pair
assert demodulate_list("1101100001110110001000") == [1, [2]]  # pair

assert demodulate_list("1101100001111101100010110110001100110110010000") == [1, [[2, [3]], [4]]] # nested pairs