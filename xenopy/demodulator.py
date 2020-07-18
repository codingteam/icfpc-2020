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
            result.append("None, ")
            args[-1] += 1
            shift = shift + 2

        if mode == "11": # a definition of pair/list
            logging.debug("pair")
            # see https://message-from-space.readthedocs.io/en/latest/message35.html
            if args[-1] == 0:
                result.append("[")
                args.append(0)
            else:
                args[-1] = 0
            shift = shift + 2

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
            result.append("], ")
            args.pop(len(args)-1)
            args[-1] = args[-1] + 1

        logging.debug("".join(result))

    nested_list = eval("".join(result))[0]

    return nested_list
