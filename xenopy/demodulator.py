from typing import Any

# Documentation:
# https://message-from-space.readthedocs.io/en/latest/message13.html
# https://message-from-space.readthedocs.io/en/latest/message35.html

def demodulate_list(string: str) -> Any:
    if string.count("1") + string.count("0") != len(string):
        raise Exception(f"Extra symbols: {string}")
    result, shift = parse(string, 0)
    if shift != len(string):
        raise Exception(f"Extra data: {string[shift:]}")
    return result

def parse(string: str, shift: int) -> (Any, int):
    if string[shift: shift+2] == "00": # nil aka empty list
        return [], shift+2

    if string[shift: shift+2] == "11": # a definition of tuple/list
        result = []
        while string[shift: shift+2] == "11":
            e, shift = parse(string, shift+2)
            result.append(e)
        if string[shift: shift+2] == "00": # trailing nil -> is list
            return result, shift+2
        if string[shift: shift+2] in ("01", "10"): # trailing number -> is tuple
            e, shift = parse(string, shift)
            result.append(e)
            return tuple(result), shift
        raise Exception("Unexpected list ending")

    if string[shift: shift+2] in ("01", "10"): # number
        sign = 1 if string[shift: shift+2] == "01" else -1
        shift += 2
        n = string.index("0", shift) - shift
        shift += n + 1
        return sign * int("0"+string[shift: shift + n*4], 2), shift + n*4

    raise Exception("Unexpected marker")
