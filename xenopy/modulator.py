
def modulate(x):
    if isinstance(x, list):
        return mod_list(x)
    elif isinstance(x, int):
        return mod_number(x)
    else:
        raise Exception(f"Unsupported data: {x}")

def mod_number(x):
    if x == 0:
        return "010"
    signum = "01" if x >= 0 else "10"
    x = abs(x)
    binary = "{:b}".format(x)
    n = len(binary)
    m = n % 4
    if m != 0:
        n += (4 - m)
        binary = "0"*(4-m) + binary
        #print(binary)
    n = n // 4
    len_prefix = "1" * n + "0"
    #print(len_prefix)
    return signum + len_prefix + binary

def mod_list(lst):
    if lst == []:
        return '00'
    result = ''
    for x in lst:
        result += '11' + modulate(x)
    result += '00'
    return result

