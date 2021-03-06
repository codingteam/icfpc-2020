#!/usr/bin/env python3

import sys

from draw import Interactor

sys.setrecursionlimit(15000)

class Magic:
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return f"$magic:{self.name}:{id(self)}"

def load_rules():
    result = {}

    def f(it):
        a = next(it)
        if a == "ap":
            return [f(it), f(it)]
        else:
            try:
                return int(a)
            except ValueError:
                return a

    for line in open("../data/galaxy.txt"):
        it = iter(line.strip().split(" "))
        name = next(it)
        next(it) # =
        result[name] = f(it)

    return result

RULES = load_rules()

class TooFewArgs(Exception):
    pass

def to_list(a):
    # nil $1 $2 $3        = $2
    # (cons a b) $1 $2 $3 = $1 a b $2 $3
    m1 = Magic(1)
    m2 = Magic(2)
    m3 = Magic(3)
    v = eval([a, m1, m2, m3])
    if v == m2 or v == [m2]:
        return None
    if isinstance(v, list) and len(v) == 5 and v[0] == m1 and v[3] == m2 and v[4] == m3:
        return (v[1], v[2])
    raise Exception(f"Strange list: {a}, got {v}, magic: {[m1,m2,m3]}")

def to_bool(a):
    m1 = Magic(4)
    m2 = Magic(5)
    v = eval([a, m1, m2])
    if v == m1 or v == [m1]:
        return True
    if v == m2 or v == [m2]:
        return False
    raise Exception(f"Strange bool: {a}, got {v}, magic: {[m1,m2]}")

def from_bool(a):
    if a is True: return "t"
    if a is False: return "f"
    raise Exception(f"Strange bool: {a}")

def f38(protocol, things):
    flag, things = to_list(things)
    flag = eval_i(flag)
    print("flag = ", flag)
    newState, things = to_list(things)
    newState = eval(newState)
    print("newState = ", newState)
    if flag == 0:
        pass
    else:
        pass
    raise Exception("Done")

def x_div(a, b):
    a = eval_i(a)
    b = eval_i(b)
    if a < 0 or b < 0:
        raise Exception(f"div: {a} {b}")
    return a // b

ops = {
    'add':   (2, lambda a, b:    eval_i(a) + eval_i(b)),
    'mul':   (2, lambda a, b:    eval_i(a) * eval_i(b)),
    'b':     (3, lambda a, b, c: [a, [b, c]]),
    'c':     (3, lambda a, b, c: [a, c, b]),
    'div':   (2, x_div),
    'eq':    (2, lambda a, b:    from_bool(eval_i(a) == eval_i(b))),
    'lt':    (2, lambda a, b:    from_bool(eval_i(a) < eval_i(b))),
    'i':     (1, lambda a:       a),
    's':     (3, lambda a, b, c: [[a, c], [b, c]]),
    'neg':   (1, lambda a:       -eval_i(a)),
    't':     (2, lambda a, b:    a),
    'f':     (2, lambda a, b:    b),
    'cons':  (3, lambda a, b, c: [c, a, b]),
    'nil':   (1, lambda a:       "t"),
    'isnil': (1, lambda a:       from_bool(to_list(a) is None)),
    'car':   (1, lambda a:       [a, "t"]),
    'cdr':   (1, lambda a:       [a, "f"]),
    'f38':   (2, f38),
    'interact': (3, lambda protocol, state, vector: ['f38', protocol, [protocol, state, vector]]),
}

def step(a):
    if isinstance(a, list):
        if isinstance(a[0], list):
            return a[0] + a[1:]
        if len(a) == 1:
            return a[0]
        if isinstance(a[0], str) and a[0] in ops:
            argc = ops[a[0]][0]
            op_f = ops[a[0]][1]
            if argc > len(a) - 1:
                raise TooFewArgs(f"Expected {argc} args, got {len(a)-1}")
            return [op_f(*a[1:1+argc])] + a[1+argc:]
        if isinstance(a[0], str) and a[0] in RULES:
            return [RULES[a[0]]] + a[1:]
    elif isinstance(a, str) and a in RULES:
        return RULES[a]
    elif isinstance(a, str):
        return a
    elif isinstance(a, int):
        return a
    raise Exception(f"Unexpected {a}")

LEVEL = 0
def eval(a):
    global LEVEL
    #print(":" + " " * LEVEL + f":\x1b[31mEvaling: {a}\x1b[m")
    LEVEL += 1
    while isinstance(a, list) and not isinstance(a[0], Magic) or isinstance(a, str) and a in RULES:
        #print(":" + " " * LEVEL + ";" + str(a))
        try:
            a = step(a)
        except TooFewArgs:
            break
    LEVEL -= 1
    #print(":" + " " *LEVEL + f":\x1b[31mDone evaling, result {a}\x1b[m")
    return a

CACHE = {}
def eval_cached(x):
    key = str(x)
    if key not in CACHE:
        value = eval(x)
        CACHE[key] = value
        return value
    else:
        return CACHE[key]

def eval_i(a):
    a = eval_cached(a)
    if isinstance(a, int):
        return a
    raise Exception(f"Strange int: {a}")

def eval_list(a):
    a = eval(a)
    if isinstance(a, list) and a[0] == "cons":
        result = []
        while isinstance(a, list) and a[0] == "cons":
            it = eval_list(a[1])
            # print("LIST ITEM:", it)
            result.append(it)
            a = eval(a[2:])
        if not isinstance(a, int) and not a == 'nil':
            raise Exception(f"Not an list nor a cons{a}")
        result.append(a)
        return result
    # print("JUST VALUE:", a)
    return a

def fixup_list(a):
    if a == "nil":
        return []
    if not isinstance(a, list):
        return a
    if a[-1] == "nil":
        return [fixup_list(i) for i in a[:-1]]
    return tuple(fixup_list(i) for i in a)

def from_py(a):
    if isinstance(a, int):
        return a
    if isinstance(a, tuple) and len(a) == 2:
        return ["cons", a[0], a[1]]
    if isinstance(a, list):
        result = "nil"
        for i in reversed(a):
            result = ["cons", from_py(i), result]
        return result
    raise Exception(f"Bad structure {a}")

def interact():
    state = []
    vec = (0, 0)

    while True:
        a = eval(["galaxy", from_py(state), from_py(vec)])

        flag, newState, data = fixup_list(eval_list(a))
        print(flag, newState, data)

        click = Interactor(data).run()
        if click is None:
            exit()

        state = newState
        vec = click

interact()

# a = eval(["interact", "galaxy", "nil", ["cons", 0, 0]])
# print(eval_list(a))

#a = eval(["galaxy", ["cons", 0, "nil"], "nil"])
#print("END: ", a)
