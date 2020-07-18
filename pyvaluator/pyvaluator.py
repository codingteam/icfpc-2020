#!/usr/bin/env python3

import sys

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
    print("flag = ", flag)
    raise Exception("Done")

    #  f38(protocol, (flag, newState, data)) = if flag == 0
    #                  then (modem(newState), multipledraw(data))
    #                  else interact(protocol, modem(newState), send(data))
    #  interact(protocol, state, vector) = f38(protocol, protocol(state, vector))

ops = {
    'add':   (2, lambda a, b:    eval_i(a) + eval_i(b)),
    'mul':   (2, lambda a, b:    eval_i(a) * eval_i(b)),
    'b':     (3, lambda a, b, c: [a, [b, c]]),
    'c':     (3, lambda a, b, c: [[a, c], b]),
    'div':   (2, lambda a, b:    eval_i(a) // eval_i(b)), # TODO: proper division
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
    'f38':   (2, f38)
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
        if len(a) == 1:
            return a[0]
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

def eval_i(a):
    a = eval(a)
    if isinstance(a, int):
        return a
    raise Exception(f"Strange int: {a}")

def eval_list(a):
    a = eval(a)
    if isinstance(a, list) and a[0] == "cons":
        result = []
        while isinstance(a, list) and a[0] == "cons":
            it = eval_list(a[1])
            print("LIST ITEM:", it)
            result.append(it)
            a = eval(a[2:])
        result.append(a)
        return result
    print("JUST VALUE:", a)
    return a

a = eval(["galaxy", "nil", ["cons", 0, 0]])
print("LIST = ", eval_list(a))

#a = eval(["galaxy", ["cons", 0, "nil"], "nil"])
#print("END: ", a)
