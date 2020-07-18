with open("../data/galaxy.txt") as galaxy:
    data = galaxy.read()

def ap(a):
    def ap_(b):
        return a(b)
    return ap_

def t(a):
    def t_(b):
        return a
    return t_

def f(a):
    def f_(b):
        return b
    return f_

def eq(a):
    def eq_(b):
        if a == b:
            return t
        else:
            return f
    return eq_

def add(a):
    def add_(b):
        return a + b
    return add_

def mul(a):
    def mul_(b):
        return a * b
    return mul_

def i(a):
    return a

def nil(a):
    return t

def cons(a):
    def cons_(b):
        if type(a) is list:
            a_local = a
        elif callable(a) and a.__name__ is "nil":
            a_local = []
        else:
            a_local = [a]
        if type(b) is list:
            b_local = b
        elif callable(b) and b.__name__ is "nil":
            b_local = []
        else:
            b_local = [b]
        return a_local + b_local
    return cons_

def car(a):
    return a[0]

def cdr(a):
    return a[-1]

def isnil(a):
    if callable(a) and a.__name__ is "nil":
        return t
    else:
        return f

def lt(a):
    def lt_(b):
        if a < b:
            return t
        else:
            return f
    return lt_

def neg(a):
    return -a

import math
def div(a):
    def div_(b):
        result = a / b
        if result >= 0:
            return math.floor(result)
        else:
            return math.ceil(result)
    return div_

def s(a):
    def s_(b):
        def s__(c):
            return ap(ap(a)(c))(ap(b)(c))
        return s__
    return s_

def c(a):
    def c_(b):
        def c__(x):
            return ap(ap(a)(x))(b)
        return c__
    return c_

def b(a):
    def b_(b):
        def b__(c):
            return ap(a)(ap(b)(c))
        return b__
    return b_

def inc(a):
    return a+1

def dec(a):
    return a-1

operations = {
    "inc": inc,
    "add": add,
    "mul": mul,
    "neg": neg,
    "div": div,
    "eq": eq,
    "cons": cons,
    "nil": nil,
    "isnil": isnil,
    "lt": lt,
    "car": car,
    "cdr": cdr,
    "s": s,
    "c": c,
    "dec": dec,
    "b": b,
    "t": t,
    "f": f,
    "i": i
}

variables = {}

def eval_expr(string):
    stack = []
    for token in reversed(string.split(" ")):
        if token == "ap":
            a = stack.pop(0)
            b = stack.pop(0)
            stack.insert(0, a(b))
        else:
            mapped = operations.get(token)
            if mapped is None:
                if token[0] is ":":
                    mapped = variables[token]
                else:
                    mapped = int(token)
            stack.insert(0, mapped)
    assert len(stack) == 1
    return stack[0]


from graphviz import *
import re

def draw_graph():
    dot = Graph()
    dot.attr("graph", {"size": "30", "dpi": "600"})
    for statement in data.split("\n"):
        variable, expression = statement.split(" = ")
        variable = variable.replace("galaxy", ":galaxy")

        dependencies = set(re.findall(":\w+", expression))

        attributes = {}
        if variable[1:] == "galaxy":
            attributes = {"fillcolor": "red", "width": "5", "height": "5", "style": "filled"}
        elif len(dependencies) == 0:
            attributes = {"fillcolor": "yellow", "width": "1", "height": "1", "style": "filled"}
        else:
            attributes = {"width": str(1 + math.log2(len(dependencies))),
                          "height": str(1 + math.log2(len(dependencies)))}
        dot.node(variable[1:], _attributes=attributes)
        for dependency in dependencies:
            dot.edge(dependency[1:], variable[1:])

    dot.render("galaxy.gv", view=True, format="png")

# dependencies = {}
# records = {}
# for statement in data.split("\n"):
#     variable, expression = statement.split(" = ")
#
#     dependencies[variable] = set(re.findall(":\w+", expression))
#
#     if variable in dependencies[variable]:
#         expression.replace(variable, "-99999999")
#         dependencies[variable].remove(variable)
#
#     records[variable] = expression
#
# def resolve_and_evaluate(variable):
#     global_dependencies_list = []
#
#     registered_dependencies = {variable}
#     dependencies_to_resolve = [variable]
#     while len(dependencies_to_resolve) != 0:
#         print(global_dependencies_list)
#         dependencies_of_dependencies = set()
#         for to_resolve in dependencies_to_resolve:
#             for dependency in dependencies[to_resolve]:
#                 if dependency not in variables and dependency not in registered_dependencies:
#                     dependencies_of_dependencies.add(dependency)
#
#         ordered_dependencies_of_dependencies = []
#         while len(dependencies_of_dependencies) != 0:
#             for dependency in list(dependencies_of_dependencies):
#                 if len(dependencies[dependency].intersection(dependencies_of_dependencies)) == 0:
#                     ordered_dependencies_of_dependencies.append(dependency)
#                     dependencies_of_dependencies.remove(dependency)
#
#         dependencies_to_resolve.clear()
#         for dependency in reversed(ordered_dependencies_of_dependencies):
#             if dependency not in registered_dependencies:
#                 registered_dependencies.add(dependency)
#                 dependencies_to_resolve.append(dependency)
#                 global_dependencies_list.append(dependency)
#
#     for dependency in reversed(global_dependencies_list):
#         print("Evaluating", dependency)
#         variables[dependency] = eval_expr(records[dependency])
#
#     return eval_expr(records[variable])
#
# import random
# def resolve_and_evaluate2(request):
#     to_evaluate = list(records.keys())
#     while request in to_evaluate:
#         random.shuffle(to_evaluate)
#         print(len(to_evaluate), to_evaluate)
#         for variable in to_evaluate:
#             try:
#                 variables[variable] = eval_expr(records[variable])
#                 to_evaluate.remove(variable)
#             except KeyError as e:
#                 pass
#     return variables[request]
#
# print(resolve_and_evaluate2(":galaxy"))