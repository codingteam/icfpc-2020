flag = 0
newState = [0, [0, 'nil'], 0, 'nil', 'nil']
data = [
	[[-1, -3], [0, -3], [1, -3], [2, -2], [-2, -1], [-1, -1], [0, -1], [3, -1], [-3, 0], [-1, 0], [1, 0], [3, 0], [-3, 1], [0, 1], [1, 1], [2, 1], [-2, 2], [-1, 3], [0, 3], [1, 3]],
	[[-7, -3], [-8, -2]],
	[]]

def draw(what):
    if len(what) == 0:
        x0 = y0 = x1 = y1 = 0
    else:
        x0 = min(map(lambda a: a[0], what))
        x1 = max(map(lambda a: a[0], what))
        y0 = min(map(lambda a: a[1], what))
        y1 = max(map(lambda a: a[1], what))

    res = [["."] * (x1 - x0 + 1) for y in range(y0, y1+1)]

    for x, y in what:
        res[y - y0][x - x0] = "#"

    print("\n".join(map("".join, res)))
    print()


#draw(data[0])
#draw(data[1])
#draw(data[2])
