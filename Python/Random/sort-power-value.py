import sys

def sort_pow_val(lo: int, high: int, k: int):
    queue = []

    for val in range(lo, high + 1):
        queue.append((get_pow_val(val), val))

    return sorted(queue)[k - 1][1]

def get_pow_val(val: int):
    steps: int = 0
    while val != 1:
        if val % 2 == 0:
            val = val / 2
        else:
            val = 3 * val + 1
        steps += 1
    return steps

print(sort_pow_val(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3])))