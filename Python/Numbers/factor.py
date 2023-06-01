import sys
import time
from math import sqrt

def factor(num: int):
    factors = []

    mid = int(sqrt(num))
    r = range(3, mid, 2) if num % 2 != 0 else range(2, mid)

    for i in r:
        if num % i == 0:
            factors.append(i)
            if i != num / i:
                factors.append(int(num / i))

    return factors

if __name__ == '__main__':
    s = time.perf_counter()
    print(factor(int(sys.argv[1])))
    e = time.perf_counter()
    print(round(e - s, 3))