import numpy as np
from numpy import random as nr
import math
import time
import showgraph


def brownian(size):
    h = 2**size
    j_max = 1

    # Generate timesteps
    #t = np.array(range(h + 1))
    t = np.linspace(0, 0.5, h + 1)
    # Generate length random numbers (Z_1, Z_{len}) ~ N(0,1)
    Z = nr.standard_normal(size=h + 1)
    print(Z)
    # Initialize output vector
    #w = [0]*(h + 1)
    w = np.zeros(h+1)
    w[h] = math.sqrt(t[h]) * Z[h]

    for k in range(1, size + 1):
        i_min = round(h/2)
        i = i_min

        l = 0
        r = h

        for j in range(1, j_max):
            a = ((t[r] - t[i]) * w[l] + (t[i] - t[l]) * w[r]) / (t[r]-t[l]);
            b = math.sqrt((t[i]*t[l]) * (t[r] - t[i]) / (t[r] - t[l]))
            w[i] = a + b*Z[i]
            i = i + h
            l = l + h
            r = r + h
        j_max *= 2
        h = i_min
    return (w[1:-1], Z)

def main():
    (res, Z) = brownian(8)
    print(res)
    print(np.mean(res))

    np.save("gen-" + time.ctime().replace(" ", "-"), res)
    showgraph.showplot(res)

if __name__ == "__main__":
    main()
