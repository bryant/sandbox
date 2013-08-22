#!/usr/bin/env python

from math import pi, cos, sin

FLOWERS = (
    (lambda t: (cos(t)*(sin(6*t)+1), sin(t)*(sin(6*t)+1)), 'k-'),
    (lambda t: (cos(t)*(sin(6*t)+3), sin(t)*(sin(6*t)+3)), 'ro'),
    (lambda t: (cos(t)*(sin(6*t)+5), sin(t)*(sin(6*t)+5)), 'g.'),
    (lambda t: (cos(t)*(sin(6*t)+7), sin(t)*(sin(6*t)+7)), 'b+'),
)

def xy(func):
    def t(s, e, step):
        while s < e:
            yield s
            s += step
    return zip(*map(func, t(0, 2*pi+0.005, 0.05)))

if __name__ == "__main__":
    from matplotlib import pyplot as p
    for func, style in FLOWERS:
        x, y = xy(func)
        p.plot(x, y, style)
    p.show()
