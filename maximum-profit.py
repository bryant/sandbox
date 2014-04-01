def get_maxup(ints):
    low, high, lowest = ints[0], ints[1], ints[0]

    for i in ints[1:]:
        if i > high:
            high = i
        if i - lowest > high - low:
            high = i
            low = lowest
        if i < lowest:
            lowest = i

    return low, high

def get_mins(ints):
    return [min(ints[:i]) for i in xrange(1, len(ints))]

if __name__ == "__main__":
    # test_case = [1,2,3,-1,-2,5,9,22]
    test_case = [1, 0, 0]
    # print get_mins(test_case)
    print get_maxup(test_case)
