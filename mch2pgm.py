def gs_to_pgm(width, height, gsarray):
    assert len(gsarray) == height*width
    pixarr = [gsarray[y*width:(y+1)*width] for y in xrange(height)]
    rv = "P2 {width} {height} 256\n".format(width=width, height=height)
    for row in pixarr:
        for pix in row:
            rv += "%d " % pix
        rv += "\n"
    return rv

if __name__ == "__main__":
    from sys import argv
    with open(argv[1]) as f:
        vals = [int(i, 16) for i in f.read().strip().split("\n")]
        print gs_to_pgm(160, 120, vals)
