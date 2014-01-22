def kmp_search(s, w):
    def build_table(needle):
        t = [-1, 0] + [0]*(len(needle)-2)
        cnd = 0
        for pos in xrange(2, len(needle)):
            if needle[pos-1] == needle[cnd]:
                cnd += 1
                t[pos] = cnd
            elif cnd > 0:
                cnd = t[cnd]
            else:
                t[pos] = 0
        return t

    m, i = 0, 0
    t = build_table(w)

    while m + i < len(s):
        if w[i] == s[m+i]:
            if i == len(w) - 1:
                return m
            i += 1
        else:
            m += i - t[i]
            if t[i] > -1:
                i = t[i]
            else:
                i = 0
    return None

kmp_search("bananabananabananas", "asdfgasdfgasdfeasdf")
