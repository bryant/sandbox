/*
-O3-friendly version of python's

for i in xrange(59):
    ...

traditional for (x = ; x < ; ++x) encodes 3 axes of info (init, lt, step) whereas for i in xrange encodes only 2 (init, step); whether the boundary check is dynamically constructed. directly porting xrange gives rise to cases where bounds check can only be determined at run-time, which means slightly slower than c-style fors. so we encode the missing info with choice of incrange or decrange.
*/

#include <cstdio>

using std::printf;

template <typename T>
class incrange {
public:
    class iterator {
        const T delta;
        T v;
    public:
        iterator(T val, T increm): v(val), delta(increm) {}
        T operator* () const { return v; }
        bool operator!= (iterator ot) {
            if (v < ot.v) return true;
            else return false;
        }
        T operator++ () {
            v += delta;
            return v;
        }
    };

    incrange(T start, T end, T delta_): s(start, delta_), e(end, delta_),
        cur(start, delta_) {}
    incrange(T start, T end): incrange(start, end, 1) {}
    incrange(T end): incrange(0, end, 1) {}
    iterator begin() const { return s; }
    iterator end() const { return e; }

private:
    iterator s, e, cur;
};

/* consider wrapping in a function for template deduction:
template <typename T>
Range<T> range(T s, T e) {
    return Range<T>(s, e);
}*/

int main() {
    for (auto i: incrange<int>(59)) {
        printf("%d ", i);
    }
    printf("\n");
    /* asm output above should be equivalent to */
    for (auto i = 0; i < 59; ++i) {
        printf("%d ", i);
    }
    printf("\n");
    return 0;
}
