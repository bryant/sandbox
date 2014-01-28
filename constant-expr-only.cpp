/*
cpp-11 introduced constexpr:

constexpr unsigned f :: "f might possibly being evaluable at compile-time."

we want a stricter version of this to convince the compiler to completely
inline our calls. we do this with templates.
*/

/* placeholder for a constant expression. */
const int MEDIAN_FILTER_DEPTH = 7;

/* constant expressions only */
template <int a, int b>
int g() {
    return a + b;
}

/* bit more complex */
template <unsigned input>
unsigned isqrt() {
    unsigned base(1 << 15);
    unsigned nextapprox(0);
    unsigned result(0);

    for (; base > 0; base >>= 1) {
        nextapprox = result | base;
        if (nextapprox*nextapprox <= input) {
            result = nextapprox;
        }
    }
    return result;
}

int main(int argc, char **argv) {
    auto k = g<MEDIAN_FILTER_DEPTH, MEDIAN_FILTER_DEPTH + 1>();
    /* bit more complex but g++ inlines it nonetheless. */
    return isqrt<MEDIAN_FILTER_DEPTH>();
}
