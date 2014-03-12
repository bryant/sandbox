#include <iostream>
#include <vector>
#include <typeinfo>

using std::vector;

template <typename T>
const char *print_type() { return __PRETTY_FUNCTION__; }

struct LengthOf {
    template <typename T>
    int operator () (const vector<T> &w) {  /* [w] -> Int */
        std::cout << print_type<vector<T>>() << std::endl;
        return static_cast<int>(w.size());
    }
};

template <typename F, typename U, typename V>
bool same(F f, U u, V v) {  /* Eq t => (forall f. f -> t) -> u -> v -> Bool */
    return f(u) == f(v);
}

int main() {
    std::cout << same(LengthOf(), vector<char> {'n', 'y', 'c'},
                      vector<int> {2, 1}) << std::endl;
    return 0;
}
