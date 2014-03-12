#include <iostream>
#include <vector>
#include <functional>

using std::vector; using std::function;

struct S {
    template <typename T>
    int operator () (const T &arr) {
        std::cout << "hey! " << arr.size() << std::endl;
        return static_cast<int>(arr.size());
    }
};

template <typename F, typename U, typename V>
bool check(F f, U u, V v) {
    return f(u) == f(v);
}

int main() {
    //check(get_length, vector<char> {'n', 'y', 'c'}, vector<int> {2, 1, 2});
    check(S(), vector<char> {'n', 'y', 'c'}, vector<int> {2, 1});
    return 0;
}
