/* using cpp for embedded:
 * - inline operator new or don't use it
 * - g++ -std=c++11 -O3 -ggdb -static-libgcc -static-libstdc++
 */

#include <memory>

template <typename T>
using uptr = std::unique_ptr<T>;

//const uptr<int> P(34);
const int p = 34;

int main() {
    uptr<int> k(new int(34));
    return *k;
}
