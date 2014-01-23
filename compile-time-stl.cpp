#include <map>

//std::map<int, int> f = {{4, 230}};

struct A {
    unsigned b;
    A *next;
};

A a = {4, 0};

int main() {
    return a.b;
}
