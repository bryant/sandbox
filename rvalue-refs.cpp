#include <iostream>
#include <cstring>
#include <typeinfo>

class Moar {
    unsigned *inc;
    unsigned l;
public:
    Moar(unsigned u): l(u) {
        inc = new unsigned[u]();
    }
    Moar(const Moar &dropbox): l(dropbox.l) {
        std::memcpy(inc, dropbox.inc, l);
    }
    ~Moar() {
        delete [] inc;
    }
};

void f(const Moar &&elephant) { std::cout << "f(const &&)" << std::endl; }

template <typename T>
void print_type(T &&x) {
    std::cout << typeid(x).name() << std::endl;
}

template <typename T>
void f(T &&elephant) { std::cout << "f(&&)" << std::endl; }

int main() {
    Moar k(23);
    Moar m(Moar(44));
    print_type(k);
    print_type(Moar(44));
    return 0;
}
