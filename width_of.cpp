#include <iostream>

template <std::size_t n> struct width_of_impl {
    enum { width = 1 + width_of_impl<(n >> 1)>::width };
};

template <> struct width_of_impl<0> {
    enum { width = 0 };
};

template <typename T> struct width_of {
    enum { value = width_of_impl<static_cast<T>(-1)>::width };
};

int main() {
    std::cout << width_of<std::uint8_t>::value << std::endl;
    return 0;
}
