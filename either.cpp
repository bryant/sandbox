#include <iostream>
#include <type_traits>

using std::enable_if;
using std::is_same;

template <typename L, typename R> struct Either {
    union _EitherVal {
        _EitherVal(L left) : left(left) {}
        _EitherVal(R right) : right(right) {}

        L left;
        R right;
    };

    Either(L left) : val(left), parity(false) {}
    Either(R right) : val(right), parity(true) {}

    static Either<L, R> Left(L left) { return Either(left); }
    static Either<L, R> Right(R right) { return Either(right); }

    friend std::ostream &operator<<(std::ostream &out, const Either &e) {
        if (e.parity == false) {
            out << "Left(" << e.val.left;
        } else {
            out << "Right(" << e.val.right;
        }
        out << ")";
        return out;
    }

    _EitherVal val;
    bool parity;
};

template <typename T, typename U> struct enable_if_same {
    using type = typename enable_if<is_same<T, U>::value, U>::type;
};

template <typename L, typename R, typename Functor0, typename Functor1>
auto do_either(Either<L, R> &e, Functor0 when_left, Functor1 when_right)
    -> typename enable_if_same<decltype(when_left(e.val.left)),
                               decltype(when_right(e.val.right))>::type {
    if (e.parity) {
        return when_right(e.val.right);
    } else {
        return when_left(e.val.left);
    }
}

int main() {
    auto e = Either<int, bool>::Left(32);
    do_either(e, [](int j) { std::cout << "left!" << j << std::endl; },
              [](bool c) { std::cout << "right!" << c << std::endl; });
    std::cout << e << std::endl;
    return 0;
}
