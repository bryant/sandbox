#include <iostream>

template <typename L, typename R>
struct Either {
    Either(L left) : val {left}, parity(false) {}
    Either(R right) : val {right}, parity(true) {}

    static Either<L, R> Left(L left) { return Either(left); }
    static Either<L, R> Right(R right) { return Either(right); }

    friend std::ostream& operator<< (std::ostream &out, const Either &e) {
        if (e.parity == false) { out << "Left(" << e.val.left; }
        else { out << "Right(" << e.val.right; }
        out << ")";
        return out;
    }

    union {
        L left;
        R right;
    } val;
    bool parity;
};

int main() {
    volatile auto e = Either<int, bool>::Left(32);
    return 0;
}
