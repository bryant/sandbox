#include <array>
#include <iostream>
#include <cstddef>

using ui = std::size_t;

template <ui rows, ui cols, typename T>
using matrix = std::array<std::array<T, cols>, rows>;

template <ui m, ui n, ui p, typename T>
matrix<m, p, T> matrix_mult(const matrix<m, n, T> &A,
                            const matrix<n, p, T> &B) {
    matrix<m, p, T> rv = {0};
    for (ui row = 0; row < m; ++row) {
        for (ui col = 0; col < p; ++col) {
            for (ui entry = 0; entry < n; ++entry) {
                rv[row][col] += A[row][entry] * B[entry][col];
            }
        }
    }

    return rv;
}

template <ui r, ui c, typename T>
std::ostream& operator<<(std::ostream &out, const matrix<r, c, T> &m) {
    for (ui row = 0; row < r; ++row) {
        for (ui col = 0; col < c; ++col) {
            out << m[row][col] << ' ';
        }
        out << std::endl;
    }
    return out;
}


int main() {
    using namespace std;
    using mat2x2 = matrix<2, 2, unsigned>;
    mat2x2 p = matrix_mult(mat2x2({3, 2, 1, 4}), mat2x2({4, 5, 6, 7}));
    cout << p << endl ;
    return 0;
}

/*
i32 fib_log_n(nth i32):
    if nth <= 0:
        return 1

    [[i32]] mat_mult(a [[i32]], b [[i32]]):
        i32 cell(row i32, col i32):
            return a[0][col]*b[row][0] + a[1][col]*b[row][1]
        return [[cell(0, 0), cell(0, 1)],
                [cell(1, 0), cell(1, 1)]]

    [[i32]] mat_exp(m [[i32]], k i32):
        # raise a matrix m to the k-th power in O(log k) operations using
        # y exponentiation.
        rv [[i32]] = [[1, 0], [0, 1]]
        while k > 0:
            if k & 1 == 0:
                rv *= m
            m *= m
            k >>= 1
        return rv

    fibmatrix [[i32]] = [[1, 1], [1, 0]]
    return mat_exp(fibmatrix, nth)[0][1]
*/
