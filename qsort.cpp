#include <vector>
#include <algorithm>
#include <iostream>
#include <ctime>

template <typename T>
using vecidx = typename std::vector<T>::size_type;

template <typename T>
using vec = std::vector<T>;

template <typename T>
using vecptr = typename std::vector<T>::iterator;

template <typename T>
void quicksort(T l, T r) {
    /* sort elements in [l, r) */
    auto rmost = r - 1;
    if (l < rmost) {
        //std::iter_swap(l, rmost);  /* hardcoded choice of pivot @ l */

        T pivpos = l;

        for (T i = l; i < rmost; i += 1) {
            if (*i <= *rmost) {
                std::iter_swap(i, pivpos);
                pivpos += 1;
            }
        }

        std::iter_swap(pivpos, rmost);
        quicksort(l, pivpos);
        quicksort(pivpos+1, r);
    }
}

int main() {
    using namespace std;

    vec<int> d = {3, 1, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9};
    volatile clock_t k = clock();
    quicksort(d.begin(), d.end());
    k = clock() - k;
    cout << k << endl;
    for (auto &i : d) cout << i << " ";

    vec<int> e = {3, 1, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9};
    k = clock();
    sort(e.begin(), e.end());
    k = clock() - k;
    cout << k << endl;
    for (auto &i : e) cout << i << " ";

    return 0;
}
