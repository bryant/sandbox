/*
vector-based heap. mainly for that juggler thing.

two choices for the swap_top op:
> swap_top (1) + sift_down (lg n)
> pop_heap (lg n) + push_back (1) + push_heap (lg n)

first choice is faster since push_heap == sift_up which has a higher bound than
sift_down since heap is already heap.
*/

#include <vector>
#include <algorithm>
#include <iostream>

template <typename T>
class Heap {
private:
    std::vector<T> arr;
    using Iter = typename std::vector<T>::iterator;
    using Index = typename std::vector<T>::size_type;
public:
    Heap() {}
    void push_back(T elem) {
        /* ad hoc construction to extend the rear by 1; if the empty constructor
         * were expensive...
         */
        arr.emplace_back();

        auto pos = arr.end() - 1;
        while (pos > arr.begin()) {
            auto par = parent_of(pos);
            if (elem < *par) { std::move(par, par+1, pos); }
            else { break; }
            pos = par;
        }
        *pos = elem;
    }

    /* uses sift-down to swap top element
     * equivalent to but faster than pop() >>= insert() (n vs. nlogn)
     */
    void swap_top(T elem) {
        T rv = arr[0];
        auto spot = arr.begin();
        while (spot <= parent_of(arr.end()-1)) {
            auto lchild = arr.begin() + 2*(spot-arr.begin()) + 1;
            auto rchild = lchild + 1;
            Iter swapto = static_cast<Iter>(&elem);

            if (*lchild < elem) {
                swapto = lchild;
            }
            if (rchild < arr.end() && *rchild < *swapto) {
                swapto = rchild;
            }
            if (swapto != static_cast<Iter>(&elem)) {
                std::move(swapto, swapto+1, spot);
                spot = swapto;
            }
            else { break; }
        }
        *spot = elem;
    }

    /* reminder: not a const member method because rets nonconst iter */
    Iter parent_of(Iter child) {
        if (child > arr.begin()) {
            return arr.begin() + (child-arr.begin()-1)/2;
        }
        return arr.begin();
    }

    friend std::ostream& operator<< (std::ostream &out, const Heap &h) {
        for (auto k: h.arr) {
            out << k << ", ";
        }
        return out;
    }
};

int main() {
    Heap<int> h;

    for (auto k: std::vector<int>{3,1,4,1,5,9,2,6,5,3,5,8,9}) {
        h.push_back(k);
    }
    std::cout << h << std::endl;
    for (auto k: std::vector<int>{24,12,36,3}) {
        h.swap_top(k);
    }
    std::cout << h << std::endl;
    return 0;
}
