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
    using Index = typename std::vector<T>::difference_type;
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
        auto spot = 0;
        while (spot <= parent_of(arr.size()-1)) {
            auto lchild = 2*spot + 1;
            auto rchild = lchild + 1;
            auto swapto = spot;

            if (arr[lchild] < elem) {
                swapto = lchild;
            }
            if (rchild < arr.size() && arr[rchild] < arr[swapto]) {
                swapto = rchild;
            }
            if (swapto != spot) {
                arr[spot] = std::move(arr[swapto]);
                spot = swapto;
            }
            else { break; }
        }
        arr[spot] = std::move(elem);
    }

    /* reminder: not a const member method because rets nonconst iter */
    Iter parent_of(Iter child) {
        return arr.begin() + (child-arr.begin()-1)/2;
    }

    Index parent_of(Index child) {
        if (child <= 0) return -1;
        return (child - 1) / 2;
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

    Heap<int> j;
    j.push_back(1);
    j.swap_top(4);
    std::cout << j << std::endl;
    return 0;
}
