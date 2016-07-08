#pragma once

#include "llvm/ADT/iterator_range.h"
#include <algorithm>
#include <array>
#include <tuple>

namespace details {
using llvm::make_range;
using std::get;
using std::forward;

// NatList :: [Nat]
template <unsigned... Ns> struct NatList {};

template <typename L, typename R> struct Cons {};

template <unsigned n, unsigned... m> struct Cons<NatList<n>, NatList<m...>> {
    using eval = NatList<n, m...>;
};

template <unsigned n, typename T> struct Cons<NatList<n>, T> {
    using eval = Cons<NatList<n>, typename T::eval>;
};

template <unsigned n, unsigned max> struct BuildNatList {
    using eval = typename Cons<NatList<n>,
                               typename BuildNatList<n + 1, max>::eval>::eval;
};

template <unsigned max> struct BuildNatList<max, max> {
    using eval = NatList<max>;
};

template <typename... Iters> struct ZipFirst {
    typedef typename BuildNatList<0, sizeof...(Iters)-1>::eval nat_list;
    typedef std::input_iterator_tag iterator_category;
    typedef std::tuple<decltype(*std::declval<Iters>())...> value_type;

    std::tuple<Iters...> iterators;

    template <unsigned... Ns> value_type deres(NatList<Ns...>) {
        return value_type(*get<Ns>(iterators)...);
    }
    value_type operator*() { return deres(nat_list{}); }

    template <unsigned... Ns> decltype(iterators) tup_inc(NatList<Ns...>) {
        return std::tuple<Iters...>(std::next(get<Ns>(iterators))...);
    }
    void operator++() { iterators = tup_inc(nat_list{}); }

    bool operator!=(const ZipFirst<Iters...> &other) const {
        return get<0>(iterators) != get<0>(other.iterators);
    }
    ZipFirst(Iters &&... ts) : iterators(forward<Iters>(ts)...) {}
};

template <typename... Iters> struct ZipShortest : public ZipFirst<Iters...> {
    template <unsigned... Ns>
    bool test(const ZipFirst<Iters...> &other, NatList<Ns...>) const {
        std::array<bool, sizeof...(Iters)> conds = {
            (get<Ns>(this->iterators) != get<Ns>(other.iterators))...};
        return std::all_of(conds.begin(), conds.end(),
                           [](bool a) { return a; });
    }
    bool operator!=(const ZipFirst<Iters...> &other) const {
        return test(other, typename ZipFirst<Iters...>::nat_list{});
    }
    ZipShortest(Iters &&... ts) : ZipFirst<Iters...>(forward<Iters>(ts)...) {}
};

template <template <typename...> class ItType, typename... Args> struct Zippy {
    typedef typename BuildNatList<0, sizeof...(Args)-1>::eval nat_list;
    typedef ItType<typename std::remove_reference<Args>::type::iterator...>
        iterator;

    std::tuple<Args...> ts;

    template <unsigned... Ns> iterator begin_impl(NatList<Ns...>) {
        return iterator(get<Ns>(ts).begin()...);
    }
    template <unsigned... Ns> iterator end_impl(NatList<Ns...>) {
        return iterator(get<Ns>(ts).end()...);
    }
    iterator begin() { return begin_impl(nat_list{}); }
    iterator end() { return end_impl(nat_list{}); }
};
}

template <typename T, typename U, typename... Args>
details::Zippy<details::ZipFirst, T, U, Args...> zip(T &&t, U &&u,
                                                     Args &&... args) {
    using namespace details;
    return Zippy<ZipFirst, T, U, Args...>{std::tuple<T, U, Args...>(
        forward<T>(t), forward<U>(u), forward<Args>(args)...)};
}

template <typename T, typename U, typename... Args>
details::Zippy<details::ZipShortest, T, U, Args...>
zip_shortest(T &&t, U &&u, Args &&... args) {
    using namespace details;
    return Zippy<ZipShortest, T, U, Args...>{std::tuple<T, U, Args...>(
        forward<T>(t), forward<U>(u), forward<Args>(args)...)};
}
