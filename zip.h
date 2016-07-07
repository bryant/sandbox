#pragma once

#include "llvm/ADT/iterator_range.h"

namespace details {
using llvm::make_range;
using std::get;
using std::tuple_size;
using std::tuple;
using std::input_iterator_tag;

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

template <typename Tup, unsigned... Ns>
auto tup_inc(Tup args, NatList<Ns...>)
    -> decltype(make_tuple(next(get<Ns>(args))...)) {
    return make_tuple(next(get<Ns>(args))...);
}

template <typename... ArgIters> class ZipIter {
    typedef typename BuildNatList<0, tuple_size<tuple<ArgIters...>>::value -
                                         1>::eval nat_list;
    tuple<ArgIters...> iterators;
    template <unsigned... Ns>
    auto deres(NatList<Ns...>) -> tuple<decltype(*get<Ns>(iterators)) &&...> {
        return tuple<decltype(*get<Ns>(iterators))...>(*get<Ns>(iterators)...);
    }

  public:
    ZipIter(ArgIters... ts) : iterators(make_tuple(ts...)) {}

    auto operator*() -> decltype(deres(nat_list{})) {
        return deres(nat_list{});
    }

    void operator++() { iterators = tup_inc(iterators, nat_list{}); }

    bool operator!=(const ZipIter<ArgIters...> &other) const {
        return get<0>(iterators) != get<0>(other.iterators);
    }

    typedef input_iterator_tag iterator_category;
    typedef decltype(*std::declval<ZipIter<ArgIters...>>()) value_type;
};

template <typename... Args> ZipIter<Args...> zip_impl(Args... args) {
    return ZipIter<Args...>(args...);
}
}

template <typename T, typename U, typename... Args>
auto zip(T &&t, U &&u, Args &&... args) -> decltype(
    make_range(details::zip_impl(t.begin(), u.begin(), args.begin()...),
               details::zip_impl(t.end(), u.end(), args.end()...))) {
    return make_range(details::zip_impl(t.begin(), u.begin(), args.begin()...),
                      details::zip_impl(t.end(), u.end(), args.end()...));
}
