#pragma once

#include <algorithm>
#include <array>
#include <tuple>

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

template <typename... Iters> class ZipFirst {
  public:
    typedef typename BuildNatList<0, sizeof...(Iters)-1>::eval nat_list;
    typedef std::input_iterator_tag iterator_category;
    typedef std::tuple<decltype(*std::declval<Iters>())...> value_type;
    std::tuple<Iters...> iterators;

  private:
    template <unsigned... Ns> value_type deres(NatList<Ns...>) {
        return value_type(*std::get<Ns>(iterators)...);
    }

    template <unsigned... Ns> decltype(iterators) tup_inc(NatList<Ns...>) {
        return std::tuple<Iters...>(std::next(std::get<Ns>(iterators))...);
    }

  public:
    value_type operator*() { return deres(nat_list{}); }

    void operator++() { iterators = tup_inc(nat_list{}); }

    bool operator!=(const ZipFirst<Iters...> &other) const {
        return std::get<0>(iterators) != std::get<0>(other.iterators);
    }
    ZipFirst(Iters &&... ts) : iterators(std::forward<Iters>(ts)...) {}
};

template <typename... Iters> class ZipShortest : public ZipFirst<Iters...> {
    template <unsigned... Ns>
    bool test(const ZipFirst<Iters...> &other, NatList<Ns...>) const {
        std::array<bool, sizeof...(Iters)> conds = {(
            std::get<Ns>(this->iterators) != std::get<Ns>(other.iterators))...};
        return std::all_of(conds.begin(), conds.end(),
                           [](bool a) { return a; });
    }

  public:
    bool operator!=(const ZipFirst<Iters...> &other) const {
        return test(other, typename ZipFirst<Iters...>::nat_list{});
    }
    ZipShortest(Iters &&... ts)
        : ZipFirst<Iters...>(std::forward<Iters>(ts)...) {}
};

template <template <typename...> class ItType, typename... Args> class Zippy {
  public:
    typedef ItType<typename std::remove_reference<Args>::type::iterator...>
        iterator;

  private:
    typedef typename BuildNatList<0, sizeof...(Args)-1>::eval nat_list;
    std::tuple<Args...> ts;

    template <unsigned... Ns> iterator begin_impl(NatList<Ns...>) {
        return iterator(std::get<Ns>(ts).begin()...);
    }
    template <unsigned... Ns> iterator end_impl(NatList<Ns...>) {
        return iterator(std::get<Ns>(ts).end()...);
    }

  public:
    iterator begin() { return begin_impl(nat_list{}); }
    iterator end() { return end_impl(nat_list{}); }
    Zippy(Args &&... ts_) : ts(std::forward<Args>(ts_)...) {}
};

template <typename T, typename U, typename... Args>
Zippy<ZipFirst, T, U, Args...> zip(T &&t, U &&u, Args &&... args) {
    return Zippy<ZipFirst, T, U, Args...>(
        std::forward<T>(t), std::forward<U>(u), std::forward<Args>(args)...);
}

template <typename T, typename U, typename... Args>
Zippy<ZipShortest, T, U, Args...> zip_shortest(T &&t, U &&u, Args &&... args) {
    return Zippy<ZipShortest, T, U, Args...>(
        std::forward<T>(t), std::forward<U>(u), std::forward<Args>(args)...);
}
