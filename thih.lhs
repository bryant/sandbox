2. Preliminaries
================

For simplicity, we present the code for our typechecker as a single Haskell
module. The program uses only a handful of standard prelude functions, like
`map`, `concat`, `all`, `any`, `mapM`, etc., and a few operations from the List
and Monad libraries:

```lhs

> module TypingHaskellInHaskell where
> import List(nub, (\\), intersect, union, partition)
> import Monad(msum)

```

For the most part, our choice of variable names follows the notational
conventions set out in Figure 1.

    Description         Symbol      Type
    kind                k, ...      Kind
    type constructor    tc, ...     Tycon
    type variable       v, ...      Tyvar
    -'fixed'            f, ...
    -'generic'          g, ...
    type                t, ...      Type
    class               c, ...      Class
    instance            it, ...     Inst
    predicate           p, q, ...   Pred
    -'deferred'         d, ...
    -'retained'         r, ...
    qualified type      qt, ...     QualType
    class environment   ce, ...     ClassEnv
    scheme              sc, ...     Scheme
    substitution        s, ...      Subst
    unifier             u, ...      Subst
    assumption          a, ...      Assump
    identifier          i, ...      Id
    literal             l, ...      Literal
    pattern             pat, ...    Pat
    expression          e, f, ...   Expr
    alternative         alt, ...    Alt
    binding group       bg, ...     BindGroup

        Figure 1: Notational Conventions

A trailing `s` on a variable name usually indicates a list. Numeric suffices or
primes are used as further decoration where necessary. For example, we use `k`
or `k'` for a kind, and `ks` or `ks'` for a list of kinds. The types and terms
appearing in the table are described more fully in later sections. To
distinguish the code for the typechecker from program fragments that are used to
discuss its behavior, we typeset the former in an italic font, and the latter in
a typewriter font.

Throughout this paper, we implement identifiers as strings, and assume that
there is a simple way to generate identifiers from integers using the `enumId`
function:

```lhs

> type Id  = String
>
> enumId  :: Int -> Id
> enumId n = "v" ++ show n

```

The `enumId` function will be used in the definition of the `newTVar` operator
in Section 10 to describe the allocation of fresh type variables during type
inference. With the simple implementation shown here, we assume that variable
names beginning with "v" do not appear in input programs.

3. Kinds
========

To ensure that they are valid, Haskell type constructors are classified into
different _kinds_: the kind `*` (pronounced "star") represents the set of all
simple (i.e., nullary) type expressions, like `Int` and `Char -> Bool`; kinds of
the form `k1 -> k2` represent type constructors that take an argument type of
kind `k1` to a result type of kind `k2`. For example, the standard list, Maybe
and IO constructors all have kind `* -> *`. Here, we will represent kinds as
values of the following datatype:

```lhs

> data Kind  = Star | Kfun Kind Kind
>              deriving Eq

```

Kinds play essentially the same role for type constructors as types do for
values, but the kind system is clearly very primitive. There are a number of
extensions that would make interesting topics for future research, including
polymorphic kinds, subkinding, and record/product kinds. A simple extension of
the kind system-adding a new row kind-has already proved to be useful for the
Trex implementation of extensible records in Hugs [Gaster & Jones, 1996, Jones &
Peterson, 1999].
