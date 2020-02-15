---
title: "[ANN] distributed-dataset: A distributed data processing framework in pure Haskell"
date: 2020-02-02
draft: true
---

If we want to process large amount of data, using a single computer
becomes cumbersome when we start to push the limits of the disk or
the CPU. Using a cluster of computers is the most common solution
to this problem; but it comes with significant overhead, since a
distributed application usually is much more complex than a traditional
single-threaded process for the same task.

[distributed-dataset][] is a Haskell library trying overcome this
problem. It hides the complexity of coordinating those machines behind a
high-level list-like API, so that data transformations can be expressed
almost as easily as the traditional programming model.

The library is based around the `Dataset` type, which describes a
chain of transformations to be applied on top a partitioned data
source. Those transformations are applied in a distributed fashion
where the computations are shipped to the executor nodes, and each one
of those executors work on a subset of the data.

The transformations are higher-order functions taking ordinary
Haskell functions. It is achieved by using the [StaticPointers][] GHC
extension and the [distributed-closure][] library. Thanks to these,
distributed-dataset is able to leverage the existing Haskell ecosystem,
both when defining transformations and reading and writing the inputs
and outputs.

#### Example

I will walk through a small example to give you a taste how how the API looks
like. Code below downloads all public GitHub events from [GH Archive] on
2019, and ranks the user based on the number of commits they used the word
`cabal`. Here it is:

```{ .haskell .numberLines }
app :: DD ()
app =
  ghArchive (fromGregorian 2019 1 1, fromGregorian 2019 12 31)
    & dConcatMap (static (\e ->
        let author = e ^. gheActor . ghaLogin
            commits = e ^.. gheType . _GHPushEvent
                        . ghpepCommits . traverse . ghcMessage
        in  map (author, ) commits
      ))
    & dFilter (static (\(_, commit) ->
        "cabal" `T.isInfixOf` T.toLower commit
      ))
    & dGroupedAggr 50 (static fst) dCount
    & dAggr (dTopK (static Dict) 20 (static snd))
    >>= mapM_ (liftIO . print)
```

Let's go through it step by step:

---

``` { .haskell .numberLines startFrom=3 }
ghArchive (fromGregorian 2019 1 1, fromGregorian 2019 12 31)
```

This function is provided by a separate library called
`distributed-dataset-opendataset`, which returns a `Dataset GHEvent`
with public GitHub events within given dates. Internally, each partition
in returned `Dataset` downloads, decompresses and parses 4 hours worth of
data. This means that `distributed-dataset` will spawn 2190 (365*24/4)
executors to process this `Dataset` by default.

---

``` { .haskell .numberLines startFrom=4 }
  & dConcatMap (static (\e ->
      let author = e ^. gheActor . ghaLogin
          commits = e ^.. gheType . _GHPushEvent
                      . ghpepCommits . traverse . ghcMessage
      in  map (author, ) commits
    ))
  & dFilter (static (\(_, commit) ->
      "cabal" `T.isInfixOf` T.toLower commit
    ))
```

Other than using some usual Haskell code to extract commits from a
`GHEvent`; we are using two combinators provided by `distributed-dataset`:

```haskell
dConcatMap :: (StaticSerialise a, StaticSerialise b)
           => Closure (a -> [b]) -> Dataset a -> Dataset b
dFilter :: StaticSerialise a
        => Closure (a -> Bool) -> Dataset a -> Dataset a
```

As you can see, they are quite similar to `concatMap` and `filter` functions
in `Data.List`, and they work as you expect. There are two main differences:

* We have `StaticSerialize` constraints on input and output values since
they will possibly be transferred across executors.
* Functions we pass to the combinators are wrapped in `Closure`'s using
the `static` keyword. This keyword has some constraints about what kind
of values it can wrap, but usually those limitations can be overcomed
using the combinators in `distributed-closure` library.

Another thing to point out is that `Dataset` is a delayed representation
of the transformations; so they are only evaluated when the final result
is requested, also they are fused with each other when possible.

---

```haskell
  & dGroupedAggr 50 (static fst) dCount
  & dAggr (dTopK (static Dict) 20 (static snd))
```

When a transformation requires combining information from multiple input
rows that are possibly on different partitions, it is called "wide"
(as opposed to "narrow"). They are represented as the `Aggr` data type
defined in [Control.Distributed.Dataset.Aggr][] module, alongside with
functions to create and compose them.

In this snippet; using the `dGroupedAggr` function we first apply
`dCount` aggregation to every row where the `fst` is the same; meaning
we are counting the number of commits per author. After that, we use
`dAggr` function to apply `dTopK` aggregation to find the top 20 rows
according to `snd` over the whole dataset.

---

Since `dAggr` function aggregates the whole value, it directly returns
the result. So we can just print the value in the end:

```haskell
  >>= mapM_ (liftIO . print)
```

This example will download around 300 GB of compressed data, extract
them to almost a terabyte worth of JSON, parse and return them. Using
`distributed-dataset-aws`, the whole process take less than two minutes.

---

```haskell
aggrCollect :: StaticSerialise a => Aggr a [a]
aggrSum :: StaticSerialise a => Closure (Dict (Num a)) -> Aggr a a
aggrMax :: StaticSerialise a => Closure (Dict (Ord a)) -> Aggr a (Maybe a)
```

Some transformations are "wide", which means in order to calculate one
output row, information from multiple input rows are required. These
transformations are a bit harder than "narrow" ones,

A small set of hightake -level transformations are also provided:

```haskell
dMap :: (StaticSerialise a, StaticSerialise b)
     => Closure (a -> b) -> Dataset a -> Dataset b
```

With the StaticPointers extension
of GHC Haskell, we are able to distribute a computation across different
machines; and using the technique described by Matei et al[2] that led
to Apache Spark, we can express and execute large scale data transforms
using a pretty DSL.

[distributed-dataset]: https://github.com/utdemir/distributed-dataset
[Control.Distributed.Dataset.Aggr]: https://utdemir.github.io/distributed-dataset/distributed-dataset/Control-Distributed-Dataset-Aggr.html
[StaticPointers]: https://downloads.haskell.org/~ghc/8.8.2/docs/html/users_guide/glasgow_exts.html#static-pointers
[distributed-closure]: https://hackage.haskell.org/package/distributed-closure
[GH Archive]: https://www.gharchive.org/
