---
title: "[ANN] distributed-dataset: A distributed data processing framework in Haskell"
date: 2020-03-03
draft: true
---

When processing large amounts of data, using a single computer becomes cumbersome when we start to push the limits of our network bandwidth, disk space or the processing power. Using a cluster of computers is the most common solution to this problem; but it comes with significant overhead, since a distributed application usually is much more complex than a traditional single-threaded process for the same task.

[distributed-dataset][] is a Haskell library trying to overcome this problem. It hides the complexity of coordinating those machines behind a high-level list-like API, so the data transformations can be expressed almost as easily as the traditional programming model. It is inspired by [Apache Spark][].

The library is based around the `Dataset` type, which is [internally implemented](https://github.com/utdemir/distributed-dataset/blob/28addb6d8997afb35edd7fa181fec3f19227ce3f/distributed-dataset/src/Control/Distributed/Dataset/Internal/Dataset.hs#L74-L92) as a chain of transformations applied on top a partitioned data source. Those transformations are applied in a distributed fashion where the computations are shipped to the executor nodes, and each one of those executors work only on a subset of the data in parallel.

The transformations are defined using ordinary Haskell functions. This is achieved by the [StaticPointers][] GHC extension and the [distributed-closure][] library. Thanks to those, `distributed-dataset` can leverage the existing Haskell ecosystem, both when defining transformations and reading and writing the inputs and outputs. More specifically, it usually uses `Conduit`'s on the low-level interface; so integrating a new data source or a new kind of transformation is usually as easy as writing a `Conduit` for the purpose.

`distributed-dataset` uses pluggable [Backend][]'s for actually running the computations. Backends are easy to implement; but currently the only existing backend is `distributed-dataset-aws`. This backend uses [AWS Lambda][] to run the computations and S3 to store the intermediate results. The main advantage is that it can rapidly spin up thousands of containers to run the transformations in parallel without requiring any pre-existing infrastructure.

# Example

Before we dig into the details, let's walk through a small example. Code below downloads all public GitHub events from 2020 using [GH Archive][], and ranks the users based on the number of commits they pushed which contains the word "cabal" in the message. The entire code can be found (and built & ran) in the `examples/` subdirectory on the repository, but the relevant function is pasted below:

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

As seen here; the code, I believe, is quite readable and concise; while not using the fancier features of Haskell.

Let's go through it step by step:

---

``` { .haskell .numberLines startFrom=3 }
ghArchive (fromGregorian 2019 1 1, fromGregorian 2019 12 31)
```

This function is provided by a separate library called `distributed-dataset-opendatasets`. It returns a `Dataset GHEvent` with public GitHub events within given dates. Internally, each partition in the returned `Dataset` downloads, decompresses and parses 4 hours worth of data. This means that `distributed-dataset` will spawn 2190 (365*24/4) executors to process this `Dataset` by default.

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

Here; we are using two combinators from `distributed-dataset`:

```haskell
dConcatMap :: (StaticSerialise a, StaticSerialise b)
           => Closure (a -> [b]) -> Dataset a -> Dataset b
dFilter :: StaticSerialise a
        => Closure (a -> Bool) -> Dataset a -> Dataset a
```

As you can see, they are quite similar to `concatMap` and `filter` functions in `Data.List`, and they work as you expect. There are two main differences:

* We have `StaticSerialize` constraints on input and output values since they will possibly be transferred across executors.
* Functions we pass to the combinators are wrapped in `Closure`'s using the `static` keyword. This keyword has some constraints about what kind of values it can wrap, but usually those limitations can be overcome using the combinators in the `distributed-closure` library.

Another thing to point out is that `Dataset` is a delayed representation of the transformations; so they are only evaluated when the final result is requested, also they are fused when possible. In this example, the two calls to `dConcatMap` and `dFilter` will only do a single pass over the input dataset.

---

``` { .haskell .numberLines startFrom=13 }
  & dGroupedAggr 50 (static fst) dCount
  & dAggr (dTopK (static Dict) 20 (static snd))
```

Transformations requiring information from multiple input rows that are possibly on different partitions, are called "wide" (as opposed to "narrow"). This means that the data will be serialized and "shuffled" across the network. In `distributed-dataset`, this is usually done with the `Aggr` data type, which provides some utilities around creating and composing these kind of transformations.

In this snippet; using the `dGroupedAggr` function we first apply `dCount` aggregation to every row where the `fst` is the same; meaning we are counting the number of commits per author. After that, we use `dAggr` function to apply `dTopK` aggregation to find the top 20 rows according to the commit count, over the whole dataset.

---

Finally, since `dAggr` function aggregates the entire `Dataset`, it directly returns the result of an aggregation as an ordinary Haskell value. So we can just print the value in the end:

``` { .haskell .numberLines startFrom=15 }
  >>= mapM_ (liftIO . print)
```

This example will download around 300 GB of compressed data, extract them to almost a terabyte worth of JSON, parse, aggregate and transfer the result back to us. Using `distributed-dataset-aws`, the whole process will take less than two minutes, including the time to provision and destroy the required cloud resources. Here's the result for the curious:

<div style="height: 10em; overflow-y: scroll; border: solid black 1px;">
```
("bgamari",25334)
("ghc-mirror-bot",897)
("aarongable",639)
("peti",634)
("haskell-pushbot",590)
("pull[bot]",539)
("phadej",311)
("fendor",268)
("iohk-bors[bot]",262)
("mergify[bot]",191)
("Ericson2314",190)
("rcaballeromx",184)
("jneira",177)
("vadimeisenbergibm",144)
("aherrmann",127)
("felixonmars-bot",121)
("alanz",111)
("diyessi",107)
("newhoggy",105)
("vdemeester",104)
```
</div>

# Implementation

As mentioned before, a `Dataset` is a _distributed_ multiset, where the actual rows live in separate _partitions_ which can be fetched and processed in parallel. For simpler transformations this can be done easily; `map`'s and `filter`'s can be applied to each partition independently. An n-fold speedup can be achieved easily in this case by processing each partition in parallel on separate executors.

However, it requires a bit more effort when a single output row requires information from multiple input rows. Imagine a `distinct` transformation which removes duplicate rows from a `Dataset`. We can simply remove duplicates within a partition, however there can still be duplicates spread across different partitions; so we can not complete this task by processing each partition on its own. In this case, we need another kind of operation where the rows are _shuffled_ across partitions so that the same rows always end up in the same partition. This can also be done in parallel; we can pass each row through an hash function and determine the target partition using `hash(row) / partition_count` formula. With this functionality, it is possible to implement the `distinct` combinator by:

1. Removing duplicates within each partition.
2. Shuffling the rows so the same rows end up in the same partition.
3. Removing duplicates within each partition again.

Turns out those two operations are enough to implement the `Dataset` type. So, below is a slightly simplified excerpt from the library:

``` { .haskell }
newtype Partition a
  = Partition (Closure (ConduitT () a (ResourceT IO) ()))

data Dataset a where
  DExternal
    :: StaticSerialise a
    => [Partition a]
    -> Dataset a
  DPipe
    :: StaticSerialise b
    => Closure (ConduitT a b (ResourceT IO) ())
    -> Dataset a
    -> Dataset b
  DPartition
    :: StaticHashable k
    => Closure (a -> k)
    -> Dataset a
    -> Dataset a
```

To clarify:

* A `Partition` is a set of rows that can be streamed independently.
* `Dataset`'s can be created from a list of `Partition`'s using the `DExternal` constructor.
* `DPipe` constructor takes a `Conduit`; and converts a `Dataset a` to a `Dataset b` by passing every `Partition` through the given `Conduit` in parallel.
* `DPartition` constructor takes a key function and shuffles the data across `Partition`'s so that all rows sharing the same key ends up in the same `Partition`.

Almost everything else in the library is implemented in terms of above data types. `distributed-dataset` implements a bunch of well-known combinators like `map` and `filter`; and most significantly it adds a data type called `Aggr` implementing composable map-reduce style aggregations, see [Control.Distributed.Dataset.Aggr][] module for more information on this.

## Control.Distributed.Fork

As mentioned before, `distributed-dataset` uses pluggable `Backend`'s to actually run the tasks. Implementing a `Backend` is quite simple, only a single function with signature `ByteString -> IO ByteString` is enough; which should spawn the current program executable(`argv[0]`) in some environment, pass the given `ByteString` as standard input and return the standard output. This provides us the ability to use arbitrary Haskell functions in our transformations, but comes with the requirement of using the exact same binary on the executors. `distributed-dataset-aws` requires the executable is compiled statically, and handles shipping the binary to the executors.

The library also exposes `Control.Distributed.Fork` which provides a `fork` function which can run any `Closure (IO ())` using a `Backend` and fetch the result back. This is a very useful building block if you just need an equivalent of `forkIO` using multiple computers rather than multiple threads; so it is exposed as a separate module.

# Final Words

This is pretty much everything `distributed-dataset` can do for now. It does not come batteries-included, but the code is quite hackable (let me know if you are interested!). Don't build your production system on top of it (yet), but I believe it might already be useful to some.

If anyone else is keen to work on this; below is a non-exhaustive list of missing features:

1. Relational joins. `dJoin :: Dataset (k, a) -> Dataset (k, b) -> Dataset (These a b)`
2. A row-polymorphic API. Currently we manipulate ordinary Haskell data types; if we had more descriptive types for the datatypes and transformations specifying the columns they have and how they are modified, we could apply many more optimisations using that information.
3. An SQL interpreter. This will make it possible to have an interactive notebook-like experience that can be used without recompiling any Haskell code.
4. Helpers for common data formats and storage systems. Currently this requires figuring out which `Conduit`'s to stich together from various Hackage libraries; we should make this easier by providing utilities for common uses.
5. More `Backend`s; Kubernetes, YARN, Mesos, Google Cloud Run, static machines...

Project is hosted at [https://github.com/utdemir/distributed-dataset](). Feel free to open [issues][] for questions, feature requests or bug reports, and [send me an email][] if you want to discuss it further.

[distributed-dataset]: https://github.com/utdemir/distributed-dataset
[issues]: https://github.com/utdemir/distributed-dataset/issues
[Control.Distributed.Dataset.Aggr]: https://utdemir.github.io/distributed-dataset/distributed-dataset/Control-Distributed-Dataset-Aggr.html
[StaticPointers]: https://downloads.haskell.org/~ghc/8.8.2/docs/html/users_guide/glasgow_exts.html#static-pointers
[distributed-closure]: https://hackage.haskell.org/package/distributed-closure
[GH Archive]: https://www.gharchive.org/
[AWS Lambda]: https://aws.amazon.com/lambda/
[Apache Spark]: https://spark.apache.org/
[Backend]: https://utdemir.github.io/distributed-dataset/distributed-dataset/Control-Distributed-Fork-Backend.html#t:Backend
[send me an email]: mailto:me@utdemir.com
