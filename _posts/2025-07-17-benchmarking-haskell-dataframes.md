---
layout: post
title: Benchmarking Haskell dataframes against Python dataframes
---

I've been working on a dataframe implementation in Haskell for about a year now. While my focus has been on ergonomics the question of performance has inevitably come up. I haven't made significant performance investments but I thought it might be worth snapshotting the performance to establish a baseline.

## A benchmark for the impatient

I'll start by adopting a small benchmark from the [C++ dataframe implementation](https://github.com/hosseinmoein/DataFrame?tab=readme-ov-file#performance). The benchmark generates a dataframe with three columns containing random numbers, runs statistics some statistics on the column, and finally filters elements satisfying a predicate.

Because all these operations are effectively vector/array traversals this benchmark tests the underlying array implementation more than it tests dataframe specific operations (such as groupBy, pivot, melt, etc). But this is a good starting point since slow arrays mean slow everything. I used Haskell's Criterion library to generate the benchmark stats. The Python implementation has the slight overhead of spawning a separate process so there are some error bars in the performance numbers. The code can be found [here](https://github.com/mchav/dataframe/tree/main/benchmark).

I used three values of `n`: 100M, 150M and 300M. Polars and Pandas, on my machine, HP chromebook dragonfly g4 (32GB RAM) Polars and Pandas ran out of memory so there were only two data points.

| Size     | Haskell  | Polars  |  Pandas |
|----------|----------|---------|---------|
| O(100M)  |  6.343s  |  6.607s |  9.874s |
| O(150M)  |  9.063s  | 10.607s |  17.65s |
| O(300M)  | 15.260s  | N/A     | N/A     |

The performance is comparable to Polars and 2x faster than Pandas. But, again, these are simple scans. Let's compare `groupBy` operations since those are a little more complicated.

## Comparing groupBy
We'll use the [California housing dataset](https://github.com/mchav/dataframe/blob/main/data/housing.csv). At a high level the code will do the following: read a CSV file, group by `ocean_proximity` then compute the minimum and maximum `median_house_value`.

| Haskell  | Polars  |  Pandas |
|----------|---------|---------|
|  119ms   |  227ms  |  376ms  |

We see a similar trend as before. The Polars performance might actually be much closer to Haskell because of the process overhead. But, again, this is comparable.

I'll update these benchmarks as I add more complex operations. Also, as focus shifts more on performance I'd be curious to see how each performance knob changes these numbers.

Some interesting future projects for performance:
* Parallelism: this is all single-threaded.
* Vectorization/hardware optimiztion: exploit SIMD-supporting containers e.g. HaskTorch tensors, blas etc.
